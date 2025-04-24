
from typing import Generator, Tuple, Optional
from context import Context

class Transformer:
    @staticmethod
    def parse(content: str) -> Generator[Tuple[str, Optional[str]], None, None]:
        """
        Parse template content and extract variable placeholders.
        
        Args:
            content: Template string containing {{variable}} placeholders
            
        Yields:
            Tuples of (text_segment, variable_name) where variable_name is None for plain text
        """
        pos = 0
        while pos < len(content):
            start_pos = content.find("{{", pos)
            
            if start_pos < 0:
                break
                
            if start_pos > pos:
                yield content[pos:start_pos], None
                
            pos = start_pos
            end_pos = content.find("}}", start_pos + 2)
            
            if end_pos > start_pos:
                yield content[start_pos:end_pos + 2], content[start_pos + 2:end_pos]
                pos = end_pos + 2
            else:
                yield content[start_pos+2:], None
                pos += 2
                
        if pos < len(content):
            yield content[pos:], None

    @staticmethod
    def join(context: Context, items: Generator[Tuple[str, Optional[str]], None, None]) -> Generator[str, None, None]:
        """
        Replace variables in parsed template with values from context.
        
        Args:
            context: Context object containing variable values
            items: Generator of parsed template segments
            
        Yields:
            Resolved string segments
        """
        for item in items:
            if item[1] is not None:
                yield context.get(item[1], item[0])
            else:
                yield item[0]

    @staticmethod
    def transform(context: Context, content: str) -> str:
        """
        Transform a template string by replacing variables with values from context.
        
        Args:
            context: Context object containing variable values
            content: Template string containing {{variable}} placeholders
            
        Returns:
            The fully resolved template string
            
        Raises:
            TypeError: If context is not an instance of Context
        """
        if not isinstance(context, Context):
            raise TypeError("context must be an instance of Context")
        
        items = Transformer.parse(content)
        result = [str(x) for x in Transformer.join(context, items)]
        return "".join(result)