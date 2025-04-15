from context import Context

class Transformer:
    def parse(content):
        pos = 0
        while pos < len(content):
            start_pos = content.find("{{", pos)
            if start_pos < 0:
                break
            if start_pos >= pos:
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
        yield content[pos:], None

    def join(context, items):
        for item in items:
            if item[1] is not None:
                yield context.get(item[1], item[0])
            else:
                yield item[0]

    def transform(context, content):

        if not isinstance(context, Context):
            raise TypeError("context must be an instance of Context")
        
        items = Transformer.parse(content)
        result = [str(x) for x in Transformer.join(context, items)]

        return "".join(result)
    
def test():
    from context import Context

    context = Context()
    context.set("name", "John")
    context.set("age", 30)

    content = "Hello {{name}}, you are {{age}} years old."
    result = Transformer.transform(context, content)
    print(result)  # Output: Hello John, you are 30 years old.
    context.extend(Context({
        "UpperName": lambda c: c.get("name").upper(),
        "NewAge": lambda c: c.get("age") + 1
    }))
    result = Transformer.transform(context, "Hello {{UpperName}}! You are {{NewAge}} years old.")
    print(result)  # Output: Hello JOHN, you are 30 years old.
    

test()
