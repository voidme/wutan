import os
import sys

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
sys.path.append(parent_dir)

from context import Context
from transformer import Transformer

def test():

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

if __name__ == "__main__":
    test()