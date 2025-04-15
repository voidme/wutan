
class Context:
    def __init__(self, data = {}, parent=None):
        self.parent = parent if isinstance(parent, Context) else None
        self.data = data

    def get(self, key, default=None, source=None):
        value = self.data.get(key, default)
        
        if value is None and self.parent:
            value = self.parent.get(key, default, self)

        if hasattr(value, '__call__'):
            value = value(self)
        
        return value

    def set(self, key, value):
        self.data[key] = value

    def isExtended(self, context):
        return self == context or self.parent != None and self.parent.isExtended(context)

    def extend(self, context):
        if not self.isExtended(context):
            exteneded = self.parent
            self.parent = context
            context.parent = exteneded

    def handles(self, key, method):
        self.data[key] = method

def test():
    context1 = Context()
    context2 = Context(parent=context1)
    context3 = Context(parent=context2)

    context1.set('a', 1)
    context2.set('b', 2)
    context3.set('c', 3)

    context1.handles("test", lambda context: context.get('a') + 1)

    assert context3.get('a') == 1
    assert context3.get('b') == 2
    assert context3.get('c') == 3
    assert context3.get('d', default=4) == 4
    assert context1.get("test") == 2

    print("All tests passed!")

test()