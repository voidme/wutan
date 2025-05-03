from typing import Any, Callable, Dict, Optional

class Context:
    def __init__(self, data: Optional[Dict[str, Any]] = None, parent: Optional['Context'] = None):
        self.parent: Optional['Context'] = parent
        self.data: Dict[str, Any] = data if data is not None else {}

    def get(self, key: str, default: Any = None, source: Optional['Context'] = None) -> Any:
        if not source:
            source = self

        value = self.data.get(key)

        if callable(value):
            value = value(source)
        elif value is None and self.parent:
            value = self.parent.get(key, default, self)

        return value if value is not None else default

    def set(self, key: str, value: Any) -> None:
        self.data[key] = value

    def isExtended(self, context: 'Context') -> bool:
        return self == context or (self.parent is not None and self.parent.isExtended(context))

    def extend(self, context: 'Context') -> None:
        if not self.isExtended(context):
            extended = self.parent
            self.parent = context
            context.parent = extended

    def handles(self, key: str, method: Callable[['Context'], Any]) -> None:
        self.data[key] = method


def test() -> None:
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
