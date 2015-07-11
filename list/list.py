class List:

    def __getitem__(self, n):
        return reduce(lambda (result, n), value: (result or value, n - 1) if n <= 0 else (result, n - 1), self, (None, n))[0]

    def __len__(self):
        return reduce(lambda n, _: n + 1, self, 0)

    def reverse(self):
        return reduce(lambda tail, head: Cons(head, tail), self, Nil())

    @staticmethod
    def create(*values):
        return reduce(lambda acc, head: lambda k: acc(lambda tail: Cons(head, k(tail))), values, lambda k: k(Nil()))(lambda x: x)

class Nil(List):

    def __add__(self, other):
        return other

    def __eq__(self, other):
        return isinstance(other, Nil)

    def __iter__(self):
        raise StopIteration()

class Cons(List):

    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    def __add__(self, other):
        return Cons(self.head, self.tail + other)

    def __eq__(self, other):
        return isinstance(other, Cons) and self.head == other.head and self.tail == other.tail

    def __iter__(self):
        yield self.head
        for value in self.tail:
            yield value

if __name__ == '__main__':
    foo = List.create(0, 1, 2)
    assert len(foo) == 3
    assert foo[1] == 1
    assert foo[2] == 2
    assert foo[3] == None
    assert foo + foo == List.create(0, 1, 2, 0, 1, 2)
    assert foo.reverse() == List.create(2, 1, 0)
