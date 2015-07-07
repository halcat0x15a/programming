class Nil:

    def __len__(self):
        return 0

    def __getitem__(self, n):
        return None

class Cons:

    def __init__(self, head, tail):
        self.head = head
        self.tail = tail

    def __len__(self):
        return 1 + len(self.tail)

    def __getitem__(self, n):
        if n == 0:
            return self.head
        else:
            return self.tail[n - 1]

def create(*values):
    return reduce(lambda acc, head: lambda k: acc(lambda tail: Cons(head, k(tail))), values, lambda k: k(Nil()))(lambda x: x)

if __name__ == '__main__':
    foo = create(0, 1, 2)
    assert len(foo) == 3
    assert foo[1] == 1
    assert foo[2] == 2
    assert foo[3] == None
