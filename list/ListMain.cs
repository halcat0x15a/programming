using System.Diagnostics;

class ListMain
{

    static void Main()
    {
        var foo = new Cons<int>(0, new Cons<int>(1, new Cons<int>(2, new Nil<int>())));
        Debug.Assert(foo.Size() == 3);
        Debug.Assert(foo.Get(1) == 1);
        Debug.Assert(foo.Get(2) == 2);
        Debug.Assert(foo.Get(3) == 0);
    }

}

interface List<T>
{

    T Get(int n);

    int Size();

}

class Cons<T> : List<T>
{

    private T head;
    private List<T> tail;

    public Cons(T head, List<T> tail)
    {
        this.head = head;
        this.tail = tail;
    }

    public T Get(int n)
    {
        if (n == 0)
            return head;
        else
            return tail.Get(n - 1);
    }

    public int Size()
    {
        return 1 + tail.Size();
    }

}

class Nil<T> : List<T>
{

    public T Get(int n)
    {
        return default(T);
    }

    public int Size()
    {
        return 0;
    }

}
