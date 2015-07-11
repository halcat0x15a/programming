class ListMain
{

    static void Main()
    {
        var foo = List<int>.Create(0, 1, 2);
        System.Console.WriteLine(foo.Size() == 3);
        System.Console.WriteLine(foo.Get(1) == 1);
        System.Console.WriteLine(foo.Get(2) == 2);
        System.Console.WriteLine(foo.Get(3) == 0);
        System.Console.WriteLine(foo.Append(foo).Equals(List<int>.Create(0, 1, 2, 0, 1, 2)));
        System.Console.WriteLine(foo.Reverse().Equals(List<int>.Create(2, 1, 0)));
    }

}

abstract class List<T>
{

    public abstract T Get(int n);

    public abstract List<T> Append(List<T> list);

    public abstract R FoldLeft<R>(R r, System.Func<R, T, R> f);

    public int Size()
    {
	return FoldLeft(0, (n, value) => n + 1);
    }

    public List<T> Reverse()
    {
	return FoldLeft((List<T>) new Nil<T>(), (tail, head) => new Cons<T>(head, tail));
    }

    public static List<T> Create(params T[] values)
    {
	List<T> list = new Nil<T>();
	for (int i = values.Length - 1; i >= 0; i--)
	    list = new Cons<T>(values[i], list);
	return list;
    }

}

class Cons<T> : List<T>
{

    public T Head { get; }
    public List<T> Tail { get; }

    public Cons(T head, List<T> tail)
    {
        Head = head;
        Tail = tail;
    }

    public override T Get(int n)
    {
        if (n == 0)
            return Head;
        else
            return Tail.Get(n - 1);
    }

    public override List<T> Append(List<T> list)
    {
	return new Cons<T>(Head, Tail.Append(list));
    }

    public override R FoldLeft<R>(R r, System.Func<R, T, R> f)
    {
	return Tail.FoldLeft(f(r, Head), f);
    }    

    public override bool Equals(System.Object obj)
    {
	Cons<T> cons = obj as Cons<T>;
	return cons != null && cons.Head.Equals(Head) && cons.Tail.Equals(Tail);
    }

    public override int GetHashCode()
    {
	return Head.GetHashCode() ^ Tail.GetHashCode();
    }

}

class Nil<T> : List<T>
{

    public override T Get(int n)
    {
        return default(T);
    }

    public override List<T> Append(List<T> list)
    {
	return list;
    }

    public override R FoldLeft<R>(R r, System.Func<R, T, R> f)
    {
	return r;
    }

    public override bool Equals(System.Object obj)
    {
	return obj is Nil<T>;
    }

    public override int GetHashCode()
    {
	return 0;
    }

}
