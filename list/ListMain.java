import java.util.Optional;
import java.util.function.BiFunction;

public class ListMain {

    public static void main(String[] args) {
	List<Integer> foo = List.create(0, 1, 2);
	assert foo.size() == 3;
	assert foo.get(1).equals(Optional.of(1));
	assert foo.get(2).equals(Optional.of(2));
	assert foo.get(3).equals(Optional.empty());
	assert foo.append(foo).equals(List.create(0, 1, 2, 0, 1, 2));
	assert foo.reverse().equals(List.create(2, 1, 0));
    }

}

interface List<T> {

    public List<T> append(List<T> list);

    public Optional<T> get(int n);
    
    public <R> R foldLeft(R value, BiFunction<R, T, R> f);

    public default int size() {
	return this.<Integer>foldLeft(0, (n, value) -> n + 1);
    }

    public default List<T> reverse() {
	return foldLeft((List<T>) new Nil<T>(), (tail, head) -> new Cons<T>(head, tail));
    }

    @SafeVarargs
    public static <T> List<T> create(T... values) {
	List<T> list = new Nil<T>();
	for (int i = values.length - 1; i >= 0; i--)
	    list = new Cons<T>(values[i], list);
	return list;
    }

}

final class Cons<T> implements List<T> {

    private T head;
    private List<T> tail;

    public Cons(T head, List<T> tail) {
	this.head = head;
	this.tail = tail;
    }

    public Optional<T> get(int n) {
	if (n == 0)
	    return Optional.of(head);
	else
	    return tail.get(n - 1);
    }

    public <R> R foldLeft(R value, BiFunction<R, T, R> f) {
	return tail.foldLeft(f.apply(value, head), f);
    }

    public List<T> append(List<T> list) {
	return new Cons<T>(head, tail.append(list));
    }

    public boolean equals(Object other) {
	return other instanceof Cons && ((Cons) other).head.equals(head) && ((Cons) other).tail.equals(tail);
    }

}

final class Nil<T> implements List<T> {

    public Optional<T> get(int n) {
	return Optional.empty();
    }

    public List<T> append(List<T> list) {
	return list;
    }

    public boolean equals(Object other) {
	return other instanceof Nil;
    }

    public <R> R foldLeft(R value, BiFunction<R, T, R> f) {
	return value;
    }

}
