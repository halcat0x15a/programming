import java.util.Optional;

public class ListMain {

    public static void main(String[] args) {
	List<Integer> foo = new Cons<>(0, new Cons<>(1, new Cons<>(2, new Nil<>())));
	assert foo.size() == 3;
	assert foo.get(1).equals(Optional.of(1));
	assert foo.get(2).equals(Optional.of(2));
	assert foo.get(3).equals(Optional.empty());
    }

}

interface List<T> {

    public Optional<T> get(int n);

    public int size();

}

class Cons<T> implements List<T> {

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

    public int size() {
	return 1 + tail.size();
    }

}

class Nil<T> implements List<T> {

    public Optional<T> get(int n) {
	return Optional.empty();
    }

    public int size() {
	return 0;
    }

}
