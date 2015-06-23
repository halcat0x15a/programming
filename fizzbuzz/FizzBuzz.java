import java.util.stream.IntStream;

public class FizzBuzz {

    public static String fizzbuzz(int n) {
	if (n % 15 == 0)
	    return "FizzBuzz";
	else if (n % 3 == 0)
	    return "Fizz";
	else if (n % 5 == 0)
	    return "Buzz";
	else
	    return String.valueOf(n);
    }

    public static void main(String[] args) {
	IntStream.rangeClosed(1, 100).forEach(i -> System.out.println(fizzbuzz(i)));
    }

}
