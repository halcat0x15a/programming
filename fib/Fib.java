import java.math.BigInteger;
import java.util.Arrays;
import java.util.ArrayList;

public class Fib {

    public static ArrayList<BigInteger> fib(int n) {
	ArrayList<BigInteger> buf = new ArrayList<>(Arrays.asList(BigInteger.ZERO, BigInteger.ONE));
	for (int i = 2; i < n; i++)
	    buf.add(buf.get(i - 1).add(buf.get(i - 2)));
	return buf;
    }
    
    public static void main(String[] args) {
	for (BigInteger n: fib(100))
	    System.out.println(n);
    }
    
}
