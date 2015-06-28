using System.Numerics;
using System.Linq;
using System.Collections.Generic;

class Fib
{

    static List<BigInteger> fib(int n)
    {
	var buf = new List<BigInteger> {0, 1};
	foreach (var i in Enumerable.Range(2, n - 2))
	    buf.Add(buf[i - 1] + buf[i - 2]);
	return buf;
    }

    static void Main()
    {
        foreach (var n in fib(100))
	    System.Console.WriteLine(n);
    }

}