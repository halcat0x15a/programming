using System.Linq;

class FizzBuzzMain
{

    static string FizzBuzz(int n)
    {
        if (n % 15 == 0)
	    return "FizzBuzz";
	else if (n % 3 == 0)
	    return "Fizz";
	else if (n % 5 == 0)
	    return "Buzz";
	else
	    return n.ToString();
    }

    static void Main()
    {
        foreach (var i in Enumerable.Range(1, 100))
	    System.Console.WriteLine(FizzBuzz(i));
    }

}
