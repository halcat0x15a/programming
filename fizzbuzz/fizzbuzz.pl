sub fizzbuzz {
    my ($n) = @_;
    if ($n % 15 == 0) {
	return "FizzBuzz";
    } elsif ($n % 3 == 0) {
	return "Fizz";
    } elsif ($n % 5 == 0) {
	return "Buzz";
    } else {
	return $n;
    }
}

foreach my $i (1..100) {
    print fizzbuzz($i), "\n";
}
