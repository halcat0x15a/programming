sub fib {
    my @buf = (0, 1);
    foreach my $n (2..100) {
	$buf[$n] = $buf[$n - 1] + $buf[$n - 2];
    }
    return @buf;
}

foreach my $n (fib(100)) {
    print $n, "\n";
}
