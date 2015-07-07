use Class::Struct;

struct Cons => {
    head => '$',
    tail => 'Cons'
};

sub cons {
    my ($head, $tail) = @_;
    my $cons = new Cons;
    $cons->head($head);
    if ($tail) {
	$cons->tail($tail);
    }
    return $cons;
}

sub size {
    my ($list) = @_;
    if ($list) {
	return 1 + size($list->tail());
    } else {
	return 0;
    }
}

sub get {
    my ($n, $list) = @_;
    if (!$list) {
	return $list;
    } elsif ($n == 0) {
	return $list->head();
    } else {
	return get($n - 1, $list->tail());
    }
}

my $foo = cons(0, cons(1, cons(2)));
print size($foo) == 3, "\n";
print get(1, $foo) == 1, "\n";
print get(2, $foo) == 2, "\n";
print get(3, $foo) == (), "\n";
