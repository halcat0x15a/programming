fib() {
    buf=(0 1)
    for i in `seq 2 $1`
    do
	n=`expr $i - 2`
	m=`expr $i - 1`
	buf+=(`expr ${buf[n]} + ${buf[m]}`)
    done
    for n in ${buf[@]}
    do
	echo $n
    done
}

fib 50
