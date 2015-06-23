fizzbuzz() {
    if [ `expr $1 % 15` -eq 0 ]
    then
	echo "FizzBuzz"
    elif [ `expr $1 % 3` -eq 0 ]
    then
	echo "Fizz"
    elif [ `expr $1 % 5` -eq 0 ]
    then
	echo "Buzz"
    else
 	echo $1
    fi
}

for i in `seq 1 100`
do
    fizzbuzz $i
done
