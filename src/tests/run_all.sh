#!/bin/bash

for i in *.k
do
	printf "%25s: " "$i"
	../../bin/kil -v 3 $i > $i.out
	OUT=`md5sum $i.out`
	EXP=`grep "$i.out" expected.dat`
	if [ "$OUT" != "$EXP" ]; then
		echo "	[FAILED]"
	else echo "	[passed]"
	fi
done
