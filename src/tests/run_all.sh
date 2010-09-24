#!/bin/bash

for i in *.k
do
	printf "%25s: " "$i"
	../../bin/kil -v 0 $i > $i.out
	OUT=`md5sum $i.out`
	EXP=`grep "$i.out" expected.dat`
	if [ "$OUT" != "$EXP" ]; then
		echo "	[FAILED]"
		exit 1
	else echo "	[passed]"
	fi
done
