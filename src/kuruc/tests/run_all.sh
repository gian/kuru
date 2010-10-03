#!/bin/bash

echo "Kuruc tests:"
for i in *.k
do
	printf "%28s: " "$i"
	../../../bin/kuruc $i > $i.out
	OUT=`md5sum $i.out`
	EXP=`grep "$i.out" expected.dat`
	if [ "$OUT" != "$EXP" ]; then
		echo "	[FAILED]"
	else echo "	[passed]"
	fi
done



