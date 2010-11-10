#!/bin/bash

echo "Kuruc tests:"
for i in *.k
do
	printf "%35s: " "$i"
	../../../bin/kuruc -qq -L ../../runtime:../../lib:../../../lib ../../lib/kuru.kb $i -o $i.out 2> /dev/null
	if [ $? -eq 0 ] ; then
		OUT=`md5sum $i.out`
		EXP=`grep "$i.out" expected.dat`
		if [ "$OUT" != "$EXP" ]; then
			echo "	[FAILED]"
		else echo "	[passed]"
		fi
	else
		echo "	[FAILED]"
	fi
done



