#!/bin/bash

export KURU_LIBRARY_PATH=../../runtime:../../lib:../../../lib

echo "Kuruc tests:"
for i in *.k
do
	printf "%35s: " "$i"
	../../../bin/kuruc -qq $i -o $i.out 2> /dev/null
#	../../../bin/kuruc $i -o $i.out -d
	if [ $? -eq 0 ] ; then
		OUT=`md5sum $i.out`
		EXP=`grep "$i.out" expected.dat`
		if [ "$OUT" != "$EXP" ]; then
			echo "	[FAILED (Compiled Output Mismatch)]"
		else echo "	[passed]"
		fi
	else
		echo "	[FAILED (Compilation Failed)]"
	fi
done



