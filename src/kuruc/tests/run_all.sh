#!/bin/bash

export KURU_LIBRARY_PATH=../../runtime:../../lib:../../../lib

f=0
t=0

echo "Kuruc tests:"
for i in *.k
do
	t=$(($t+1))
	printf "%35s: " "$i"
	../../../bin/kuruc -q $i -o $i.out 2> $i.log
#	../../../bin/kuruc $i -o $i.out -d
	if [ $? -eq 0 ] ; then
		OUT=`md5sum $i.out`
		EXP=`grep "$i.out" expected.dat`
		if [ "$OUT" != "$EXP" ]; then
			echo "	[FAILED (Compiled Output Mismatch)]"
			f=$(($f+1))
		else echo "	[passed]"
		fi
	else if `echo $i | grep "fail" 1>/dev/null 2>&1` ; then
		echo "	[passed]"
	else
		f=$(($f+1))
		echo "	[FAILED (Compilation Failed)]"
	fi fi
done

echo ""
echo " * Summary: $(($t-$f))/$t tests passed.  $f failures."



