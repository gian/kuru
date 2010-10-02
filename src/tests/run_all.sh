#!/bin/bash

echo "KIL tests:"
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

echo "KuPEG tests:"
for i in *.kpg
do
	printf "%25s: " "$i"
	../../bin/kupeg $i > /dev/null
   mv "$i.k" "$i.k.out"
   OUT=`md5sum $i.k.out`
	EXP=`grep "$i.k.out" expected.dat`
	if [ "$OUT" != "$EXP" ]; then
		echo "	[FAILED]"
	else echo "	[passed]"
	fi
done

echo "Kuruc tests:"
for i in *.k
do
	printf "%25s: " "$i"
	../../bin/kuruc $i > $i.kuruout
	OUT=`md5sum $i.kuruout`
	EXP=`grep "$i.kuruout" expected.dat`
	if [ "$OUT" != "$EXP" ]; then
		echo "	[FAILED]"
	else echo "	[passed]"
	fi
done



