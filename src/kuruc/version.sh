#!/bin/bash

MAJOR="0"
MINOR="1"
BUILD=`git describe`
DATE=`date +%Y%m%d`
HOST=`hostname`

echo "(* This file is generated by version.sh *)"
echo "structure Version ="
echo "struct"
echo "   val major = \"${MAJOR}\""
echo "   val minor = \"${MINOR}\""
echo "   val build = \"${BUILD}\""
echo "   val date = \"${DATE}\""
echo "   val hostname = \"${HOST}\""
echo "end"

