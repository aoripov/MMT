#!/bin/bash

BASE_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


basedir=$BASE_PATH/../
destdir=$BASE_PATH/release/

mkdir -p $destdir

# JS Builting config
build="$destdir"JOBAD.js
sourcedirjs="$basedir"js

# CSS Building config
buildc="$destdir"JOBAD.css
sourcedirc="$basedir"css

echo "JOBAD build script "

echo "Checking build requirements ..."

printf "Python2 ... "

if which python2 >/dev/null; then
	echo "OK"
else
	echo "FAIL"
	echo "Abort: Python2 not found. "
	echo "You might want to apt-get install python"
	exit 1
fi

printf "Compiling development version ... "


cat $BASE_PATH/config/dev_header.js | sed -e "s/\${BUILD_TIME}/$(date)/" > $build

while read filename
do
	echo "/* start <$filename> */" >> $build
	cat $sourcedirjs/$filename >> $build
	echo "/* end   <$filename> */" >> $build
done < "$BASE_PATH/config/js.txt"

cat $BASE_PATH/config/dev_footer.js | sed -e "s/\${BUILD_TIME}/$(date)/" >> $build

echo "OK"


printf "Preparing compilation with Closure Compiler ... "

echo "" > $buildmin.tmp
cat $build >> $buildmin.tmp

echo "OK"

printf "Compiling minimized version ... "

cat $BASE_PATH/config/min_header.js | sed -e "s/\${BUILD_TIME}/$(date)/" > $buildmin

python2 $BASE_PATH/deps/closurecompilerpy/closureCompiler.py -s $buildmin.tmp >> $buildmin

cat $BASE_PATH/config/min_footer.js | sed -e "s/\${BUILD_TIME}/$(date)/" >> $buildmin
