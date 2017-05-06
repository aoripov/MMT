#!/bin/bash

function npm-do { (PATH=$(npm bin):$PATH; eval $@;) }

BASE_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

basedir=$BASE_PATH/../
destdir=$BASE_PATH/release/

mkdir -p $destdir

# CSS Building config
buildc="$destdir"JOBAD.css
sourcedirc="$basedir"css

printf "Compiling CSS Development Version ..."
cat $BASE_PATH/config/dev_header.css | sed -e "s/\${BUILD_TIME}/$(date)/" > $buildc
while read filename
do
	echo "/* start <$filename> */" >> $buildc
	npm-do lessc $sourcedirc/$filename >> $buildc
	echo "/* end   <$filename> */" >> $buildc
done < "$BASE_PATH/config/css.txt"
cat $BASE_PATH/config/dev_footer.css | sed -e "s/\${BUILD_TIME}/$(date)/" >> $buildc

echo "OK"
