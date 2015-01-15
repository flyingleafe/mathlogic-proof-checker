#!/bin/bash
function run {
    echo "processing file: $2.in -> $2.out... "
    time ./dist/build/$1/$1 inputs/$1/$2.in outputs/$1/$2.out
}

if [ $# -eq 2 ]
then
    run $1 $2
elif [ $# -eq 1 ]
then
    inputs=$(find inputs/$1/*.in -exec basename {} .in \;)
    for i in $inputs
    do
        run $1 $i
    done
else 
    echo "Wrong number of arguments"
fi
