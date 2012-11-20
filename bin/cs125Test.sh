#!/bin/sh
# cs125 Builder
# builds and runs cs 125 MP Test Cases
filename=$(basename "$1")
classname="${filename%.*}"
echo $1

javac -classpath ".:junit.jar" $1
java -cp ".:junit.jar" junit.textui.TestRunner $classname

# no need to keep these around.
rm *.class
