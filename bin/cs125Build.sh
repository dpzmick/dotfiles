#!/bin/sh
# cs125 Builder
# builds and runs cs 125 MP's
filename=$(basename "$1")
classname="${filename%.*}"

echo $1
javac -classpath ".:junit.jar" $1
java -classpath ".:junit.jar" $classname

# no reason to keep class files around for these
rm *.class
