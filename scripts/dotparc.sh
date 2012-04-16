#!/bin/sh

# locations, uniq'd by pid
Compile_dir=/tmp/dotpar_compile$$
Scala_file=$Compile_dir/main.scala
Manifest_file=$Compile_dir/manifest.mf
Class_files=$Compile_dir/classes/
Gen_file=program.jar

CurDir=`pwd`

SCALA_PATH=/usr/share/java/

# clean up any previously generated file
rm -f $Gen_file

# prep the compilation area
mkdir -p $Compile_dir
mkdir -p $Class_files

# we assume this file is executed with ./dotparc.sh
cat $1 | bin/dotpar > $Scala_file

# do the compilation
scalac -d $Class_files $Scala_file

# pack it into a jar with the scala jar
echo "Main-Class: Main\nClass-path: scala-library.jar" > $Manifest_file
cd $Class_files
jar -cfm $CurDir/$Gen_file $Manifest_file *
cd $CurDir

# add the needed scala-library.jar
# it is possible to unpack and repack with scala/
cp -n $SCALA_PATH/scala-library.jar $CurDir/

# clean up
rm -rf $Compile_dir
