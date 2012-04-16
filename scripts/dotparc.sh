#!/bin/sh

# locations, uniq'd by pid
Compile_dir=/tmp/dotpar_compile$$
Scala_file=$Compile_dir/main.scala
Manifest_file=$Compile_dir/manifest.mf
Class_files=$Compile_dir/classes/
Program_lib=$Compile_dir/program_lib
Scala_lib=$Compile_dir/scala_lib
Gen_file=program.jar

# handle a premature user-initialized exit
trap "rm -rf $Compile_dir; exit 1" 0 1 2 3 6

# for moving about
CurDir=`pwd`

# add this as an option
# path to scala-library.jar
SCALA_PATH=/usr/share/java/

# clean up any previously generated file
rm -f $Gen_file

# prep the compilation area
mkdir -p $Compile_dir
mkdir -p $Class_files
mkdir -p $Program_lib
mkdir -p $Scala_lib

# we assume this file is executed with ./dotparc.sh
cat $1 | bin/dotpar > $Scala_file

# do the compilation
scalac -d $Class_files $Scala_file

# pack it into a jar with the scala jar
echo "Main-Class: Main" > $Manifest_file
cd $Class_files
jar -cfm $Compile_dir/$Gen_file $Manifest_file *
cd $CurDir

# unpack program.jar, repack it with scala/
cp -n $SCALA_PATH/scala-library.jar $Scala_lib
unzip -q -d $Scala_lib $Scala_lib/scala-library.jar
unzip -q -d $Program_lib $Compile_dir/$Gen_file
cp -R $Scala_lib/scala $Program_lib
cd $Program_lib
zip -q -r $Program_lib/$Gen_file *
cd $CurDir

# move the program.jar to the right place
mv $Program_lib/$Gen_file $CurDir/$Gen_file

# clean up
rm -rf $Compile_dir
