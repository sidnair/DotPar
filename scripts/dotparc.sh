#!/bin/sh

# locations, uniq'd by pid
Compile_dir=/tmp/dotpar_compile$$
Scala_file=$Compile_dir/main.scala
Lib_file=lib/dotpar.scala
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
cd `dirname $1`
cat `basename $1` | $CurDir/bin/dotpar > $Scala_file
cd $CurDir
if [ $? -ne 0 ]
then
    echo "Dotpar -> Scala conversion failed"
    exit $?
fi

# do the compilation
scalac -d $Class_files $Lib_file $Scala_file
if [ $? -ne 0 ]
then
    echo "Scalac compilation failed"
    exit $?
fi

# unpack scala/, move it to $Class_files
cp -n $SCALA_PATH/scala-library.jar $Scala_lib
unzip -q -d $Scala_lib $Scala_lib/scala-library.jar
if [ $? -ne 0 ]
then
    echo "Unpacking scala-library.jar failed"
    exit $?
fi
cp -R $Scala_lib/scala $Class_files

# pack it into a jar with scala/
echo "Main-Class: Main" > $Manifest_file
cd $Class_files
jar -cfm $Compile_dir/$Gen_file $Manifest_file *
if [ $? -ne 0 ]
then
    echo "Packaging into a jar failed"
    exit $?
fi
cd $CurDir

# move the program.jar to the right place
mv $Compile_dir/$Gen_file $CurDir/$Gen_file

# clean up
rm -rf $Compile_dir

# undo the trap at the end of the universe
trap - 0 1 2 3 6

exit 0
