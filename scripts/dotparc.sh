#!/bin/sh

# locations, uniq'd by pid
Compile_dir=/tmp/dotpar_compile$$
Scala_file=$Compile_dir/main.scala
Lib_file=lib/dotpar.scala
Manifest_file=$Compile_dir/manifest.mf
Class_files=$Compile_dir/classes/
Program_lib=$Compile_dir/program_lib
Scala_lib=$Compile_dir/scala_lib

# for moving about
CurDir=`pwd`

# handle a premature user-initialized exit
trap "rm -rf $Compile_dir; cd $CurDir; exit 1" 0 1 2 3 6

# add this as an option
# path to scala-library.jar
SCALA_PATH=/usr/share/java/

# prep the compilation area
mkdir -p $Compile_dir
mkdir -p $Class_files
mkdir -p $Program_lib
mkdir -p $Scala_lib

# unpack scala/, move it to $Class_files
cp -n $SCALA_PATH/scala-library.jar $Scala_lib
unzip -q -d $Scala_lib $Scala_lib/scala-library.jar
if [ $? -ne 0 ]
then
    echo "Unpacking scala-library.jar failed"
    exit $?
fi
cp -R $Scala_lib/scala $Class_files

# now, for each file given, compile it
for file in $@
do
    # prep the compilation area
    mkdir -p $Compile_dir
    mkdir -p $Class_files
    mkdir -p $Program_lib
    mkdir -p $Scala_lib

    filename=`basename $1`
    noextension=${filename%.*}
    Gen_file=$noextension.jar
    dir_file=`dirname $1`
    # clean up any previously generated file
    rm -f $Gen_file

    # we assume this file is executed with scripts/dotparc.sh
    cd $dir_file
    $CurDir/bin/dotpar <$filename >$Scala_file
    if [ $? -ne 0 ]
    then
        echo "Dotpar -> Scala conversion failed"
        exit $?
    fi
    cd $CurDir

    # do the compilation
    scalac -d $Class_files $Lib_file $Scala_file
    if [ $? -ne 0 ]
    then
        echo "Scalac compilation failed"
        exit $?
    fi

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

    # move the .jar to the right place
    mv $Compile_dir/$Gen_file $CurDir/$Gen_file

    rm -rf $Compile_dir
done

# undo the trap at the end of the universe
trap - 0 1 2 3 6

exit 0
