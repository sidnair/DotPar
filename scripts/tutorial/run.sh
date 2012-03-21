#! /bin/zsh

ruby extract_fns.rb $1 > /tmp/fns.par
cat /tmp/fns.par | ../../front_end/a.out

if [ $? -eq 0 ]; then; echo "\n\n---\nGOOD SYNTAX"; else; echo "\n\n---\nSYNTAX ERROR"; fi
