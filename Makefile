all: compiler

compiler:
	cd ocaml_front_end; make; cd ..

c_compiler:
	cd c_front_end; make; cd ..

test: compiler
	python tests/parser_test.py

clean:
	cd ocaml_front_end; make clean; cd ..
	cd c_front_end; make clean; cd ..

# http://linuxdevcenter.com/pub/a/linux/2002/01/31/make_intro.html?page=2
.PHONY: all compiler c_compiler test clean
