SRC = src
OCAML_PATH = $(SRC)/ocaml

all: bin compiler

bin:
	mkdir bin

compiler:
	cd $(OCAML_PATH); \
	ocamlc -c ast.ml; \
	ocamlc -c transform.ml; \
	ocamlyacc parser.mly; \
	ocamlc -c parser.mli; \
	ocamllex scanner.mll; \
	ocamlc -c scanner.ml; \
	ocamlc -c parser.ml; \
	ocamlc -c semantic.ml; \
	ocamlc -c parallelizer.ml; \
	ocamlc -c generate.ml; \
	ocamlc -c compile.ml; \
	ocamlc -c dotpar.ml; \
	ocamlc -o ../../bin/dotpar scanner.cmo ast.cmo transform.cmo \
			parser.cmo semantic.cmo parallelizer.cmo generate.cmo compile.cmo \
			dotpar.cmo

clean_ocaml:
	rm -f bin/dotpar
	cd $(OCAML_PATH); \
	rm -f *.cmo scanner.ml parser.ml parser.mli *.cmi

test: parser_test semantic_test full_test

parser_test: compiler
	python tests/parser_test.py
semantic_test: compiler
	python tests/semantic_test.py
full_test: compiler
	scala tests/full_test.scala

clean: clean_ocaml

# http://linuxdevcenter.com/pub/a/linux/2002/01/31/make_intro.html?page=2
.PHONY: all compiler test clean
