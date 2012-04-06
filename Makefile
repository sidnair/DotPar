SRC = src
OCAML_PATH = $(SRC)/ocaml
C_PATH = $(SRC)/c

all: bin compiler

bin:
	mkdir bin

compiler:
	cd $(OCAML_PATH); \
	ocamlyacc parser.mly; \
	ocamlc -c parser.mli; \
	ocamllex scanner.mll; \
	ocamlc -c scanner.ml; \
	ocamlc -c parser.ml; \
	ocamlc -c dotpar.ml; \
	ocamlc -o ../../bin/dotpar scanner.cmo parser.cmo dotpar.cmo

clean_ocaml:
	rm -f bin/dotpar
	cd $(OCAML_PATH); \
	rm -f *.cmo scanner.ml parser.ml parser.mli *.cmi

parser_test: compiler
	python tests/parser_test.py

c_compiler:
	cd $(C_PATH); \
	lex lexer.l; \
	yacc -d --verbose dotpar.y; \
	gcc lex.yy.c y.tab.c -ly
	mv a.out ../../bin/dotpar

clean_c:
	cd $(C_PATH); \
	rm -f a.out *.c *.h *.output;

clean: clean_ocaml clean_c

# http://linuxdevcenter.com/pub/a/linux/2002/01/31/make_intro.html?page=2
.PHONY: all compiler c_compiler test clean
