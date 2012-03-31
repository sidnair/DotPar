all:
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamllex scanner.mll
	ocamlc -c scanner.ml
	ocamlc -c parser.ml
	ocamlc -c dotpar.ml
	ocamlc -o dotpar scanner.cmo parser.cmo dotpar.cmo

test:
	python parser_test.py

clean:
	rm -f dotpar *.cmo parser.ml scanner.ml parser.mli
