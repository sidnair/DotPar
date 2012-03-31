all:
	lex lexer.l && yacc -d --verbose dotpar.y && gcc lex.yy.c y.tab.c -ly

test:
	python parser_test.py

clean:
	rm -f a.out *.c *.h *.output
