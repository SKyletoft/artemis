generate:
	bnfc -m --c --outputdir=parser src/parser.cf

build:
	clang -O3 -march=native -w \
	parser/Absyn.c \
	parser/Absyn.h \
	parser/Buffer.c \
	parser/Buffer.h \
	parser/Parser.h \
	parser/Printer.c \
	parser/Printer.h \
	parser/Skeleton.c \
	parser/Skeleton.h \
	parser/Test.c
