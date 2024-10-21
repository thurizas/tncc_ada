GNAT=gnat
GNATFLAGS=-g -O0

PROG=tncc

OBJS = main.o lexer.o
ALIS = main.ali #lexer.ali



$(PROG) : $(OBJS)
	gnatbind -x $(ALIS)
	gnatlink -g -O0 $(ALIS) -o $(PROG)

main.o : ./main.adb
	gnatgcc -c -I./ -O0 ./main.adb -o main.o

lexer.o : ./lexer.adb ./lexer.ads
	gnatgcc -c -I./ -O0 ./lexer.adb -o lexer.o

all: clean $(PROG)

clean:
	rm -f *.*~ *.o *.ali
	rm -f $(PROG)
	rm -f b~main.*