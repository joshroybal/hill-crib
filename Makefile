all: hill2 hill3 hillattack2 hillattack3

hill2: hill2.o chlib.o modmatlib.o
	gfortran -static -s -o hill2 hill2.o chlib.o modmatlib.o

hill3: hill3.o chlib.o modmatlib.o
	gfortran -static -s -o hill3 hill3.o chlib.o modmatlib.o

hillattack2: ha2.o chlib.o modmatlib.o
	gfortran -static -s -o hillattack2 ha2.o chlib.o modmatlib.o

hillattack3: ha3.o chlib.o modmatlib.o
	gfortran -static -s -o hillattack3 ha3.o chlib.o modmatlib.o

hill2.o: hill2.f
	gfortran -O2 -c hill2.f

hill3.o: hill3.f
	gfortran -O2 -c hill3.f

ha2.o: ha2.f
	gfortran -O2 -c ha2.f

ha3.o: ha3.f
	gfortran -O2 -c ha3.f

chlib.o: chlib.f
	gfortran -O2 -c chlib.f

modmatlib.o: modmatlib.f
	gfortran -O2 -c modmatlib.f

clean:
	rm hill2
	rm hill3
	rm hillattack2
	rm hillattack3
	rm *.o
