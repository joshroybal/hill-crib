all: hill2 hill3 hill4 ha2 ha3 ha4

hill2: hill2.o cryptlib.o modmatlib.o chlib.o
	gfortran -static -s -o hill2 hill2.o cryptlib.o modmatlib.o chlib.o

hill3: hill3.o cryptlib.o modmatlib.o chlib.o
	gfortran -static -s -o hill3 hill3.o cryptlib.o modmatlib.o chlib.o

hill4: hill4.o cryptlib.o modmatlib.o chlib.o
	gfortran -static -s -o hill4 hill4.o cryptlib.o modmatlib.o chlib.o

ha2: ha2.o attacklib.o modmatlib.o chlib.o
	gfortran -static -s -o ha2 ha2.o attacklib.o modmatlib.o chlib.o

ha3: ha3.o attacklib.o modmatlib.o chlib.o
	gfortran -static -s -o ha3 ha3.o attacklib.o modmatlib.o chlib.o

ha4: ha4.o attacklib.o modmatlib.o chlib.o
	gfortran -static -s -o ha4 ha4.o attacklib.o modmatlib.o chlib.o

cryptlib.o: cryptlib.f
	gfortran -O2 -c cryptlib.f
	
attacklib.o: attacklib.f
	gfortran -O2 -c attacklib.f

chlib.o: chlib.f
	gfortran -O2 -c chlib.f

modmatlib.o: modmatlib.f
	gfortran -O2 -c modmatlib.f

hill2.o: hill2.f
	gfortran -O2 -c hill2.f

hill3.o: hill3.f
	gfortran -O2 -c hill3.f

hill4.o: hill4.f
	gfortran -O2 -c hill4.f

ha2.o: ha2.f
	gfortran -O2 -c ha2.f

ha3.o: ha3.f
	gfortran -O2 -c ha3.f

ha4.o: ha4.f
	gfortran -O2 -c ha4.f

clean:
	rm hill2
	rm hill3
	rm hill4
	rm ha2
	rm ha3
	rm ha4
	rm *.o
