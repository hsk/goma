all:
	cd src; omake

hello:
	./gomac example/hello.goma example/hello.cpp
	g++ example/hello.cpp -o hello
	./hello

fib:
	./gomac example/fib.goma example/fib.cpp
	g++ example/fib.cpp -o fib
	./fib

calc: example/calc.goma
	./gomac example/calc.goma example/calc.cpp
	g++ example/calc.cpp -o calc
	./calc

clean:
	cd src; omake clean
	rm -rf gomac gomac.opt hello calc fib example/*.cpp src/.omakedb




