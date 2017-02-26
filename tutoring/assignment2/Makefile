CFLAGS = -g -Wall

dynamic_array_main: dynamic_int_array.o dynamic_array_main.cpp 
	g++ $(CFLAGS) -o dynamic_array_main dynamic_int_array.o dynamic_array_main.cpp

dynamic_int_array.o: dynamic_int_array.cpp dynamic_int_array.h
	g++ -g -Wall -c dynamic_int_array.cpp
