#include <iostream>
#include <string>
#include "dynamic_int_array.h"

using namespace std;
using namespace cs20a;

void display(DynamicIntArray &d, string title = "");

int main()
{
        DynamicIntArray d1;

	cout << "Dynamic Array Test Suite:" << endl << endl;

	for (int i = 0; i < 10; i++) d1.add(i*i);
	display(d1, "Starting array:");

	d1.insert(2, 999999);
	display(d1, "insert(2, 999999);");

	d1.remove(2);
	display(d1, "d1.remove(2);");

	d1[3] = 888888;
	display(d1, "d1[3] = 888888;");

	DynamicIntArray d2 = d1;
	display(d1, "DynamicIntArray d2 = d1;");

	if (d1 == d2)
		cout << "d1 and d2 are equal." << endl << endl;
	else
		cout << "d1 and d2 are not equal." << endl << endl;

	d2[3] = 777777;
	display(d2, "d2[3] = 777777;");

	if (d1 == d2)
		cout << "d1 and d2 are equal." << endl << endl;
	else
		cout << "d1 and d2 are not equal." << endl << endl;

	DynamicIntArray d3;

	d3 = d1;
	display(d3, "d3 = d1;");

	if (d1 == d3)
		cout << "d1 and d3 are equal." << endl << endl;
	else
		cout << "d1 and d3 are not equal." << endl<< endl;

	d3.add(666666);
	display(d3, "d3.add(666666);");

	d3.remove(3);
	display(d3, "d3.remove(3);");

	d3.clear();
	display(d3, "d3.clear();");

	if (d3.isEmpty())
		cout << "d3 is empty" << endl << endl;
	else
		cout << "d3 is not empty" << endl << endl;

	system("pause");
	return 0;
} // main

void display(DynamicIntArray &d, string title) {
	if (title != "")
		cout << title << endl;
	cout << "used = " << d.getUsed() << " capacity = " << d.getCapacity() << endl;
	cout << d << endl ;
} // display 
