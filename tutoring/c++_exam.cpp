#include <iostream>

using namespace std;


int fib(int x);

int fib(int x) {
  if (x == 1) {
    return 1;
  }
  else
    return fib(x-1) + fib(x-2);
}


class Base1 {
public:
  ~Base1() { cout << " Base1's destructor" << endl; }
};

class Derived: public Base1 {
public:
  ~Derived() { cout << " Derived's destructor" << endl; }
};


// insertion sort 
void sort (int arr[], int length) {
  int j, temp;

  for (int i=0; i<length; i++) {
    j = i;
    while (j>0 && arr[j] < arr[j-1]){
      temp = arr[j];
      arr[j] = arr[j-1];
      arr[j-1] = temp;
      j--;
    }
  }
};


class A {
public : int f1(int i)
  {
    return i + 4;
  }

public : double f1(double d)
  {
    return d * 5.0;
  }
};

class B : public A
{
public : double f1(double d)
  {
    return d * 6.0;
  }
};


int y = 20;
namespace outer {
  int y = 10;
  namespace inner {
    int z = y;
  }
}


int main(){

  char letter = 'b';

  switch(letter)
    {
    case 'A':
    case 'a':
      cout << "That is a good grade." << endl;
      
    }


  int y = 4;
  int z = 5;

  int x = (y>z) ? y : z;

  cout << " x = " << x << endl;


  int n = 2;
  int m = ++n;
  int o = n++;

  cout << "m = " << m << endl;
  cout << "o = " << o << endl;
  

  for(int i=0; i<10; i++)
    cout << 5 + rand() % 5 << endl;
 
  cout << endl;


  int i1 = 7;
  int i2 = 8;
  int i3 = 9;
  
  int *p1, *p2, *p3;
  p1 = &i1;
  p2 = &i2;
  p3 = &i3;
  
  p1 = p2;
  *p2 += *p3;

  cout << "*p1 " << *p1 << endl;


  // why does this segfaults? 
  //cout << "fibonacci number 5 is  " << fib(5) << endl;
  
  int i = 2147483647;
  i += 1;
  cout << "i++ " << i << endl;

  int len = 10;
  int foo[10] = {5, 3, 6, 7, 1, 9, 10, 8, 4, 2};
  cout << "foo " << foo << endl;
  cout << "&foo " << &foo << endl;


  Derived d;
  
  
  sort(foo, len);
  for (int j=0; j<len; j++)
    cout << foo[j] << " ";
  cout << endl;
  

  A a;
  B b;
  // why does b.f1 convert 4 to double and use that function? 
  cout << "A/B funcs: " << a.f1(2) + a.f1(3.0) + b.f1(4) + b.f1(5.0) << endl;
  
  // int 
  x = -1; 
  try {
    cout << "Inside try \n";
    if (x < 0)
      {
	cout << "Before throw \n";
	throw x;
      }
  }
  catch (int x) {
    cout << "Exception Caught \n";
  }

  cout << "After catch \n";

  
  cout << outer::inner::z;
  getchar();
  
	
  return 0;
}
