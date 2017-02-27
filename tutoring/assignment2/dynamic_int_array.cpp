#include "dynamic_int_array.h"

namespace cs20a {

  int growth_factor = 2;

  DynamicIntArray::DynamicIntArray(){
    used = 0;
    capacity = 0;
    
    elements = NULL;
  } // default constructor 

  DynamicIntArray::DynamicIntArray(int size){
    used = 0;
    capacity = size;
    elements = new int[capacity];
  } // explicit constructor 

  DynamicIntArray::DynamicIntArray(const DynamicIntArray & d)
  {
    capacity = d.capacity;
    used = d.used;    
    elements = new int[capacity];
    
    for (int i=0; i<used; i++)
      elements[i] = d.elements[i];
  } // copy constructor 

  DynamicIntArray& DynamicIntArray::operator= (const DynamicIntArray& rhs)
  {
    used = rhs.used;
    capacity = rhs.capacity;

    int* old_elements = elements;
    elements = new int[capacity];
    
    for (int i=0; i<used; i++)
      elements[i] = rhs.elements[i];

    delete old_elements;

    return *this;

  } // assignment operator 

  DynamicIntArray::~DynamicIntArray(){
    delete elements;
  } // destructor 


  bool DynamicIntArray::isEmpty() const{
    return used == 0;
  }

  int DynamicIntArray::getCapacity() const{
    return capacity;
  } // accessor 

  int DynamicIntArray::getUsed() const{
    return used;
  } // accessor 


  int& DynamicIntArray::operator [](int i) {
    return elements[i];
  }

  std::ostream& operator <<(std::ostream& outs, const DynamicIntArray& d){
    for(int i=0; i<d.used; i++)
      outs << d.elements[i] << " ";
    return outs;
  }

  bool operator ==(const DynamicIntArray &d1, const DynamicIntArray &d2){

    if(d1.used != d2.used)

      return false;

    for (int i=0; i < d1.used; i++)

      if (d1.elements[i] != d2.elements[i])

        return false;

    return true;

  }

  void DynamicIntArray::add(int element){
    if (used >= capacity) {
      expandCapacity();
    } // if used >= capacity 

    //add the new element
    elements[used] = element;
    //(*this)[used] = element;
    used++;

  } // add 


  void DynamicIntArray::insert(int i, int element)
  {
    if (used >= capacity) {
      expandCapacity();
    } // if used >= capacity 
    
    // shift everything to the right of i 1 spot to the right 
    for(int j=used-1; j>i-1; j--)
      {
	elements[j+1] = elements[j];
      }

    // set new element 
    elements[i] = element;
    used++;
    
  } // insert 

  int DynamicIntArray::remove(int i){
    if (i>used-1 || i<0)
      {
	return 0;
      }
    
    int val = elements[i];

    // shift everything right of i 1 index left 
    for (int j=i; j<used-1; j++)
      elements[j] = elements[j+1];

    used--;

    return val;

  } // remove 

  void DynamicIntArray::clear(){
    for(int i=0; i<used; i++){
      elements[i] = 0;
    }
    used = 0;
    //delete elements;
    //capacity = 0;
    //elements = NULL;
  }

  void DynamicIntArray::expandCapacity()
  {
    // create new memory allocation 
    if (capacity > 0)
      capacity *= growth_factor;
    else
      capacity += growth_factor;
    
    // create new larger array 
    int *old_elements = elements;      
    elements = new int[capacity];
    if ( !elements)
      {
	delete old_elements;
	printf("Error allocating memory!\n");
	exit( 1) ;
      } // check if new allocation failed 
    
    // copy values of array 
    for(int i=0; i<used; i++)
      elements[i] = old_elements[i];
    
    delete old_elements;
  } // expandCapacity 
  
} // end namespace cs20a
