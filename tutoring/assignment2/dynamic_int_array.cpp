#include "dynamic_int_array.h"

namespace cs20a {

  int growth_factor = 20;

  DynamicIntArray::DynamicIntArray(){
    used = 0;
    capacity = 2;
    //elements = NULL;
    //elements = (int*)malloc(capacity*sizeof(int));
    
    elements = new int[capacity];
  }

  DynamicIntArray::DynamicIntArray(int size){
    used = 0;
    capacity = size;
    elements = (int*)malloc(size*sizeof(int));
  }


  DynamicIntArray::~DynamicIntArray(){
    delete elements;
  }


  bool DynamicIntArray::isEmpty() const{
    return used == 0;
  }

  int DynamicIntArray::getCapacity() const{
    return capacity;
  }

  int DynamicIntArray::getUsed() const{
    return used;
  }


  int& DynamicIntArray::operator [](int i) {
    //return *(elements + i*sizeof(int));
    return elements[i];
  }

  std::ostream& operator <<(std::ostream& outs, const DynamicIntArray& d){
    outs << d.capacity << std::endl;
    //int* ep = d.elements;
    for(int i=0; i<d.used; i++)
      outs << d[i] << " ";
      //outs << *(d.elements + i*sizeof(int)) << " ";
    //outs << *ep << " ";
    //ep += sizeof(int);
    return outs;
  }

  bool operator ==(const DynamicIntArray &d1, const DynamicIntArray &d2){

    if(d1.used != d2.used)

      return false;

    for (int i=0; i < d1.used; i++)

      if (*(d1.elements + i*sizeof(int)) != *(d2.elements + i*sizeof(int)))

        return false;

    return true;

  }

  void DynamicIntArray::add(int element){
    if (used >= capacity) {

      // create new memory allocation 
      if (capacity > 0)
	capacity *= growth_factor;
      else
	capacity += growth_factor;

      int *old_elements = elements;
      //elements = (int*)malloc(capacity*sizeof(int)); 

      

      std::cout << "reallocating " << capacity << std::endl;
      int *new_elements = (int*)realloc(elements, capacity*sizeof(int));
      std::cout << "allocated " << new_elements << std::endl;
      if (new_elements != NULL)
	elements = new_elements; 
      else
	{
	  //free(elements);
	  printf("Error allocating memory!\n");
	  exit( 1) ;
	}

      /*
      //copy the old data
      for(int i=0; i<used; i++){
	*(new_elements + i*sizeof(int)) = *(old_elements + i*sizeof(int));
      }
      std::cout << "copied " << used << " now freeing.." << std::endl;
      //clear the old memory
      free(old_elements);
      elements = new_elements;
      */
    } // if used >= capacity 

    //add the new element
    std::cout << "about to set new element" << std::endl;
    (*this)[used] = element;
    std::cout << "set new element " << used << std::endl;
    used++;

  } // add 



  int DynamicIntArray::remove(int i){
    int val = *(elements + i*sizeof(int));

    // shift everything right of i 1 index left 
    for (int j=i; j<used-1; j++)
      *(elements + j*sizeof(int)) = *(elements + (j+1)*sizeof(int));

    used -= 1;

    return val;

  } // remove 

  void DynamicIntArray::clear(){
    for(int i=0; i<used; i++){
      *(elements + i*sizeof(int)) = 0;
    }
    used = 0;
    //free(elements);
    //capacity = 0;
    //elements = NULL;
  }
}
