#include "dynamic_int_array.h"

namespace cs20a {

  int growth_factor = 2; 

  DynamicIntArray::DynamicIntArray(){
    used = 0;
    capacity = 0;
    elements = NULL;    
  }

  DynamicIntArray::DynamicIntArray(int size){
    used = 0;
    capacity = size;
    elements = (int*)malloc(size*sizeof(int));
  }

  DynamicIntArray::~DynamicIntArray(){
    free(elements);
  }


  int DynamicIntArray::getUsed() const{
    return used;
  }

  int DynamicIntArray::getCapacity() const{
    return capacity;
  }


  bool DynamicIntArray::isEmpty() const{
    return used == 0;
  }

  void DynamicIntArray::expandCapacity(){
    capacity *= 2;    
  }

  int& DynamicIntArray::operator [](int i){
    return *(elements + i*sizeof(int));
  }

  std::ostream& operator << (std::ostream& outs, const DynamicIntArray& d){
    // doesn't specify what output should look like, using a placeholder for now 
    outs << d.capacity;
    return outs;
  }

  bool operator ==(const DynamicIntArray &d1, const DynamicIntArray &d2){
    if(d1.used != d2.used)
      return false;
    for (int i=0; i<d1.used; i++)
      if (*(d1.elements+i*sizeof(int)) != *(d2.elements+i*sizeof(int)))
	return false;
    return true;
  }


} //namespace cs20a 
