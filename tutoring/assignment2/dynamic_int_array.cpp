#include "DynamicIntArray.h"

namespace cs20a {

  DynamicIntArray::DynamicIntArray(){
    used = 0;
    capacity = 1;
    elements = NULL;

  }

  explicit DynamicIntArray(int size){
    used = 0;
    capacity = size;
    //elements
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
  }

  bool operator ==(const DynamicIntArray &d1, const DynamicIntArray &d2){
    if(d1.used != d2.used)
      return false;
    //for (int i=0; i<used; i++)
    //
    return true;
  }


} //namespace cs20a 
