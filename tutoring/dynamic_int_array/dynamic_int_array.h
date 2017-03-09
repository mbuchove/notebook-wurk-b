#pragma once
#include <iostream>

namespace cs20a {

  class DynamicIntArray
  {
    
  public:
    DynamicIntArray();
    explicit DynamicIntArray(int size);
    DynamicIntArray(const DynamicIntArray & d);
    
    ~DynamicIntArray();
    DynamicIntArray & operator= (const DynamicIntArray& rhs);
    
    bool isEmpty() const;
    int getUsed() const;
    int getCapacity() const;
    
    void add(int element);
    void insert(int i, int element);
    int remove(int i);
    
    void clear();
    
    int& operator [](int i);
    
    friend std::ostream& operator << (std::ostream& outs, const DynamicIntArray& d);
    friend bool operator ==(const DynamicIntArray &d1, const DynamicIntArray &d2);
    
  private:
    int used, capacity;
    int* elements;
    
    void expandCapacity();

  }; // class DynamicIntArray
} // namespace cs20a
