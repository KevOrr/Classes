#ifndef _DYNARR_H_
#define _DYNARR_H_

#include <cassert>
#include <iostream>
#include <string>

class RuntimeException {
protected:
    std::string errorMsg;
public:
    RuntimeException(const std::string& err) { errorMsg = err; }
    std::string getMessage() const { return errorMsg; }
};

class InvalidIndex : public RuntimeException {
public:
    InvalidIndex(const std::string& err): RuntimeException(err) {};
};

template <typename E>
class dynarr {
private:
    int capacity;
    E *A;

public:
    dynarr();
    dynarr(int N);
    dynarr(const dynarr<E> &other);
    dynarr<E> & operator=(const dynarr<E> &other);

    ~dynarr();

    E & operator[](int ndx) throw(InvalidIndex);

    int getCapacity();
    void reserve(int newcap, bool duplicate=true);
    // if newcap <= capacity, does nothing;
    // if capacity is 0, allocates a dynamic array of
    // capacity newcap and makes A point to that array;
    // otherwise allocates a new dynamic array newA of capacity
    // newcap, copies values in A to newA, deletes A and sets
    // A equal to newA
};

// Empty ctor
template <typename E>
dynarr<E>::dynarr(): capacity(0), A(nullptr) {}

// provide initial capacity
template <typename E>
dynarr<E>::dynarr(int N): capacity(N), A(new E[N]) {}

// copy ctor
template <typename E>
dynarr<E>::dynarr(const dynarr<E> &other): capacity(other.capacity), A(new E[other.capacity]) {
    *this = other;
}

// Assignment
template <typename E>
dynarr<E> & dynarr<E>::operator=( const dynarr<E> &other) {
    if (this != &other) {
        reserve(other.capacity, false);
        for (int i=0; i<capacity; ++i)
            A[i] = other.A[i];
    }
    return *this;
}

// dtor
template <typename E>
dynarr<E>::~dynarr() {
    delete[] A;
}

template <typename E>
E & dynarr<E>::operator[](int i) throw(InvalidIndex) {
    if (i < 0 || i >= capacity)
        throw new InvalidIndex("index must be greater than 0 and less than capacity");

    return A[i];
}

template <typename E>
int dynarr<E>::getCapacity() {
    return capacity;
}

template <typename E>
void dynarr<E>::reserve(int newcap, bool preserve) {
    if (newcap <= capacity) // noop
        return;

    else if (capacity == 0 || !preserve) { // reserve without duplication
        A = new E[newcap];

    } else { // reserve than duplicate
        E *newA = new E[newcap];
        for (int i=0; i<capacity; ++i)
            newA[i] = A[i];

        A = newA;
        capacity = newcap;
    }
}

#endif
