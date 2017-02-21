#ifndef __DLNODE__
#define __DLNODE__

#include <iostream>
#include <cassert>

template <class T>
class dlNode {
 public:
  T info;
  dlNode<T> *prev;
  dlNode<T> *next;
  dlNode<T>(T val, dlNode<T> *p, dlNode<T> *n):info(val),prev(p),next(n){};
};

template <class T>
void printDlList(dlNode<T>* header,dlNode<T> *trailer,const char *sep)
{
  assert(header != NULL && trailer != NULL);
  dlNode<T> *cursor = header->next;
  while(cursor->next != trailer) {
    std::cout << cursor->info << sep;
    cursor = cursor->next;
  }
  if (cursor->next == trailer)
    std::cout << cursor->info << std::endl;
}

template <class T>
void insertAfter(dlNode<T> *trailer, dlNode<T> *current, T newval)
{
  assert(current != trailer);
  current->next = new dlNode<T>(newval,current,current->next);
  current = current->next;
  current->next->prev = current;
}

template <class T>
void deleteNode(dlNode<T>* header, dlNode<T>* trailer, dlNode<T>* current)
{
  assert(current!= header && current != trailer);
  dlNode<T> *hold = current;
  current->prev->next = current->next;
  current->next->prev = current->prev;
  delete hold;
}

template <class T>
void deleteNext(dlNode<T>* header, dlNode<T>* trailer, dlNode<T> *current)
{
  assert(current != trailer && current->next != trailer);
  deleteNode(header,trailer, current->next);
}

template <class T>
void deletePrevious(dlNode<T> * header,dlNode<T> * trailer,dlNode<T> *current)
{
	assert(current != header && current->prev != header);
	deleteNode(header, trailer,current->prev);
}

template <class T>
void clearList(dlNode<T> *p)
{
  dlNode<T> *hold = p;
  while(p != NULL) {
    p = p->next;
    delete hold;
    hold = p;
  }
}

#endif
