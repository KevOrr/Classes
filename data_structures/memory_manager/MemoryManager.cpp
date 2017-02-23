#include <cassert>
#include <iostream>
#include "dlUtils.h"
#include "MemoryManager.h"

MemoryManager::MemoryManager(unsigned int memtotal)
    : memsize(memtotal),
      baseptr(new unsigned char[memsize]) {

    blockdata dummyBlock(0, false, 0);
    header = new dlNode<blockdata>(dummyBlock, nullptr, nullptr);
    trailer = new dlNode<blockdata>(dummyBlock, nullptr, nullptr);

    blockdata originalBlock(memsize, true, baseptr);
    header->next = new dlNode<blockdata>(originalBlock, header, trailer);
    trailer->prev = header->next;
}

MemoryManager::~MemoryManager() {
    delete [] baseptr;
    clearList(header);
}

void MemoryManager::showBlockList()  {
    printDlList(header,trailer,"->");
}

void MemoryManager::splitBlock(dlNode<blockdata> *p, unsigned int chunksize) {
    assert(chunksize <= p->info.blocksize);
    if (chunksize == 0)
        return;
    else if (chunksize == p->info.blocksize) {
        p->info.free = false;
        return;
    }

    dlNode<blockdata> *next = p->next;
    blockdata unused(p->info.blocksize - chunksize, true, p->info.blockptr + chunksize);
    p->next = new dlNode<blockdata>(unused, p, next);
    next->prev = p->next;
}

unsigned char * MemoryManager::malloc(unsigned int request) {
    for (dlNode<blockdata> *cur = header->next; cur != trailer; cur = cur->next) {
        if (cur->info.free && cur->info.blocksize >= request) {
            splitBlock(cur, request);
            return cur->info.blockptr;
        }
    }

    return nullptr;
}

void MemoryManager::mergeForward(dlNode<blockdata> *p) {
    dlNode<blockdata> *hold = p->next;

    p->info.blocksize += hold->info.blocksize;
    p->next = hold->next;
    hold->next->prev = p;

    delete hold;
}

void MemoryManager::mergeBackward(dlNode<blockdata> *p) {
    dlNode<blockdata> *hold = p->prev;

    p->info.blocksize += hold->info.blocksize;
    p->info.blockptr = hold->info.blockptr;
    p->prev = hold->prev;
    hold->prev->next = p;

    delete hold;
}

void MemoryManager::free(unsigned char *ptr2block) {
    for (auto it=header->next; it != trailer; it = it->next) {
        if (it->info.blockptr == ptr2block) {
            if (it->prev->info.free)
                mergeBackward(it);
            if (it->next->info.free)
                mergeForward(it);

            it->info.free = true;
            return;
        }
    }
}
