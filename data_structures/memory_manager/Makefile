CXXFLAGS = -std=c++11 -g

main:  MemoryManager.o blockdata.o testMemMgr.o
	$(CXX) $(CXXFLAGS) -o $@ $^

testMemMgr.o:  testMemMgr.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $^

blockdata.o: blockdata.cpp blockdata.h dlUtils.h
	$(CXX) $(CXXFLAGS) -c -o $@ $<

MemoryManager.o: MemoryManager.cpp MemoryManager.h
	$(CXX) $(CXXFLAGS) -c -o $@ $<

clean:
	rm -f *.o main
