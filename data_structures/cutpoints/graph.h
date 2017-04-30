#ifndef GRPH
#define GRPH

#include <iostream>
#include <vector>

using namespace std;

struct vnode {
  int vertex; 
  vnode *next;
  vnode(int u, vnode *n): vertex(u), next(n){};
};

typedef vnode * vnodeptr;

class graph {
 public:
  graph(); // interactive constructor using cin
  bool connected(int excluded);
  void dfs(vector<bool> &visited);
  vector<int> get_cutpoints();
    
 private:
  int size;
  vector<vnodeptr> adjList;
};

#endif
