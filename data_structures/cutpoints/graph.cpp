#include "graph.h"
#include <cassert>
#include <stack>

graph::graph()
{
  int vertex;
  cin >> size;
  adjList.resize(size,NULL);
  for(int i = 0; i < size; ++i) {
    cin >> vertex;
    while(vertex != -1) {
      adjList[i] = new vnode(vertex,adjList[i]); // insert at begining
      cin >> vertex;
    }
  }
}

int firstFalse(vector<bool> b)
{
  int i = 0;
  while(i < b.size() && b[i])
    i += 1;
  return i;
}
 
bool all(vector<bool> b)
{
  for(int i = 0; i < b.size(); ++i)
    if(!b[i])
      return false;
  return true;
}

void graph::dfs(vector<bool> &visited)
{
  int start = firstFalse(visited);
  int nbr, current = start;
  stack<int> S;
  vnodeptr cursor;
  visited[start] = true;
  S.push(start);
  // Supply the remaining code below

}
  

bool graph::connected(int excluded = -1)
{
  vector<bool> visited(size,false);
  if(excluded != -1)
    visited[excluded] = true;

  // Supply the remaining code below


}  
    
vector<int> graph::get_cutpoints()
{
  vector<int> cutpts;
  // Supply the remaining code below

  
}
  

   
  
