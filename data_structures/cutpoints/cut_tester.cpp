#include "graph.h"
using namespace std;

int main()
{
  graph G;
  if(!G.connected(-1)) {
    cout << "Graph is not connected; terminating" << endl;
    return 1;
  }
  vector<int> cutpoints = G.get_cutpoints();
  cout << "Number of cutpoints: " << cutpoints.size() << endl;
  cout << "Cutpoints: ";
  for(int i = 0; i < cutpoints.size(); ++i)
    cout << cutpoints[i] << " ";
  cout << endl;
  return 0;
}
