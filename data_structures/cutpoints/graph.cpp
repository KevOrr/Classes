#include "graph.h"
#include <cassert>
#include <stack>

graph::graph() {
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

int firstFalse(vector<bool> b) {
    size_t i = 0;
    while(i < b.size() && b[i])
        i += 1;
    return i;
}

bool all(vector<bool> b) {
    for(size_t i = 0; i < b.size(); ++i)
        if(!b[i])
            return false;
    return true;
}

void graph::dfs(vector<bool> &visited) {
    int start = firstFalse(visited);
    stack<int> S;
    visited[start] = true;
    S.push(start);

    while (S.size()) {
        int el = S.top();
        S.pop();

        visited[el] = true;

        for (vnode * nextAdj = adjList[el]; nextAdj != nullptr; nextAdj = nextAdj->next) {
            if (!visited[nextAdj->vertex]) {
                S.push(nextAdj->vertex);
                visited[nextAdj->vertex] = true;
            }
        }
    }
}


bool graph::connected(int excluded = -1) {
    vector<bool> visited(size,false);
    if(excluded != -1)
        visited[excluded] = true;

    dfs(visited);
    return all(visited);

}

vector<int> graph::get_cutpoints() {
    vector<int> cutpts;

    for (size_t i=0; i<adjList.size(); i++)
        if (not connected(i))
            cutpts.push_back(i);

    return cutpts;
}
