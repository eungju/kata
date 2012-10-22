#include <vector>
#include <queue>
#include <set>
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

struct Direction {
  int node;
  int distance;

  Direction(int node, int distance) {
    this->node = node;
    this->distance = distance;
  }

  Direction() {
    this->node = -1;
    this->distance = 0x7fffffff;
  }
};

bool closer(const Direction &a, const Direction &b) {
  return a.distance < b.distance;
}

typedef vector<Direction> Edges;

typedef vector<int> Path;

vector<Direction> dijkstra(Edges graph[], int nnode, int source) {
  vector<Direction> nodes(nnode);
  nodes[source].node = -1;
  nodes[source].distance = 0;
  queue<int> q;
  q.push(source);
  while (!q.empty()) {
    int current = q.front();
    q.pop();
    Edges &edges = graph[current];
    for (Edges::iterator i = edges.begin(); i != edges.end(); ++i) {
      int distance = nodes[current].distance + i->distance;
      if (distance < nodes[i->node].distance) {
        nodes[i->node].distance = distance;
        nodes[i->node].node = current;
        q.push(i->node);
      }
    }
  }
  return nodes;
}

int shortest_distance(vector<Direction> &nodes, int to) {
  return nodes[to].distance;
}

Path shortest_path(vector<Direction> &nodes, int to) {
  Path result;
  int u = to;
  while (u != -1) {
    result.push_back(u);
    u = nodes[u].node;
  }
  reverse(result.begin(), result.end());
  return result;
}

void path_print(Path path) {
  for (Path::iterator i = path.begin(); i != path.end(); ++i) {
    if (i != path.begin()) {
      cout << ",";
    }
    cout << *i;
  }
  cout << endl;
}

void load_graph(Edges graph[], char *filename) {
  ifstream input(filename);
  int prev_from = 0;
  while (input.good()) {
    string line;
    getline(input, line);
    int from, to, distance;
    sscanf(line.c_str(), "%d,%d,%d", &from, &to, &distance);
    if (prev_from != from) {
      sort(graph[prev_from].begin(), graph[prev_from].end(), closer);
      prev_from = from;
    }
    Direction direction(to, distance);
    graph[from].push_back(direction);
  }
  sort(graph[prev_from].begin(), graph[prev_from].end(), closer);
  input.close();
}

#ifdef TEST
char *filename = "sample.txt";
const int nnode = 30;
int start[] = { 0, 12, 14, 26 };
#else
char *filename = "input/input.txt";
const int nnode = 1000000;
int start[] = { 3336, 100214, 250000, 370000, 403333, 603336, 700000, 860000, 973000 };
#endif
Edges graph[nnode];
const int nstart = sizeof(start) / sizeof(int);
Direction nodes[nnode][nstart];

int main(int argc, char** argv) {
  load_graph(graph, filename);
  //cout << "Loading completed." << endl;

  vector<Direction> dijkstras[nstart];
  for (int i = 0; i < nstart; i++) {
    //cout << "Dijkstra shortest path for " << start[i] << endl;
    dijkstras[i] = dijkstra(graph, nnode, start[i]);
  }

  int best_node = -1;
  int best_cost = 0x7fffffff;
  for (int i = 0; i < nnode; i++) {
    int cost = 0;
    for (int j = 0; j < nstart; j++) {
      cost = max(cost, shortest_distance(dijkstras[j], i));
    }
    //cout << i << ":" << cost << endl;
    if (cost < best_cost) {
      best_node = i;
      best_cost = cost;
    }
  }

  //cout << "Best: " << best_cost << endl;
  for (int i = 0; i < nstart; i++) {
    path_print(shortest_path(dijkstras[i], best_node));
  }
  return 0;
}
