# Graph
- A graph is viewed as a pair of node and vertices, where vertex are from one node to the other.
- Set of nodes present in the graph are connected by the edges.
- Nodes are represented as integers.
- A graph is either an empty graph or a graph extended by a new node v together with its label and with edges to those of 
  v's successors and predecessors that are already in the graph. 
- An edge contains succesor/predecessor node and the label of the edge.
- In a graph it might be possible that we traverse twice through a node because a node might be reachable via different edges.
  But efficient way would be to visit the node only once. So one way is to mark the node with the label (mainly an integer which 
  is increased to 1 from 0 when it is visted once. So when next time we come to that node and see the label, we don't process it 
  again. 
