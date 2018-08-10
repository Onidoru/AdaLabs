## Ada Labs

This repository contains chosen labs on parallel and concurrent systems university courses.

Each lab implements a scalable system for concurrent matrix calculations - each lab scales the topology for the given number of threads.
Matrices/vectors/scalars are shared via different thread-communication mechanisms - protected modules (aka monitors) or Rendezvous (analog of channels, however entry/accept mechanism is more like server-client interaction, where server, after reaching accept point, blocks the thread until any client enters an entry point. With this kind of interaction states of the thread and objects are isolated and changed only in the entry/accept points of interactions). In labs with topologies the flow of data is allowed only between node connections, which are defined by topology.

Different matrix-vector operations are optimized for multithreading performance - each thread works with only one part of the matrix/vector, and the final result is achieved via merging all thread results.

### Monitors Lab
This lab implements a concurrent matrix calculations using Ada protected modules mechanism.

### Ring Topology
This lab implements a concurrent matrix calculations, where each node (one thread is simply one node) is connected with other nodes via the scalable ring topology.

### Grid Topology
This lab implements a concurrent matrix calculations, where nodes are connected via the scalable grid scheme.