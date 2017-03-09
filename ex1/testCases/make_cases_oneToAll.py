import random as r
import itertools
import sys

n = int(raw_input())
k = int(raw_input())
filename = raw_input()

f = open(filename, 'w')
f.truncate()
f.write("%d %d\n" % (n, k))

nodes2 = range(2, n+1)
first = [(1,i) for i in range(2, n+1)]
all_edges = list(itertools.combinations(nodes2, 2))
edges = first + r.sample(all_edges, k-n+1)

for (i,j) in edges:
	f.write("%d %d\n" % (i, j))

f.close()
