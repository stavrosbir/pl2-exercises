import random as r
import itertools
import sys

n = int(raw_input())
k = int(raw_input())
filename = raw_input()

f = open(filename, 'w')
f.truncate()
f.write("%d %d\n" % (n, k))

all_edges = list(itertools.combinations(range(1, n+1), 2))
edges = r.sample(all_edges, k)

for (i,j) in edges:
	f.write("%d %d\n" % (i, j))

f.close()
