import matplotlib.pyplot as plt
import numpy as np

f = plt.figure()

F = 1

mj_means = (2089.762)
tj_means = (0)
mincaml_means = (578.994)
c_means = (727.059)
pypy_means = (1959.507)

ind = np.arange(F)
width = 0.35
plt.bar(ind, mj_means, width, label='Method JIT')
plt.bar(ind + width, tj_means, width, label='Tracing JIT')
plt.bar(ind + 2 * width, mincaml_means, width, label='MinCaml')
plt.bar(ind + 3 * width, c_means, width, label='C')
plt.bar(ind + 4 * width, pypy_means, width, label='PyPy')

plt.xticks([])
plt.ylabel('Execution time (ms)')
plt.legend(loc='best')
plt.show()

f.savefig('figures/result.pdf', bbox_inches='tight')

g = plt.figure()

G = 1

mj_means = (8.332)
tj_means = (28.958)
mincaml_means = (2.866)
c_means = (25.263)
pypy_means = (10.836)

ind = np.arange(G)
width = 0.35
plt.bar(ind, mj_means, width, label='Method JIT')
plt.bar(ind + width, tj_means, width, label='Tracing JIT')
plt.bar(ind + 2 * width, mincaml_means, width, label='MinCaml')
plt.bar(ind + 3 * width, c_means, width, label='C')
plt.bar(ind + 4 * width, pypy_means, width, label='PyPy')

plt.ylabel('Execution time (ms)')
plt.xticks([])
plt.legend(loc='best')
plt.show()

g.savefig('figures/result2.pdf', bbox_inches='tight')
