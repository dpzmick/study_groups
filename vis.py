#!/usr/bin/env python2

import sys
import matplotlib.pyplot as plt

xs = []
ys = []
title = ''

for line in sys.stdin:
    if line.startswith("title"):
        title += line.strip()[6:] + '\n'
        continue

    x, y = line.split('\t')
    x = int(x)
    y = int(y)
    xs.append(x)
    ys.append(y)

plt.title(title[:-1])
plt.bar(xs, ys, align='center')
plt.show()
