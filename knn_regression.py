#!/usr/bin/python3.6
import numpy as np
import sys

tau = [(-2, 1),
       (-1, 3),
       (1,3),
       (2,5),
       (4,7)]

x_range = np.arange(-3,5,0.001)


def closest(x, taup):
    c = taup[0]
    for t in taup:
        if abs(x - t[0]) < abs(x - c[0]):
            c = t

    return c


def neighbors(x, k):
    nn = []
    taup = list(tau)  # if you don't do this, it creates a deep copy
    while len(nn) < k:
        c = closest(x, taup)
        taup.remove(c)
        nn.append(c)

    return nn


def estimate(i, k):
    nn = neighbors(i, k)
    return sum([x[1] for x in nn]) / k


if __name__ == '__main__':
    #print(f"f_1(1.7) = {estimate(1.7, 1)}")
    #print(f"f_2(1.7) = {estimate(1.7, 2)}")
    #xx = [(i, estimate(i, 1)) for i in x_range]
    #plt.plot(*zip(*xx))

    try:
        K = int(sys.argv[1])
    except:
        print("usage: ./regression.py k")
        exit(1)

    import matplotlib.pyplot as plt


    print(estimate(0.5), 2)
    xxp = [(i, estimate(i, K)) for i in x_range]
    plt.plot(*zip(*xxp))

    plt.xlabel("input parameter x")
    plt.ylabel("output parameter t")
    plt.title(f"KNN estimate with k={K}")

    plt.scatter(*zip(*tau), color='red')
    plt.show()
