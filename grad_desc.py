from math import degrees
from numpy import arctan

x0 = [2, 1]
alpha = 0.05


def f(x):
    return x[0]**2 + 5*(x[1]**2)


def grad_f(x):
    return [2 * x[0], 10 * x[1]]


def norm(vec):
    return ( vec[0]**2 + vec[1]**2 )**0.5


def opp_ang(vec):
    x, y = vec
    return -degrees(arctan(y/x))


nsteps = 3
nsteps = range(nsteps)

x = x0

for i in nsteps:
    print(f"-------------\ncomputing for point {x}")
    v = grad_f(x)
    print(f"v = {v}")
    print(f"level set given by f(x) = {f(x0)}")

    grad_dir = [round( v[0] / norm(v), 2), round(v[1] / norm(v), 2)]
    print(f"gradient direction: {grad_dir}")
    print(f"opposite angle: {opp_ang(x)}")
    x[0] = x[0] - alpha*v[0]
    x[1] = x[1] - alpha*v[1]
