import numpy as np
import sdeint
import matplotlib.pyplot as plt

B = np.diag([1.0, 1.0]) # diagonal, so independent driving Wiener processes

tspan = np.linspace(0.0, 10.0, 10001)
u1 = np.linspace(0.0, 10.0, 10001)
u2 = np.linspace(0.0, 10.0, 10001)

x0 = np.array([3.0, 3.0])

def f(x, u1, u2, t):
    A = np.array([[1.0 - 0.4*u1, -x[1]],
                  [ x[2], -1.0 - .2*u2]])
    return A*x

def G(x, t):
    return B

result = sdeint.itoint(f, G, x0, tspan)
print(result)
plt.plot(result)
plt.show()


