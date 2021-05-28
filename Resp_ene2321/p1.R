rm(list=ls())
a = 1.0
b = 0.8

f <- function(x,t){
   res = -(a + b^2 * x) * (1 - x^2)	
}

g <- function(x,t){
	res = b * (1-x^2)
}

tt = seq(0.0, 5.0, length.out=5001)

xx = 0*tt

xx[1] = .1
x1 = .1
del = tt[2]-tt[1]

u = sin(tt)
u = tt
u = rep(1,5001)

dw = sqrt((tt[2]-tt[1])) * rnorm(5001)

for (i in 2:5001){
	x2 = x1 + del*f(x1, t[i-1]) * u[i-1]+ g(x1, t[i-1]) * dw[i-1]
	xx[i] = x2
	x1 = x2
}

plot(xx, type='l')
   

