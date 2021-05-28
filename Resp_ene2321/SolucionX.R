SolucionX <- function(del, u){
   ss = dim(u)
   n = ss[1]

   fx <- function(x, u){
	  A = matrix(c(1-0.4 * u[1], x[2], -x[1], -1-0.2*u[2]), ncol=2)
	  res = A %*% x
   }

   fw <- function(){
	  res = c(1,1)
   }

   xx = matrix(rep(0,2*n), ncol=2)

   dwx = 0*sqrt(del)*rnorm(n)

   xx[1,] = c(.5, .7)

   x1 = xx[1,]

   for (i in 2:n){
	  x2 = x1 + del*fx(x1, u[i-1,]) + fw() * dwx[i-1]
	  xx[i,] = x2
	  x1 = x2
   }
   res = xx
}

