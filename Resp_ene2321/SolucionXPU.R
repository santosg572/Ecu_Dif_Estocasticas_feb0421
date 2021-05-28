SolucionXPU <- function(del, pp){
   ss = dim(pp)
   n = ss[1]

   fx <- function(x, p){
   	  u1 <- -.4*p[1]*x[1]
   	  u2 <- -.2*p[2]*x[2]
   	  u <- u1+u2
   	  if (u < 0){
   	  	u = 0
   	  } else if (u > 1){
   	  	u = 1
   	  }
 
	  A = matrix(c(1 - 0.4 * u, x[2], -x[1], -1 - 0.2*u), ncol=2)
	  res = A %*% x
   }

   fw <- function(){
	  res = c(1,1)
   }

   xx = matrix(rep(0,2*n), ncol=2)

   dwx1 = 0.15*sqrt(del)*rnorm(n)
   dwx2 = 0.15*sqrt(del)*rnorm(n)
   
   xx[1,] = c(.5, .7)

   x1 = xx[1,]

   for (i in 2:n){
	  x2 = x1 + del*fx(x1, pp[i-1,]) + c(dwx1[i-1], dwx2[i-1])
	  xx[i,] = x2
	  x1 = x2
   }
   
   uu = matrix(rep(0,2*n), ncol=2)
   
   for (i in 1:n){
      u1 = -.4*pp[i,1]* xx[i,1]
      u2 = -.2*pp[i,2]* xx[i,2]
      uu[i, ] = c(u1,u2)
   }
   
   res = list(xx=xx, uu=uu)
}

