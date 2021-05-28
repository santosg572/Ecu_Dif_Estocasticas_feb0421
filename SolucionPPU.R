  gx <- function(x=0, u=0, p=0){
      u = -0.4*x[1]*p[1] - 0.2*x[2]*p[2]
#      A = matrix(c(1 - 0.4 * u, -x[1], -x[2], 1 + 0.2*u), ncol=2)
#      B = matrix(c(1, p[1], -p[2], 1), ncol=2) 
#      res = A %*% p + B %*% x + c(-1,0)

       A = matrix(c(-1 + 0.4 * u, x[1], -x[2], 1 + 0.2*u), ncol=2)
       B = matrix(c(x[2], 0, 0, -x[1]), ncol=2)
       res = A %*% p + B %*% p
   }



SolucionPPUI <- function(del=0, xx=0, uu=0, dwp=0){ 
   ss = dim(xx)
   n = ss[1]
   np = trunc(n/2) + 1

   pp = matrix(rep(0,2*n), ncol=2)

   pp[np,] = c(16/15, 13/15)

   p2 = pp[np,]
	
   for (i in (np-1):1){
      x2 = xx[i+1,]
      u2 = uu[i+1]
      p1 = p2 - del * gx(x2, u2, p2) 
      pp[i,] = p1
      p2 = p1
   }
   res = pp
}

