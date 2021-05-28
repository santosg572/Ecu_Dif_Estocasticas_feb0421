  gx <- function(x=0, u=0, p=0){
      u = -0.4*x[1]*p[1] - 0.2*x[2]*p[2]
#      A = matrix(c(1 - 0.4 * u, -x[1], -x[2], 1 + 0.2*u), ncol=2)
#      B = matrix(c(1, p[1], -p[2], 1), ncol=2) 
#      res = A %*% p + B %*% x + c(-1,0)

       A = matrix(c(-1 + 0.4 * u, x[1], -x[2], 1 + 0.2*u), ncol=2)
       B = matrix(c(x[2], 0, 0, -x[1]), ncol=2)
       res = A %*% p + B %*% p
   }



SolucionPPUD <- function(del=0, xx=0, uu=0, dwp=0){ 
   ss = dim(xx)
   np = ss[1]
   pp = matrix(rep(0,2*np), ncol=2)

   pp[1,] = c(16/15, 13/15)
   p1 = pp[1,]
	
   for (i in 2:np){
      x1 = xx[i-1,]
      u2 = uu[i-1]
      p2 = p1 + del * gx(x1, u2, p1) 
      pp[i,] = p2
      p1 = p2
   }
   res = pp
}

