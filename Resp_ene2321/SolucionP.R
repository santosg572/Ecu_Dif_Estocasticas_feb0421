SolucionP <- function(del, x, u){
	print('entra SolucionP')

   ss = dim(x)
   n = ss[1]

   gx <- function(x, p, u){
	  A = matrix(c(1-0.4 * u[1], -x[1], -x[2], 1+0.2*u[2]), ncol=2)
	  B = matrix(c(1, p[1], -p[2], 1), ncol=2) 
	  res = A %*% p + B %*% x + c(-1,0)
   }

   gw <- function(p){
	  res = p
   }


   dw = sqrt(del)*rnorm(n)

   pp = matrix(rep(0,2*n), ncol=2)

   pp[n,] = c(0,0)
   #pp[n,] = xx[n,]

   p2 = pp[n,]

   for (i in (n-1):1){
	  p1 = p2 - del * gx(x[i+1,], p2, u[i+1,]) - gw(p2) * dw[i+1]
	  pp[i,] = p1
	  p2 = p1
   }
   print(pp)
   res = pp
   
}


