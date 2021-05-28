 fx <- function(x=0, p=0){
      u <- (-.4*p[1]*x[1] -.2*p[2]*x[2])
      if(is.nan(u)){
         u = 1
         print('existe un NaN')
         cat('SolucionXPU=', x, p, '\n')
         stop()
       } else if (u < 0){
            u = 0
       } else if (u > 1){
            u = 1
       }

       c11 =  1 - 0.4*u
       c22 = -1 - 0.2*u
       A = matrix(c(c11, x[2], -x[1], c22), ncol=2)
       res = A %*% x
 }


SolucionXPUI <- function(del=0, pp=0){
   ss = dim(pp)
   n = ss[1]

   xx = matrix(rep(0,2*n), ncol=2)
   np = trunc(n/2) +1 
   
   xx[np,] = c(16/15, 13/15)
   x2 = xx[np,]

   for (i in (np-1):1){
      p2  = pp[n-i+1,]
      x1 = x2 - del*fx(x2, p2)
      xx[i,] = x1
      x2 = x1
   }
   
   uu = rep(0,n)
   
   for (i in 1:np){
      pu = pp[n-i+1, ]
      xu = xx[i, ]
      u = -.4*pu[1]* xu[1] -.2*pu[2]* xu[2]
      uu[i] = u
   }
   
   x1 = c(16/15, 13/15)
 
   for (i in (np+1):(n-1)){
      p1  = pp[n-i,]
      x2 = x1 + del*fx(x1, p1)
      xx[i,] = x2
      x1 = x2
   }

   for (i in (np+1):n){
      pu = pp[n-i+1, ]
      xu = xx[i, ]
      u = -.4*pu[1]* xu[1] -.2*pu[2]* xu[2]
      uu[i] = u
   }

   res = list(xx=xx, uu=uu)
}

