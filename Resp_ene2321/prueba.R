rm(list=ls())

source('SolucionX.R')
source('SolucionP.R')

n = 12*5000
t = seq(0,5*12, length.out = n)

uu =  0*matrix(rep(1,2*n), ncol=2)

for (i in 1:2){

	par(mfrow = c(3,3))

   resX = SolucionX(t[2] - t[1], uu)
   plot(resX[,1], type='l')
   plot(resX[,2], type='l')
   plot(resX[,1], resX[,2], type='l')

   resP = SolucionP(t[2] - t[1], resX, uu)
   plot(resP[,1], type='l')
   plot(resP[,2], type='l')
   plot(resP[,1], resP[,2], type='l')

   uu = matrix(c(-.4*resP[,1]*resX[,1], -.2*resP[,2]*resX[,2]), ncol=2)

   plot(uu[,1], type='l')
   plot(uu[,2], type='l')
}
