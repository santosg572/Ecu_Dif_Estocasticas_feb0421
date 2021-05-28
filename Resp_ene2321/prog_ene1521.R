rm(list=ls())

source('SolucionX.R')
source('CreaFolder.R')

gx <- function(x, p){
   A = matrix(c(1-0.4 * x[1]*p[1], -x[1], -x[2], 1+0.2*x[2]*p[2]), ncol=2)
   B = matrix(c(1, p[1], -p[2], 1), ncol=2) 
   res = A %*% p + B %*% x + c(-1,0)
}

gw <- function(p){
  res = p
}
   
fac = 2
n = fac*5001
t = seq(0,fac*5, length.out = n)
del = t[2] - t[1]

uu =  0*matrix(rep(1,2*n), ncol=2)
#uu = matrix(c(sin(t), sin(t)), ncol=2) + 2

fig = 'fig_'
prefijo = paste('fac_',fac,'uu_0_iter_10_dwx_0_dwp_0', sep='')
pat = getwd()

CreaFolder(prefijo)

for (jj in 1:10){
	fig_name = paste(fig, prefijo, '_', jj, '.jpg', sep='')
	print(fig_name)
	
	jpeg(filename = file.path(pat, prefijo,fig_name))
	
	resX <- SolucionX(del, uu)

par(mfrow = c(3,3))

plot(resX[,1], type='l', main=paste('Iter-', jj, sep=''))
plot(resX[,2], type='l')
plot(resX[,1], resX[,2], type='l')

ban = 1

if (ban == 1){
	
dwp = 0*sqrt(del)*rnorm(n)

pp = matrix(rep(0,2*n), ncol=2)

pp[n,] = c(0,0)
#pp[n,] = resX[n,]

p2 = pp[n,]

for (i in (n-1):1){
  p1 = p2 - del * gx(resX[i+1,], p2) - gw(p2) * dwp[i+1]
  pp[i,] = p1
  p2 = p1
}

plot(pp[,1], type='l')
plot(pp[,2], type='l')
plot(pp[,1], pp[,2], type='l')

for (i in 1:n){
	u1 = -.4 * resX[i,1] * pp[i,1]
	u2 = -.2 * resX[i,2] * pp[i,2]
	uu[i, ] = c(u1, u2)
	
}   

plot(uu[,1], type='l')
plot(uu[,2], type='l')
plot(uu[,1], uu[,2], type='l')
   

}
dev.off()
}