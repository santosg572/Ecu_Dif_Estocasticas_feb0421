rm(list=ls())

# lo principal que cambie del programa anterior fue que
# el contro -u- lo reemplace por -p- al inicio del programa

source('CreaFolder.R')
source('SolucionXPU.R')
source('SolucionPPU.R')
   
fac = 6
n = fac*50000
t = seq(0,fac*5, length.out = n)
del = t[2] - t[1]

cc = rep(0, n)
#pp =  matrix(c(t, cc), ncol=2)
#pp =  matrix(rep(c(0,1), c(n,n)), ncol=2)
#pp =  matrix(rep(c(1,1), c(n,n)), ncol=2)
ppI =  matrix(rep(c(-.8125, .0769), c(n,n)), ncol=2)
ppD =  matrix(rep(c(-.8125, .0769), c(n,n)), ncol=2)


dwx_v = 0
dwp_v = 0
fig = 'fig_'
prefijo = paste('fac_',fac,'p_1_0_iter_20_dwx_',dwx_v, '_dwp_', dwp_v, sep='')
pat = getwd()

CreaFolder(prefijo)

niter = 5

for (jj in 1:niter){
	fig_name = paste(fig, prefijo, '_', jj, '.jpg', sep='')
	print(fig_name)
	
	jpeg(filename = file.path(pat, prefijo,fig_name))
	
	dwx = dwx_v*sqrt(del)*matrix(rnorm(2*n), ncol=2)
	 
	RI <- SolucionXPUI(del, ppI)
        xI = RI$xx
        uI = RI$uu
	RD <- SolucionXPUD(del, ppD)
	xD = RD$xx
        uD = RD$uu

        par(mfrow = c(3,2))
	plot(c(xI[,1], xD[,1]) , type='l', main=paste('Iter-', jj, sep=''))
	plot(c(xI[,2], xD[,2]) , type='l')
	plot(c(xI[,1], xD[,1]), c(xI[,2], xD[,2]), type='l')	
	plot(c(uI, uD), type='l', main=paste('Iter-', jj, sep=''))
	
	dwp = dwp_v*sqrt(del)*rnorm(n)

	ppI <- SolucionPPUI(del, xI, uI)	
        ppD <- SolucionPPUD(del, xD, uD)
	p1I = ppI[,1]
        p2I = ppI[,2]

        p1D = ppD[,1]
        p2D = ppD[,2]
 	
	plot(c(p1I, p1D), type='l', main=paste('Iter-', jj, sep=''))
	plot(c(p2I, p2D) , type='l')
	
	dev.off()
}
