rm(list=ls())

# lo principal que cambie del programa anterior fue que
# el contro -u- lo reemplace por -p- al inicio del programa

source('CreaFolder.R')
source('SolucionXP.R')
source('SolucionPP.R')
   
fac = 3
n = fac*5001
t = seq(0,fac*5, length.out = n)
del = t[2] - t[1]

#pp =  1*matrix(rep(c(1,0), c(n,n)), ncol=2)
pp =  1*matrix(rep(c(1,1), c(n,n)), ncol=2)

fig = 'fig_'
prefijo = paste('fac_',fac,'p_1_1_iter_20_dwx_10_dwp_10_u1eu2', sep='')
pat = getwd()

CreaFolder(prefijo)

for (jj in 1:20){
	fig_name = paste(fig, prefijo, '_', jj, '.jpg', sep='')
	print(fig_name)
	
	jpeg(filename = file.path(pat, prefijo,fig_name))
	
	R <- SolucionXP(del, pp)
	
	par(mfrow = c(3,3))
	plot(R$xx[,1], type='l', main=paste('Iter-', jj, sep=''))
	plot(R$xx[,2], type='l')
	plot(R$xx[,1], R$xx[,2], type='l')
	
	plot(R$uu[,1], type='l', main=paste('Iter-', jj, sep=''))
	plot(R$uu[,2], type='l')
	plot(R$uu[,1], R$uu[,2], type='l')
	
	pp <- SolucionPP(del, R$xx, R$uu)	
		
	plot(pp[,1], type='l', main=paste('Iter-', jj, sep=''))
	plot(pp[,2], type='l')
	plot(pp[,1], pp[,2], type='l')
	
	dev.off()
}