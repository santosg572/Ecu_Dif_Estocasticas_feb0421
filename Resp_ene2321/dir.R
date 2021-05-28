rm(list=ls())

CreaFolder <- function(dir){
#	dir = 'nuevo'
	dd = getwd()
	
	if (file.exists(dir)){
		unlink(dir, recursive=TRUE)
       dir.create(file.path(dd,dir))
    } else {
       dir.create(file.path(dd,dir))
    }
}

pat = getwd()
file = 'file1'

CreaFolder(file)

pref = 'img_'

for (i in 1:10){
	nom_arch = paste(pref,i,'.jpg',sep='')
	
	jpeg(filename = file.path(pat,file,nom_arch))
	x = seq(0, 5, length.out=100)*rnorm(100) 
	plot(sin(x), type='l')
	
	dev.off()
}
