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

