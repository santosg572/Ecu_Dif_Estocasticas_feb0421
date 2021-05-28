SolucionPPU <- function(del, xx, uu){ 
	ss = dim(xx)
	n = ss[1]
	
	gx <- function(x, u, p){
		u1 = u[1]
		u2 = u[2]
		u = u1+u2
		if (u < 0){
			u = 0
		} else if (u > 1){
			u = 1
		}
		A = matrix(c(1 - 0.4 * u, -x[1], -x[2], 1 + 0.2*u), ncol=2)
		B = matrix(c(1, p[1], -p[2], 1), ncol=2) 
		res = A %*% p + B %*% x + c(-1,0)
	}
	
	gw <- function(p){
		res = p
	}
	
	dwp = 0.15*sqrt(del)*rnorm(n)
	
	pp = matrix(rep(0,2*n), ncol=2)
	
	pp[n,] = c(0,0)
	
	p2 = pp[n,]
	
	for (i in (n-1):1){
		p1 = p2 - del * gx(xx[i+1,], uu[i+1,], p2) - gw(p2) * dwp[i+1]
		pp[i,] = p1
		p2 = p1
	}
	res = pp
}

