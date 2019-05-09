library(glmnet)
set.seed(1234)

n=500
x=as.matrix(seq(-2,2,length.out=n))
x=scale(x) # remember to always scale
k=500
# change this (the spectral density)
omega1<-t(cbind(c(rep(3,k)))) 
omega2<-t(cbind(c(rep(10,k/2),rep(20,k/2))))
omega3<-t(cbind(c(rep(1,k/5),rep(2,k/5),rep(3,k/5),rep(4,k/5),rep(5,k/5)))) 
omega4<-t(cbind(c(rep(10,k/5),rep(20,k/5),rep(30,k/5),rep(40,k/5),rep(50,k/5)))) 
omega5<-t(rnorm(k,0,5))
omega6<-t(rcauchy(k,0,0.5))
par(mfrow=c(3,4))
nsamp<-10
for(i in 1:6){
	eval(parse(text=paste0("omega<-omega",i)))
	xxprojected <- x%*%(omega) # project
	f<- sqrt(1/k)*cbind(cos(xxprojected),sin(xxprojected)) # monte carlo bit
	K<-(f)%*%t(f) # recreate kernel matrix
	cK=chol(K+diag(1e-6,n))
	samp<- matrix(rnorm(n*nsamp),nrow=nsamp,ncol=n) %*% cK
	samp<-t(samp)
	hist(omega,20,col='black',main="Spectral Density")
	plot(x,samp[,1],type='l',ylim=c(min(samp),max(samp)),main="Samples")
	for(j in 1:nsamp){
		lines(x,samp[,j],type='l',col=j)
	}
}


#alternative way to sample functions similar to the primal
#samp<- f %*% t(matrix(rnorm(ncol(f)*5),nrow=5,ncol=ncol(f)))
#plot(x,samp[,1],type='l',ylim=c(min(samp),max(samp)))
#for(i in 1:5){
#	lines(x,samp[,i],type='l',col=i)
#}
