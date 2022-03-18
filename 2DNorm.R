#2-dimensional normal random vector------------------------------------
x=rnorm(1e5)#1d normal
y=rnorm(1e5)#1d normal
XX=matrix(NA,nrow=1e5,nco=2)#2d normal
plot(x,y,cex = .2)#support for 2d normal

#MASS package: mvrnorm--------------------------------------------------------
#X-Normal(m,sigma)#m is the mean vector and sigma is covariance matrix
m=c(0,0)#mean vector
Sig=rbind(c(9,-1),c(-1,1))#positive definite
eigen(Sig,only.values = TRUE)#return eigenvalues, all pos; eigenvalues determines how much more skewed
eigen(Sig)#if engenvectors almost the same, less skewness

#do not forget to check MASS library
X=mvrnorm(2000,m,Sig)#each row is a sample, which is two-dimensional point
plot(X,xlim=c(-8,8),ylim=c(-8,8))

#given the data, guess the mean
m1=mean(X[,1])
m2=mean(X[,2])
#given the data, guess the sigma("pos definite"):covariance matrix
cov(X[,1],X[,1])#covariance at row 1 col 1
cov(X[,1],X[,2])
cov(X[,2],X[,1])
cov(X[,2],X[,2])


#Box Muller---------------------------
u=runif(1e5)#uniform
v=runif(1e5)#uniform
n1=sqrt(-2*log(u))*cos(2*pi*v)#1 dimensional normal
PDF(n1,20)
n11=sqrt(-2*log(u))*sin(2*pi*v)#1 dim normal
PDF(n11,20)
N2=matrix(NA,nrow=1e5,ncol=2)
N2[,1]=n1
N2[,2]=n11
plot(n1,n11,cex = .01)





#PDF---------------------------
#a good n will help you decide which kind of PDF equation to use
PDF = function(X,n,y.min = 0,y.max = 1){
  a = min(X)
  b = max(X)
  N = length(X)
  s = (b-a)/n # spacing of the sub-intervals
  x = rep(NA,n+1)
  x.mid = rep(NA,n)
  for(i in 1:(n+1)){
    x[i] = a + (i-1)*s
  }
  
  for(i in 1:n){
    x.mid[i] = (.5)*( x[i+1] + x[i] )
  }
  
  f = rep(NA,n)
  for(i in 1:n){
    f[i] = sum( X > x[i] & X < x[i+1] )/(N*s)
  }
  plot(x.mid,f,type = "l",ylim = c(y.min,y.max) )
}