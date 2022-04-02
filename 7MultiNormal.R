#write your own script to sample from a Normal(m,sigma): where m is the vector of means 
#and sigma is covariance matrix; can be done using MASS package(mvnorm)

n=1e5


#d represents the dimension of the random vector
#n is the number of samples
homework7=function(d,n){
  M=matrix(NA,nrow=d,ncol=n)
  for(i in 1:d){
    M[i,]=rnorm(n)
  }
  M
}
X=homework7(3,1e3)
sig=rbind(c(4,12,-16),c(12,37,-43),c(-16,-43,98))
#sigma(covariance matrix)is positive definite; can be factored as A%*% transpose(A);
#Cholesky Factorization: chol, fact sigma into the form transpose(A)%*%A
At=chol(sig)
t(At)%*%At #should be equal to sigma
A=t(At)
Y=A%*%X+c(1,2,3)
Y=t(Y)

MV=mvrnorm(1e3,c(1,2,3),sig)#each row is a sample, which is two-dimensional point
plot(Y[,1],Y[,3])
plot(MV[,1],MV[,3])
