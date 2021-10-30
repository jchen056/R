least.square=function(A,b){
  b=matrix(b)#treat b as a matrix
  x=inv(t(A)%*%A)%*%(t(A)%*%b)
  x
}

#------------------------------------
# a function called eigv that takes a vector A and one of its eigenvalue 
#return one of its eigenvector associated with eigenvalue
eig_vector=function(A,lamda){
  n=nrow(A)
  I=eye(n)
  A1=rbind(A-lamda*I,rep(1,n))
  b=c(rep(0,n),1)
  least.square(A1,b)
}
A=rbind(c(1,2),c(3,4))
A
eig(A)
x=eig_vector(A,5.37)
