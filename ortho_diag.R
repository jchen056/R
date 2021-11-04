#find an orthogonal matrix, orthogonal diagonalization
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

A=rbind(c(1,2,3),c(2,0,4),c(3,4,-2))
#(1)find eigenvalues-----------------
eig(A)
#(2)find eigenvector------------------
x=eig_vector(A,5.687339)
y=eig_vector(A,-1.307261)
z=eig_vector(A,-5.380078)
#(3)normalize the eigenvector-------------
normalize_v=function(x){
  n=length(x)#number of elements in x
  sum=0;
  for(i in 1:n){
    sum=sum+x[i]*x[i]
  }
  sum=sqrt(sum)
  for(i in 1:n){
    x[i]=x[i]/sum
  }
  x
}
xx=normalize_v(x)
yy=normalize_v(y)
zz=normalize_v(z)
#(4)orthogonal matrix consists of eigenvectors
B=cbind(xx,yy,zz)
#(5)diagonal matrix will consists if eigenvalues
Diag_matrix=t(B) %*% A %*% B
B%*%Diag_matrix%*%t(B)#that is my A
eig(A)

#check for orthogonality
orthogonality=function(A){
  n=nrow(A)
  sum=0
  for(i in (1:(n-1))){
    colum_1=A[,i]
    for(j in ((i+1):n)){
      column_2=A[,j]
      sum=sum+colum_1%*%column_2
    }
  }
  sum
}
orthogonality(B)

ortho=function(x,y,z){
  sum=0
  a=as.vector(x)
  b=as.vector(y)
  c=as.vector(z)
  sum=a%*%b+a%*%c+b%*%c
  sum
}
#if sum is about zero, then it is orthogonal
sum=ortho(xx,yy,zz)
sum
orthogonality(B)