A=matrix(rbinom(9,size=3,prob=0.5),ncol=3,nrow=3)

#assume A is square b/c inverse only for sqr
matrix_2n=function(A){
  n=nrow(A)
  B=matrix(0,nrow=n,ncol=n)#used as identity matrix
  for(i in 1:n){
    B[i,i]=1
  }
  C=cbind(A,B)
  C
  }

inverse=function(A){
  #create matrix n*2n
  B=matrix_2n(A)
  C=rref(B)
  n=nrow(A)
  I=matrix(0,nrow=n,ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      I[i,j]=C[i,j+n]
    }
  }
  I
}
  
  