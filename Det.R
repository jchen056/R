#global variable, which will be used to 
#count the number of row exchanges
#to change global variable use <<-
k=0
pivot.below.nd=function(A,r,c){
  n=nrow(A)
  if(r<n){
    for(i in (r+1):n){
      A[i,]=A[i,]-A[i,c]/A[r,c]*A[r,]
    }
  }
  A
}

# R will search for a pivot point below a specified location
locate.pivot = function(A,r,c){
  n = nrow(A)
  x = A[r:n,c] # this extract the numbers we search through
  m = which.max(abs(x))
  if(m!=1){
    k<<-k+1
  }
  max.row = A[r+m-1,]
  original.row = A[r,]
  A[r,] = max.row
  A[r+m-1,] = original.row
  A
}

row.reduce.nd=function(A){
  m=nrow(A)
  n=ncol(A)
  i=1
  j=1
  k=0
  while(i<=m & j<=n){
    A=locate.pivot(A,i,j)
    if(A[i,j]!=0){
      A=pivot.below.nd(A,i,j)
      i=i+1
      j=j+1
    }
    else{
      i=i
      j=j+1
    }
    }
  A
}

deter=function(A){
  n=ncol(A)
  d=1
  for(i in 1:n){
    d=d*A[i,i]
  }
  if(d/2==0){
    d
  }
  else{
    -d
  }
}
A=matrix(rbinom(9,size=3,prob=0.5),ncol=3,nrow=3)
B=row.reduce.nd(A)

