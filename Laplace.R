#solve the Laplace's equation on the unit square domain u_xx+u_yy=0

#first, create an (n^2 by n^2) matrix based on the rule of difference equation
laplace.matrix = function(n){
  M = rep(0,n^4)
  M = matrix(M,nrow = n^2,ncol = n^2)
  
  M[1,1] = 4
  M[1,2] = -1
  M[1,n+1] = -1
  M[n,n] = 4
  M[n,n-1] = -1
  M[n,2*n] = -1
  M[n^2-n+1,n^2-n+1] = 4
  M[n^2-n+1,n^2-2*n+1] = -1
  M[n^2-n+1,n^2-n+2] = -1
  M[n^2,n^2] = 4
  M[n^2,n^2-1] = -1
  M[n^2,n^2-n] = -1
  
  for(i in 2:(n-1))
  {
    M[i,i] = 4
    M[i,i-1] = -1
    M[i,i+1] = -1
    M[i,i+n] = -1
  }
  
  for(i in 1:(n-2))
  {
    M[i*n+1,i*n+1] = 4
    M[i*n+1,i*n+2] = -1
    M[i*n+1,i*n+1+n] = -1
    M[i*n+1,i*n+1-n] = -1
  }
  
  for(i in 2:(n-1))
  {
    M[i*n,i*n] = 4
    M[i*n,i*n-1] = -1
    M[i*n,(i-1)*n] = - 1
    M[i*n,(i+1)*n] = -1
  }
  
  for(i in 2:(n-1))
  {
    M[n^2-n+i,n^2-n+i] = 4
    M[n^2-n+i,n^2-n+i+1] = -1
    M[n^2-n+i,n^2-n+i-1] = -1
    M[n^2-n+i,n^2-2*n+i] = -1
  }
  
  for(i in 2:(n-1))
  {
    for(j in 1:(n-2))
    {
      M[j*n+i,j*n+i] = 4
      M[j*n+i,j*n+i-1] = -1
      M[j*n+i,j*n+i+1] = -1
      M[j*n+i,j*n+i+n] = -1
      M[j*n+i,j*n+i-n] = -1
    }
  }
  
  M
}
#the boundary vector b is a sum of four other boundary vectors
boundary.vector = function(lower,right,upper,left,n){
  dx = 1/(n+1)
  dy = dx
  b0 = rep(0,n^2)#boundary vector b has length n*n
  
  b.lower = b0
  for(i in 1:n){
    b.lower[i] = lower(i*dx) 
  }
  
  b.right = b0
  for(i in 1:n){
    b.right[i*n] = right(i*dy) 
  }
  b.upper=b0
  for(i in 1:n){
    b.upper[n*n-n+i]=upper(i*dx)
  }
  b.left=b0
  for(i in 1:n){
    b.left[(i-1)*n+1]=left(i*dy)
  }
  b = b.lower + b.right+b.upper+b.left
  b
  
  # this code is incomplete you need a loop for b.upper and b.left
  # the output vector b will then be equal to b.lower + b.right + b.upper + b.left
  
}

#boundary.vector(lower,right,upper,left,3)

laplace.solve=function(lower,right,upper,left,n){
  A=laplace.matrix(n)
  b=boundary.vector(lower,right,upper,left,n)
  #solution u by row reduction, we care about the last row
  u=rref(cbind(A,b))[,n*n+1]
  #final solution
  U=matrix(u,nrow=n,ncol=n,byrow=TRUE)
  #the first row in the matrix is the bottom row of the grid u(1,1)...
  U}

lower=function(x){sin(pi*x)}
right=function(y){0}
upper=function(x){0}
left=function(y){0}
#n is the size of inner grid, 1/(n+1) is the delta
laplace.solve(lower,right,upper,left,4)