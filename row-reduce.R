pivot.below = function(A,r,c){
  n = nrow(A)
  if(r<n){
  A[r, ] = 1/A[r,c]*A[r, ] # create 1 in row r and col c
  for(i in (r+1):n){
    A[i, ] = A[i, ] + (-A[i,c])*A[r, ] #fill with 0s 
  }
  A #output result
  }
  else{
    A[r,] = 1/A[r,c]*A[r,]
    A #output result
  }
}


# R will search for a pivot point below a specified location
locate.pivot = function(A,r,c){
  n = nrow(A)
  x = A[r:n,c] # this extract the numbers we search through
  m = which.max(abs(x))
  max.row = A[r+m-1,]
  original.row = A[r,]
  A[r,] = max.row
  A[r+m-1,] = original.row
  A
}

# Our general row reduction calculator will fortunately be one of the 
# harder programming codes we do this semester. 
row.reduce = function(A){
  n = nrow(A)
  m = ncol(A)
  i = 1  # row number
  j = 1  # col number
  while( i <= n & j <=m ){  #while-loop is like a for-loop without specified start and end values
    A = locate.pivot(A,i,j)
    if(A[i,j] != 0 ){
      A = pivot.below(A,i,j)
      i = i + 1  
      j = j + 1
    }
    else{
      i = i
      j = j+1
    }
  }
  A
}


A = matrix( rbinom(12, size = 2, prob=0.5) , nrow = 3, ncol = 4)
print(A)
row.reduce(A)


 