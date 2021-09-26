locate.root = function(f,a=0,b=1,n=20){
  root = (a+b)/2 # choose the midpoint as your initial root approximation
  for(i in 1:n){
    if( f(a)*f(root) < 0 ){
      b = root
      root = (a+b)/2
    }
    else{
      a = root
      root = (a+b)/2
    }
  }
  root #final output result
}

# search for roots of the function f in the interval [a,b]
# by dividing the interval into N sub-intervals
# and using the bisection algorithm with n iteration on each sub-interval
roots = function(f,a=0,b=1,N=20,n=20){
  x = rep(NA,N+1)
  roots = NULL
  for( i in 1:(N+1) ){
    x[i] = a + (b-a)/N*(i-1)  # vector of sub-interval points
  }
  for( i in 1:N ){
    if( f(x[i])*f(x[i+1]) < 0 ){  # check for a change in sign
      roots = c(roots , locate.root(f,x[i], x[i+1], n) )
      # apply bisection algorithm on x[i] to x[i+1]
    }
  }
  roots # final output
}

# this searches for eigenvalues in a specified interval
# it is assumed the matrix is a square
# this will only locate the real eigenvalues
eigen = function(A,a=0,b=1,N=20,n=20){
  k = nrow(A)
  I = diag(k) # this is the n x n identity matrix
  f = function(lambda){det(A - I*lambda)}
  roots(f,a,b,N,n)
}

# This is a full row reduction calculator that allows you to set a tolerance error level
# The default value is set at tol = 0 
rr = function(A,tol=0) 
{
  stopifnot(is.numeric(A))
  if (!is.matrix(A)) 
    stop("Input parameter 'A' must be a matrix.")
  nr <- nrow(A)
  nc <- ncol(A)
  tol <- eps() * max(nr, nc) * max(abs(A))
  r <- 1
  for (i in 1:nc) {
    pivot <- which.max(abs(A[r:nr, i]))
    pivot <- r + pivot - 1
    m <- abs(A[pivot, i])
    if (m <= tol) {
      A[r:nr, i] <- 0
    }
    else {
      A[c(pivot, r), i:nc] <- A[c(r, pivot), i:nc]
      A[r, i:nc] <- A[r, i:nc]/A[r, i]
      if (r == 1) {
        ridx <- c((r + 1):nr)
      }
      else if (r == nr) {
        ridx <- c(1:(r - 1))
      }
      else {
        ridx <- c(1:(r - 1), (r + 1):nr)
      }
      A[ridx, i:nc] <- A[ridx, i:nc] - A[ridx, i, drop = FALSE] %*% 
        A[r, i:nc, drop = FALSE]
      if (r == nr) 
        break
      r <- r + 1
    }
  }
  A[abs(A) < tol] <- 0
  return(A)
}


