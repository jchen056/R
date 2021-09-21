mat.norm=function(A,type=c("one","inf","F")){
  m=ncol(A)#n is the number of cols
  n=nrow(A)#m is the number of rows
  if(type=="one"){
    #add up all the columns in absolute value, and out
    #of all those column sums, pick the largest number
    v=rep(0,m)
    for(j in 1:m){
      for(i in 1:n){
        v[j]=v[j]+abs(A[i,j])
      }
    }
    max(v)
  }
  #add up all the numbers in the rows with absolute
  #value, and out of all those row sums, pick the largest number
  else if(type=="inf"){
    vec=rep(0,n)
    for(i in 1:n){
      for(j in 1:m){
        vec[i]=vec[i]+abs(A[i,j])
      }
    }
    max(vec)
  }
  #square root of the sum of squares, which is how you compute the
  #norm of a vector in multivariable calculus
  else{
    s=0;
    for(i in 1:n){
      for(j in 1:m){
       s=s+A[i,j]*A[i,j] 
      }
    }
    sqrt(s)
  }
}

