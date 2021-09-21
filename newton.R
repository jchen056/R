newtons.method=function(f,g,x0,n){
  # x1=x0-f(x0)/g(x0)
  # xn=xn-1-f(xn-1)/g(xn-1)
  v=rep(NA,n+1) #the sequence constructed given f,g,x0,n 
  v[1]=x0;
  #having the first element, we are starting with the sec element
  for(i in 2:(n+1)){
    v[i]=v[i-1]-f(v[i-1])/g(v[i-1])
  }
  v
}
f=function(x){
  2*x*x-x
}
g=function(x){
  4*x-1
}
mat.norm=function(A,type=c("one","inf","F")){
  n=ncol(A)#n is the number of cols
  m=nrow(A)#m is the number of rows
  if(type=="one"){
    #add up all the columns in absolute value, and out
    #of all those column sums, pick the largest number
    v=rep(0,n)
    for(i in 1:n){
      for(j in 1:m){
        v[i]=v[i]+abs(A[j,i])
      }
    }
    max(v)
  }
  #add up all the numbers in the rows with absolute
  #value, and out of all those row sums, pick the largest number
  else if(type=="inf"){
    vec=rep(0,m)
    for(i in 1:m){
      for(j in 1:n){
        vec[i]=vec[i]+abs(A[i,j])
      }
    }
    max(vec)
  }
  #square root of the sum of squares, which is how you compute the
  #norm of a vector in multivariable calculus
  else{
    s=0;
    for(i in 1:m){
      for(j in 1:n){
        s=s+A[i,j]*A[i,j] 
      }
    }
    sqrt(s)
  }
}

norm.one=function(A){
  n=ncol(A)#n is the number of cols
  m=nrow(A)#m is the number of rows
  #add up all the columns in absolute value, and out
  #of all those column sums, pick the largest number
  v=rep(0,n)
  for(i in 1:n){
    for(j in 1:m){
      v[i]=v[i]+abs(A[j,i])
    }
  }
  max(v)
}
A=matrix(c(2,1,2,3,4,1),nrow=3,ncol=2)