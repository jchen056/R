#to implement gradient and Hessian, do not forget to install pracma package


#grad and hessian------------------------
f=function(X){
  x=X[1]
  y=X[2]
  z=X[3]
  (3*x*x+2*y*y+z*z)*exp(-x*x-y*y-z*z)
}

grad(f,c(0,0,1))#gradient at point=0 => critical points

#check for the Hessian:
#if eigenvalues of Hessian is all pos, local min
#if eigenvalues of Hessian is all neg, local max
#if eigenvalues of Hessian is indefinity, saddle point
eigen(hessian(f,c(1,0,0)),only.values=TRUE)#(1,0,0)local max
eigen(hessian(f,c(-1,0,0)),only.values=TRUE)#(-1,0,0)local max
eigen(hessian(f,c(0,1,0)),only.values=TRUE)#(0,1,0) saddle points
eigen(hessian(f,c(0,-1,0)),only.values=TRUE)#(0,-1,0) saddle point
eigen(hessian(f,c(0,0,1)),only.values=TRUE)#(0,0,1)saddle point
eigen(hessian(f,c(0,0,-1)),only.values=TRUE)#(0,0,-1)saddle point
eigen(hessian(f,c(0,0,0)),only.values=TRUE)#(0,0,0) local min


#Newton's methods for multi-variable---------------------------------
g=function(X){
  x=X[1]
  y=X[2]
  (log(x*x+y*y-2*x+2)+log(x*x+y*y-2*y+2))
}

newtons.method=function(f,x0,n){#note that this function is limited; st it does not work
  # x1=x0-f(x0)/g(x0), we do not to know g for multivariables
  # xn=xn-1-f(xn-1)/g(xn-1)
  nc=length(x0)#find out the length of x0, which will be a point
  v=matrix(NA,nrow =n+1,ncol=nc)
  #v=rep(NA,n+1) #the sequence constructed given f,g,x0,n 
  v[1,]=x0;
  #having the first element, we are starting with the sec element
  for(i in 2:(n+1)){
    v[i,]=v[i-1,]-inv(hessian(f,v[i-1,]))%*%grad(f,v[i-1,])
  }
  v
}
newtons.method(g,c(0.9,0.1),20)#note newtoms' method can blow up, depending on ur intial guess
grr=function(X){
  x=X[1]
  y=X[2]
  c(((2*x-2)/(x*x-2*x+y*y+2)+2*x/(x*x+y*y-2*y+2)),((2*y-2)/(x*x+y*y-2*y+2)+2*y/(x*x-2*x+y*y+2)))
}
optim(c(1,2),g)#optim works rlly well and is stable
