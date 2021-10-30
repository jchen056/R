#calculate the line of best fit by minimizing total errors; 
#it is assumed that x,y are vectors of the same length
best.line.abs=function(x,y){
  n=length(x)
  f=c(0,0,rep(1,n))
  ONE=rep(1,n)
  I=diag(n)
  A.upper=cbind(x,ONE,-I)
  A.lower=cbind(x,ONE,I)
  A=rbind(A.upper,A.lower)
  dir=c(rep("<=",n),rep(">=",n))
  rhs=c(y,y)
  (lp("min",f,A,dir,rhs)$solution)[1:2]
}

least.square=function(A,b){
  b=matrix(b)#treat b as a matrix
  x=inv(t(A)%*%A)%*%(t(A)%*%b)
  x
}
best.line.square=function(x,y){
  n=length(x)
  A=cbind(x,rep(1,n))
  answer=least.square(A,y)
  as.vector(answer)
}
z=runif(100,min=-5,max=5)
x=runif(100,min=-20,max=20)
y=x+z
plot(x,y)
#best.line.abs(x,y)
best.line.square(x,y)
curve(1.010654*x,add=TRUE,col="red")
curve(1.0018827*x+-0.1042363,add=TRUE,col="blue")
curve(1*x,add=TRUE,col="green")