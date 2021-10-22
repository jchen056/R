#calculate the line of best fit by minimizing total errors; 
#it is assumed that x,y are vectors of the same length
best.line=function(x,y){
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

z=runif(100,min=-5,max=5)
x=runif(100,min=-20,max=20)
y=x+z
plot(x,y)
best.line(x,y)
curve(1.067261*x,add=TRUE,col="red")