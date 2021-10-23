#minimize the maximum error
min_maxError=function(x,y){
  n=length(x)
  f=c(0,0,1)
  #three variables, a, b, and θ(max error)
  ONE=rep(1,n)
  A.up=cbind(x,ONE,-ONE)
  A.dn=cbind(x,ONE,ONE)
  A=rbind(A.up,A.dn)
  dir=c(rep("<=",n),rep(">=",n))
  rhs=c(y,y)
  lp("min",f,A,dir,rhs)$solution
}
x=runif(100,min=-20,max=20)
z=runif(100,min=-5,max=5)
y=x+z
plot(x,y)
min_maxError(x,y)