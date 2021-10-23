#bets fit plane in 3d
bestPlane=function(x,y,z){
  n=length(x)
  ONE=rep(1,n)
  I=diag(n)
  A.upp=cbind(x,y,ONE,-I)
  A.dn=cbind(x,y,ONE,I)
  A=rbind(A.upp,A.dn)
  dir=c(rep("<=",n),rep(">=",n))
  rhs=c(z,z)
  f=c(0,0,0,rep(1,n))
  (lp("min",f,A,dir,rhs)$solution)[1:3]
}
x=runif(100,min=-20,max=20)
y=runif(100,min=-15,max=15)
e=runif(100,min=-1,max=1)
z=2*x+3*y+2+e
bestPlane(x,y,z)