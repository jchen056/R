x=runif(1000,min=0,max=1)
y=runif(1000,min=-50,max=50)
z=runif(1000,min=0,max=1.1)
w=runif(1000,min=0,max=1.05)
s=x+y+z
PDF(s,20)

#CLT: hw5(3)
#using uniform(-1,1)
sample.normal=function(n){
  x=0
  for(i in 1:n){
    ui=runif(1,min=-1,max=1)
    x=x+ui
  }
  a=sqrt(3/n)# choose alpha to be positive 
  x=x*a
  x
}
A=replicate(1e4,sample.normal(8))
PDF(A,20)

B=A[A<-6 & A>6]#B is not within (-6,6)
pro6=length(B)/length(A)
print(pro6)
#normal distribution exception: P(fall outside of -6<x<6)
in6=pnorm(6,mean=0,sd=1)-pnorm(-6,mean=0,sd=1)
ou6=1-in6
print(ou6)