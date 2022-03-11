#1. linear(affine) tranformation of a normal random variable is normal
#2. if you have a family of independent normals, then any linear combination of them is again normal
n=10000
#use rnorm to generate sample from normal distribution
M=rnorm(n,mean=69,sd=2.9)#male
Fem=rnorm(n,mean=64,sd=2.7)#female
Z=Fem-M#find p(Fem-M>0)
PDF(Z,20)

#mean and std for new normal variable Z
mz=mean(Z)
msd=sd(Z)
#P(Z>0)=1-P(Z<=0)=1-standard normal cdf of((0-mean)/sd)
Zphi=(0-mz)/msd
p0=1-CDF.norm(Zphi,10)#prob you are looking for
p0
#actual solution: pnorm(1,mean=,sd,):calculate cumulative prob up to 1
p0actual=1-pnorm(0,mean=mz,sd=msd)
p0actual













#-------additonal function needed to run the program
#a good n will help you decide which kind of PDF equation to use
PDF = function(X,n,y.min = 0,y.max = 1){
  a = min(X)
  b = max(X)
  N = length(X)
  s = (b-a)/n # spacing of the sub-intervals
  x = rep(NA,n+1)
  x.mid = rep(NA,n)
  for(i in 1:(n+1)){
    x[i] = a + (i-1)*s
  }
  
  for(i in 1:n){
    x.mid[i] = (.5)*( x[i+1] + x[i] )
  }
  
  f = rep(NA,n)
  for(i in 1:n){
    f[i] = sum( X > x[i] & X < x[i+1] )/(N*s)
  }
  plot(x.mid,f,type = "l",ylim = c(y.min,y.max) )
}

#hw #2: CDF.norm: calculate the cumulative prob up to a value t using n iterations
oddp=function(n){
  x=rep(NA,n)
  x[1]=1
  if(n>1){
    for(i in 2:n){
      x[i]=(2*i-1)*x[i-1]}
    x}
  else{
    x
  }
}
expp=function(t,n){
  x=rep(NA,n)
  for(i in 1:n){
    x[i]=t^((2*i-1))
  }
  x
}
#CDF.norm: calculate standard normal CDF of t
CDF.norm=function(t,n){
  x=oddp(n)
  y=expp(t,n)
  z=0.5+1/(sqrt(2*pi))*exp(-t*t/2)*sum(y/x)
  z
}