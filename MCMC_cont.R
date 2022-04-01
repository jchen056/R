#let us use MCMC to sample from continuous distribution
#PDF fx(s,t)=sqrt(1-s^2-t^2) support s^2+t^2<1

#here is my pdf
f=function(s,t){
  if((s^2+t^2)<=1)
    {
    sqrt(1-s^2-t^2)}
  else{
    0
  }
}
set.seed(120)
M=matrix(NA,nrow=5e3,ncol=2)#..-1 numbers and the first number is initial state
M[1,]=c(0,0)#the inital state must be in the support of the random variable
for(n in 2:5e3){
  #make sure you want to use symmetric
  proposal=M[n-1,]+runif(2,min=-0.2,max=0.2)
  #proposal=M[n-1,]+rnorm(2,mean=0,sd=0.2)#sd used not too big(likely to be out of bound) not too small(takes too long)
  #fx(j) and fx(i): prob of getting the proposed state and current state
  propP=f(proposal[1],proposal[2])#dpois calculate the probability/likelihood, plu numbers into pdf
  propC=f(M[n-1,1],M[n-1,2])
  r=propP/propC
  M[n,]=M[n-1,]#default: set next row(n) to the current row(n-1)
  if(r>=1){
    M[n,]=proposal
  }
  else{
    if(runif(1)<r){
      M[n,]=proposal
    }
  }
  
}