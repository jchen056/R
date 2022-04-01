#general code for Markov Chain Monte Carlo using Metropolis algorithm

#f: pdf for a multi-dimensional distribution, single-vector input(see func for optim )
#x: starting state of Markov chain, a vector in R^d, d=length(x)
#n: how long the markov chain runs for
#sd=0.2: default standard deviation used to move to a prosed state
MCMC=function(f,x,n,sd=0.2){
  d=length(x)#d is the dimension of the starting state vector
  M=matrix(NA,nrow=n,ncol=d)#Markov chain, a matrix of diff samples from distribution
  M[1,]=x#markov chain start at x
  for(i in 2:n){
    proposal=M[i-1,]+rnorm(d,mean=0,sd)
    #Calculate the prob of proposal state and current state
    propP=f(proposal)#calculate the prob of proposal state
    propC=f(M[i-1,])
    r=propP/propC
    M[i,]=M[i-1,]#by defaut,the proposal state is the same as current state
    if(r>=1){
      M[i,]=proposal
    }
    else{
      if(runif(1)<r){
        M[i,]=proposal
      }
    }
  }
  M
}

#here is your odf
f=function(X){
  s=X[1]
  t=X[2]
  if((s^2+t^2)<1)
  {
    sqrt(1-s^2-t^2)}
  else{
    0
  }
}
A=MCMC(f,c(0,0),5e3)