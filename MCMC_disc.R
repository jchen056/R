#let us use MCMC to sample from poisson(2)
set.seed(116)
M=rep(NA,1e4)#999 numbers and the first number is initial state
M[1]=3#the initial state must be in the support of the random variable; arbitrary
for(n in 2:1e4){
  #make sure you want to use symmetric
  proposal=M[n-1]+sample(c(-1,1),size=1)
  #fx(j) and fx(i): prob of getting the proposed state and current state
  propP=dpois(proposal,lambda = 2)#dpois calculate the probability/likelihood, plug numbers into pmf
  propC=dpois(M[n-1],lambda=2)
  r=propP/propC
  M[n]=M[n-1]
  if(r>=1){
    M[n]=proposal
  }
  else{
    if(runif(1)<r){
      M[n]=proposal
    }
  }
  
}