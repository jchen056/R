#hypothesis testing using Bayesian methods
#I produce a drug which helps people fall asleep within 15 min
#have 120 people. randomly assign these 120 people into two groups
#control group: 40 people, not given the drug
#test group: 80people, given the drug
#results: in the control group, 10 people fall asleep in 15 min, 30 did not=>25%
#in the test group, 30 peoplefall asleep in 15 min, 50 did not=>37.5%

#p: % of people who fall asleep in 15 min
#q: % of people who fall asleep w. drugs in 15 min
#Pr(p<q)
#using Bayesian approaches, we treat p and q as random variables
#assign prior to p and q; for simplicity, let us do uniform(0,1)
#f(p)=posterior of p=c*p^10*(1-p)^30*1 supp(0,1)
#g(q)=posterior of q=c*q^30*(1-q)^50 supp(0,1)

#the idea is that we will draw samples from posterior and see how often q>p
lMCMC = function(f,x,n,sd=0.2){
  d = length(x)
  X = matrix(NA,nrow=n,ncol=d)
  X[1,] = x
  for(i in 2:n){
    proposal = X[i-1,] + rnorm(d,mean=0,sd=0.2)
    proposalL = f(proposal)
    currentL = f(X[i-1,])
    X[i,] = X[i-1,]
    r = proposalL - currentL#you are working with log instead
    if(r>=0){#log 1=0
      X[i,] = proposal
    }
    else{
      if( runif(1) < exp(r) ){
        X[i,] = proposal
      }
    }
  }
  X
}#use log-posterior

#unnormalized posterior for p and q and taking the log
f=function(x){
  if(x<1 & x>0){
    x^10*(1-x)^30
  }
  else{
    0
  }
}
g=function(x){
  if(x<1 & x>0){
    x^30*(1-x)^50
  }
  else{
    0
  }
}
Lf=function(x){
  log(f(x))
}
Lg=function(x){
  log(g(x))
}

#now let us sample from posterior for q
p=lMCMC(Lf,0.5,1e5)
q=lMCMC(Lg,0.5,1e5)
plot(density(p))
sum(q>p)/1e5
