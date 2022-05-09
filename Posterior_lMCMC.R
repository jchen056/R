#this algorithm uses Metropolis algorithm to construct a Markov Chain
#that draws samples from the log-likelihood f. 
#f represents the log of PDF of the random variables we want to samples from
#x is initial point/vector where you want to start the markov chain
#n is the number of samples you want to use; length of Markov chain
#sd is used in the random proposal state

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
}

#to illustrate codes, let us draw samples from Normal(0,1)
#make sure to use the log-likelihood i.e. the log of the PDF
f=function(s){
  -(1/2)*s^2
}
X=lMCMC(f,0,1e5)
D=density(X)#draw the pdf of the data
plot(D)
curve(dnorm(x,mean=0,sd=1),add=TRUE,col="red")
#here we do not have to take the log, we only take the log for pdf


#let us find sample from posterior rnorm(mean=69,sd=2.9)-------------------
set.seed(1155)
data=rnorm(100,mean=69,sd=2.9)
#here the function L() represent the log-likelihood 
#think of x as (mu, sigma)
L=function(x){
  sum(dnorm(data,mean=x[1],sd=x[2],log=TRUE))+
    dunif(x[1],min=65,max=77,log=TRUE)+
    dexp(x[2],rate=0.5,log=TRUE)
}
X=lMCMC(L,c(72,1),5e5,sd=2)#samples from posterior distribution
plot(X,cex=0.25)

#laplace approximation approximate posterior distribution----------------
#it is assumed that posterior is normal
nL=function(x){#note the laplace approx uses the negative
  -L(x)
  #-sum(dnorm(data,mean=x[1],sd=x[2],log=TRUE))-
   # dunif(x[1],min=65,max=77,log=TRUE)-
    #dexp(x[2],rate=0.5,log=TRUE)
}
optim(c(72,1),nL,hessian=TRUE)
#par most likely value for mu and sigma
mean(X[,1])#mean of the first column
mean(X[,2])#mean of the second column

#let us extract hessian and compute sigma; laplace approach
H=optim(c(72,1),nL,hessian=TRUE)$hessian
Sigma=solve(H)#inverse of the hessian, covariance matrix

#here we sample directly from posterior
cov(X[,1],X[,2])#covariance of col1 and col2; should be closer to 0 b/c x and mu are uncorelated
cov(X[,1],X[,1])#variance of the first col
cov(X[,2],X[,2])
