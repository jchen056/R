set.seed(54)
#set.seed(20)
data=rbinom(20,size=1,pi/4)
sum(data)
curve(813960*x^14*(1-x)^6)

#negative log posterior
L=function(p){#dbinom bc bernoulli is the model
  -sum(dbinom(data,size=1,prob=p,log=TRUE))-dunif(p,min=0,max=1,log=TRUE)
}
optim(0.5,L,hessian=TRUE)#hessian is 1/sigma^2; value is your u
curve(dnorm(x,mean=0.7,
            sd=1/sqrt(95.23969)),
      col="red",add=TRUE)
#pr(p|data)=pr(data|p)*pr(p); bayes theorem





#-------------------------using Laplace for the 2d problems
data1=rnorm(100,mean=69,sd=2.9)
L1=function(p){#dnorm b/c norm is the model
  -sum(dnorm(data1,mean=p[1],sd=p[2],log=TRUE))-dunif(p[1],min=65,max=77,log=TRUE)-dexp(p[2],rate=0.5,log=TRUE)
}
H=optim(c(72,5),L1,hessian=TRUE)$hessian
Sigma=solve(H)#solve(H) is the same as inverting H
eigen(Sigma)#eigenvalues/eigenvectors give you a sense of the shape of the posterior distribution
solve(H)%*%H#yes, solve() can be used to invert matrix
#Pr(mu,v |data)?=pr(data|mu,v)*pr(mu)*pr(v)

