set.seed(1108)
#mle for binomial random variable-----------------------------
#random generation for the binomial distribution with parameters size and prob.
x=rbinom(1000,size=500,prob=0.6)
pest=mean(x)/500

#dbinom: density function; playing
#Lv=dbinom(x,size=500,prob=0.6,log = TRUE)#apply dbinom to every element in the vector x
#L=sum(Lv)#mle L doee not work bc scale; try to use sum(log)
#exp(L)#invert l back to L

#what value of p will give you the optimal?
#note that we want mininum(b/c of - sign) instead of mle
L=function(p){
  -sum(dbinom(x,size=500,prob=p,log=TRUE))
}
optim(0.2,L)

#try to do mle for normal-------------------------
set.seed(1108)
xn=rnorm(1000,mean=20,sd=2)
mE=mean(xn)
sdE=sd(xn)
L1=function(X){
  u=X[1]
  v=X[2]
  -sum(dnorm(xn,mean=u,sd=v,log=TRUE))
}
optim(c(10,1),L1)

#try cauchy-----------------------
set.seed(1108)
xc=rcauchy(1000,location=3,scale=5)
median(xc)#about the location but not mle
L2=function(p){
  -sum(  dcauchy(xc,location=p[1],scale=p[2],log=TRUE) )
}
optim(c(2.7,4.5),L2)