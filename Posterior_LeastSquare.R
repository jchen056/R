#4000054
#check out the Stan File
#use fancier algorithms than what we are using, faster
#convert everything into C, (the reason why it is faster)

set.seed(509)
x=rnorm(200,mean=69,sd=2.9)#height of men
y=6*x-220+rnorm(200,mean=0,sd=10)#Weight
plot(x,y)

#least square approach: (t(A)*A)^-1 *(t(A)*b)
A=cbind(x,1)#that is our A
B=solve(t(A)%*%A)%*%(t(A)%*%y)
plot(x,y)
curve(B[1]*x+B[2],add=TRUE,col="blue")

#throw in ourliers: run it last----------------------
x=c(x,100)
y=c(y,15)
#plot(x,y)
#because the outlier, 
#the line you come up with will be really bad using the least square method


#let us try to use bayesian approach----------------------
#think of "p" as parameters p(a,b)
#where a=p[1] and b=p[2], these are the parameters we want to determine
#use y as your data: what choices of a and b leads to observed y
#rexp(20,rate=0.25)#our prior for a
#rcauchy(20,scale=100)#our prior for b

#our estimate of mean and sd for x
mean(x)
sd(x)

L=function(p){
  sum(dnorm(y,mean=p[1]*mean(x)+p[2],sd=p[1]*sd(x),log=TRUE))+
    dexp(p[1],rate=0.25,log=TRUE)+dcauchy(p[2],scale=100,log=TRUE)
}

#to sample from posterior
S=lMCMC(L,c(10,-300),2e4)
plot(S)#2d normal dimension, xaxis: a; yaxis: b
mean(S[,1])#the slope: approximated
mean(S[,2])#the intercept, approximated
#therefore, we do not have to go through optim to find a and b as long sample is big enough

plot(x,y)
for (n in 1e2:2e4){#throw off inial points, which are useless
  curve(S[n,1]*x+S[n,2],add=TRUE,col="red")
}
#you are not getting a line but a band of line
#they are all possible lines of best fit
#using bayesian approach, you are not seeing one line of best fit, but all
#in addition, outliers have no impact


#that is for the comparison purpose------------------------
#do not worry it when running it(do not run it)
nL=function(p){
  -L(p)
}
pars=optim(c(10,-300),nL)$par
plot(x,y)
curve(pars[1]*x+pars[2],add=TRUE,col="red")
#A=cbind(x,1)#that is our A
#B=solve(t(A)%*%A)%*%(t(A)%*%y)
#curve(B[1]*x+B[2],add=TRUE,col="blue")

H=optim(c(10,-300),nL,hessian=TRUE)$hessian#our hessian
sigma=solve(H)
#enable mass package
plot(mvrnorm(2e4,c(pars[1],pars[2]),sigma))


#sample from posterior using lMCMC----------------------
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



