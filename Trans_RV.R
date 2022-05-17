x=rcauchy(5000,location=0,scale=1)
#mean(x)#
median(x)
#---------------------affine transformation of uniform distribution
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

X=runif(1e6,min=-2,max=1)
Y=3*X+2
PDF(Y,100,y.min=0,y.max=1)

#-------------------affine transformation of normal distribution
X=rnorm(1e6,mean=0,sd=1)#E(X)=0,var(X)=1
PDF(X,100)
Y=3*X+2
mean(Y)#should be 2
var(Y)#should be 9
PDF(Y,100)

#------------hw4: codes to very the answer-----------------
X=runif(1e6,min=-2,1)
Y=abs(X)
PDF(Y,100)


###############inverse function sampling methd###################
#--------------generate exp with lamda=1 from uniform(0,1)
#original support: (0,1); new support(0,infinity)
U=runif(1e5)
X=-log(1-U)
PDF(X,25)
curve(exp(-x),col="red",add=TRUE)

#generate cauchy with x0=0 and scale=1 from uniform(0,1)
U=runif(1e5)
X=tan(pi*(0.5-U))
PDF(X,35)#cauchy have heavy tails; why it is flat
Y=X[X<3 & X>-3] #mask indexing, filtering out outiers
PDF(Y,30)
curve(1/(pi*(1+x*x)),col="red",add=TRUE)
curve(dcauchy(x,0,1),col="green",add=TRUE)


