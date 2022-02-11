#random number generator: 
#(1)iterate an affine function mod n
#a and b defines the affine function
#N is the number of be modded
rng=function(a,b,N){
  x=b
  for(i in 2:N){
    x[i]=(a*x[i-1]+b)%%N
  }
  x
}

x=rng(3,5,101)
hist(x/101)
#a,b,N are seeds of RNG
#and technically this is pseudo-random number generator b/c it is not rlly random
#and only appears to be random
#----------------------------------
#(2)runif: uniform random distribution
set.seed(1)#if we choose the same seed, we should generate the same seq
x=runif(100,min=0,max=1)
hist(runif(10000,min=0,max=1))

 y=runif(10,min=-2,max=20)
 y<0
 sum(y<0)
 