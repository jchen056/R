#random variables that only take on a finite # of values
#(1): fair coin with equal prob of getting heads/tails
coin=function(n){
  x=rep(NA,n)
  for (i in 1:n){
    if(runif(1)<0.5){
      x[i]=0
    }
    else{
      x[i]=1
    }
  }
  x
}
#(2)unfair coin
#a is the probability of getting 0
unfair_coin=function(n,a){
  x=rep(NA,n)
  for(i in 1:n){
    if(runif(1)>a){
      x[i]=1
    }
    else{
      x[i]=0
    }
  }
  x
}
#(3)---------------------------------
s=function(){
  r=runif(1)
  if(r<1/4){
    -1
  }
  else{
    if(r>1/4 & r<7/12){
      0
    }
    else{
      1
    }
  }
}
#x is a random variable with three outputs -1,0,1
#p(x=-1)=1/4, p(x=0)=1/3, p(x=1)=5/12
X=function(x1,x2,x3,a,b,c,n){
  x=rep(NA,n)
  for(i in 1:n){
    r=runif(1)
    if(r<a){
      x[i]=x1
    }
    else if(r>a & r<(a+b)){
      x[i]=x2
    }
    else{
      x[i]=x3
    }
  }
  x
}
hist(X(-1,0,1,1/4,1/3,5/12,12000))


#(5)sample
sample(c(-1,0,1),1200,replace=TRUE,prob=c(1/4,1/3,5/12))
hist(sample(c(-1,0,1),1200,replace=TRUE,prob=c(1/4,1/3,5/12)))

#(6)binomial
rbinom(1000,1,2/3)
#dbinom used to calculate binom
dbinom(3,5,1/2)