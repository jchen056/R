#twenty dice(six-sided) are thrown, probability that 
#the sum of numbers is equal to exactly 70

#will generate a 20*10^4 matrix of NAs
A=matrix(NA,nrow=20,ncol=1e4)

#fill up teh first row of the matrix with random numbers between 1-6
A[1,]=sample(1:6,size=1e4,replace=TRUE)#ith row:a dice is thrown 10000 times
for(i in 2:20){
  A[i,]=sample(1:6,size=1e4,replace=TRUE)
}

S=rep(NA,1e4)
for(i in 1:(1e4)){
  S[i]=sum(A[,i])
}
ps=sum(S==70)/(1e4)
print(ps)

#----------------binomial random number generator
#set.seed(113)
simulate.binomial=function(n,p){
#prob(xi=1)=p
  x=rep(NA,n)
  for(i in 1:n){
    x[i]=sample(c(0,1),size=1,prob=c(1-p,p))
  }
  x
}
#Draw 100 independent samples from the Binomial(1000, 1/2)
B=replicate(1000,simulate.binomial(1000,1/2))#do binomial 1000 times
C=rep(NA,1000)
for(i in 1:1000){
  C[i]=sum(B[,i])
}
rbinom(1000,1000,0.5)
