set.seed(176)
data=rexp(100,rate=1.75)

#we have the data and we suppose that it follows the exponential distribution
#we do not know anything about the lamda
prior=function(x){
  if(x>0 & x<4){
    x*(4-x)
  }
  else{
    0
  }
}

L=seq(from=0,to=4,length.out=50)#our possible lamda for distribution
post=rep(NA,50)
for (i in 1:50){
  post[i]=sum(dexp(data,rate=L[i],log=TRUE))+log(prior(L[i]))
}

eps=L[17]-L[16]
post=exp(post)# we do log and then exponential for numerical reasons
post=post/sum(post*eps)# to normalize
plot(L,post,type="l")

which.max(post)#index where p gives your max
L[24]#our rate is 1.75, 1.877 is not too far away from it