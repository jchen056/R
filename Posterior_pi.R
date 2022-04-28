set.seed(20)
n = 20
x = runif(n,min=-1,max=1)
y = runif(n,min=-1,max=1)
r = sqrt(x^2+y^2)

plot(x,y,cex=0.5)
curve( sqrt(1-x^2), add=TRUE, col="red")
curve( -sqrt(1-x^2), add=TRUE, col="red")

c = sum(r<1)
pi/4 #exact
c/n  #approximate

data = rep(0,n) 
for(i in 1:n){
if(r[i] < 1){
data[i] = 1 
}
}
sum(data)
#we have data which follows a Bernoulli(p) distrbution
#we want to determine p

#first, we need a prior
#let us consider at first prior distribution on p; uniform case
p=seq(from=0,to=1,length.out=25)#numerical methods, different p values
post=rep(NA,25)
#dbinom(1,size=1,0.7)#note it is bernoulli
for (i in 1:25){
  post[i]=sum(dbinom(data,size=1,prob=p[i],log=TRUE))+log(1)
}

eps=p[17]-p[16]
post=exp(post)# we do log and then exponential for numerical reasons
post=post/sum(post*eps)# to normalize
#plot(p,post,type='l')
plot(p,post)
curve(1627920*x^13*(1-x)^7,add=TRUE)
#the number may vary depending on the number of successes you have

#-----------------------------
#now let us consider a different prior distribution
prior=function(x){#your prior does not have to be normalized at this point
  if(x<1 & x>1/2){
    -(x-1)*(x-1/2)
  }
  else{
    0
  }
}
post1=rep(NA,25)
#dbinom(1,size=1,0.7)#note it is bernoulli
for (i in 1:25){
  post1[i]=sum(dbinom(data,size=1,prob=p[i],log=TRUE))+log(prior(p[i]))
}
post1=exp(post1)# we do log and then exponential for numerical reasons
post1=post1/sum(post1*eps)# to normalize
plot(p,post1,type="l")
curve(1e8*1/2.59*x^13*(1-x)^7*(x-1/2)*(1-x),add=TRUE,col="red")