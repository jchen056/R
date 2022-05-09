set.seed(379)
data=rnorm(100,mean=69,sd=2.9)

#you are given the following data
#you assume it is normal
#but you do not know the parameters of the normal distribution: mu and std
#doing Bayesian way: beyond point estimate(mean(data)...), you get probability distribution to work with
#which can be used for margin of error

#give prior to rnadom variables(paramters): mu and sigma
#mu: uniform(65,77)
#sigma: exp(0.5), smaller sigma allows larger espsilon to occur
mu=seq(from=65,to=77,length.out=50)
sigma=seq(from=0.01,to=6,length.out=60)#it is likely that tiy are 3 std away from the mean

post=matrix(NA,nrow=50,ncol=60)#row corresponds to mu, sigma represents sigma, diff combinations
for(i in 1:50){
  for (j in 1:60){
    post[i,j]=sum(dnorm(data,mean=mu[i],sd=sigma[j],log=TRUE))
                  +dunif(mu[i],min=65,max=77,log=TRUE)
                  +dexp(sigma[j],rate=0.5,log=TRUE)
  }
}
post=exp(post)#take the exp 
eps.mu=mu[2]-mu[1]
eps.sigma=sigma[2]-sigma[1]
post=post/sum(post*eps.mu*eps.sigma)

max(post)#located at row=16, col=27
#therefore, the most likely estimate for mu and sigma
#mu=65+(16-1)*eps.mu
#sigma=0.01+(27-1)*eps.sigma
mu[16]
sigma[27]
#using persp to visualize
persp(mu[10:22],sigma[20:34],post[10:22,20:34],col="red",shade=0.6,theta=30,phi=35)
persp(volcano)