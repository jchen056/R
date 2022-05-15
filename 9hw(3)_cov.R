#hw9(3): using R to verify the answer

#load package MASS
m=c(0,0)
sigma=rbind(c(2,-1),c(-1,1))
x=mvrnorm(300000,m,sigma)
x1=x[,1]
x2=x[,2]
y=2*x1-3*x2
mean(y)#find mean for y
var(y)#find variance for y

#from the signma, we know
cov(x1,x2)#should be -1; cov(x2,x1)
cov(x1,x1)#should be 2; var(x1)
cov(x2,x2)#should be 1;var(x2)

