#random vectors(d-dimensional random variables)
#if we have a random vector X: X=(x,y), x,y are random variables
#usually, x,y are dependent
#if x,y independent, fX(s,t)=fx(s)*fy(t)


#u and v are independent
n=1e3
u=runif(n)
v=runif(n)
#x and y are dependent
x=u+v #x is a list of numbers
y=u-v #y is a list of numbers

#you can create a matrix: X=(x,y), you take a pair of numbers x and y
X=matrix(NA, nrow=n,ncol=2)
for(i in (1:(n))){
  X[i,]=c(x[i],y[i])
}
print(X)#every row is a random point in the plane
plot(X)#plot the matrix for you, which gives you support of X
plot(x,y)#gives you support of X
#based on the simulation, it seems uniformly distributed


#--------------------this time no longer uniform, there is a concentration
x1=u*u+v
y1=u*u-v
X1=matrix(NA,nrow=n,ncol=2)
for(i in (1:(n))){
  X1[i,]=c(x1[i],y1[i])
}
plot(X1)
plot(x1,y1)

#-----------
x2=u*cos(2*pi*v)
y2=u*sin(2*pi*v)
X2=matrix(NA,nrow=n,ncol=2)
for(i in (1:(n))){
  X2[i,]=c(x2[i],y2[i])
}
plot(X2)
