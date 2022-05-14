#hw10:
#suppose you have a unit square 0<=x<=1 and 0<=y<=1
#within the square, you have a mysterious ball located at coordinates(x0,y0)
#the coordinates are not known

#sb tosses a ball on this unit square and 
#tells you whether this ball is positioned to the east/west/north/south rel to ur mysterious ball
#experiment done many times, you can use data to locate the most likely position

x0=0.3
y0=0.8
#function rel.pos displays the rel pos
#return 0,1 depending on whetehr a point is to the left/right and up/down
rel.pos=function(x,y){
  if(x<x0 & y<y0){
    result=c(0,0)
  }
  if(x>x0 & y<y0){
    result=c(1,0)
  }
  if(x<x0 & y>y0){
    result=c(0,1)
  }
  if(x>x0 & y>y0){
    result=c(1,1)
  }
  result
}
#rel.pos(0.5,0.5)

#the ball is thrown 100 times in a uniform way inside this square
tosses=matrix(runif(200),nrow=100,ncol=2)
data=matrix(NA,nrow=100,ncol=2)
for (i in 1:100){
  data[i,]=rel.pos(tosses[i,1],tosses[i,2])
}

#suppose the mysterious point is locatedt at (X,Y)
#x,y can be treated as random variables; we do not their values
#priors:x,y are ditsributed uniformed on (0,1)
#different possible pairs of x and y
X=seq(from=0,to=1,length.out=101)
Y=seq(from=0,to=1,length.out=101)
post=matrix(NA,nrow=101,ncol=101)
for (i in 1:101){
  for (j in 1:101){
    post[i,j]=sum(dbinom(data[,1],size=1,prob=1-X[i],log=TRUE))+
      sum(dbinom(data[,2],size=1,prob=1-Y[j],log=TRUE))+dunif(X[i])+dunif(Y[j])
      
  }
}
post=exp(post)#taket the exp
epsX=X[2]-X[1]
epsY=Y[2]-Y[1]
post=post/sum(post*epsX*epsY)
max(post)
#location of the max
a=which(post == max(post), arr.ind = TRUE)
print(a[1])
print(a[2])
persp(post,col="red",shade=0.6,phi=35)

#laplace method
L=function(p){
  -sum(dbinom(data[,1],size=1,prob=1-p[1],log=TRUE))-
    sum(dbinom(data[,2],size=1,prob=1-p[2],log=TRUE))-dunif(p[1])-dunif(p[2])
}
optim(c(0.3,0.8),L,hessian=TRUE)
H=optim(c(0.3,0.8),L,hessian=TRUE)$hessian
Sigma=solve(H)#solve(H) is the same as inverting H
eigen(Sigma)


