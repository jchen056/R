# d is the demand vector; 
# r is the max normal production
# k is the cost of normal production per unit
# a is the max additional production
# K is the cost of additional cost per unit
# s is the storage cost per unit
cum_demand=function(d){
  n=length(d)
  dc=rep(0,n)
  for(i in 1:n){
    for(j in 1:i)
    {
      dc[i]=dc[i]+d[j]
    }
  }
  dc
}
matrix_A=function(n){
  B=rbind(diag(2*n),diag(2*n))
  A=matrix(0,nrow=n,ncol=2*n)
  for(i in 1:n){
    for(j in 1:i){
      A[i,j]=1
    }
  }
  for(x in 1:n){
    for(y in (n+1):(x+n)){
      A[x,y]=1
    }
  }
  A=rbind(B,A)
  A
}
cost=function(n,k,K,s){
  cost=rep(0,2*n)#cost vector
  for(i in 1:n){
    cost[i]=k+s*(n-i)
  }
  for(j in (n+1):(2*n))
  {
    cost[j]=K+s*(2*n-j)
  }
  cost
}
optimal.production=function(d,r,k,a,K,s)
{
  n=length(d)
  cost=cost(n,k,K,s)
  A=matrix_A(n)
  dir=c(rep(">=",2*n),rep("<=",2*n),rep(">=",n-1),"=")
  dc=cum_demand(d)
  RHS=c(rep(0,2*n),rep(r,n),rep(a,n),dc)
  lp("min",cost,A,dir,RHS)$solution
}
demand=c(100,200,400,300)
maxN=300
pN=20
maxA=100
pA=30
st=2
