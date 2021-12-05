#wave equation u_tt=u_xx
#initial conditions: x:[0,1];y:[0,1]
#u(x,0)=sin(pix),u_t(x,0)=0,u(0,t)=0,u(1,t)=0
#dx=0.1,dt=0.1
dx=0.01
dt=0.01
x=seq(from=0,to=1,by=0.01)
t=seq(from=0,t=1,by=0.01)
A=matrix(NA,nrow=101,ncol=101)
A[1,]=sin(6*pi*x)
A[,1]=0
A[,101]=0
#second row: utilize u_t(x,0) condition
for(j in 1:101){
  A[2,j]=A[1,j]+dt*0
}
for (i in 3:101){
  for (j in 2:100){
    A[i,j]=A[i-1,j-1]+A[i-1,j+1]-A[i-2,j]
  }
}
for(i in 1:101){
  plot(x,A[101-i,],xlim=c(0,1),ylim=c(-1,1),type='l')
}

#this wave equation only applies for the situation when dx=dt
wave.equation=function(xm,xM,tm,tM,dx,dt,fi,fi.prime){
  x=seq(from=xm,to=xM,by=dx)
  t=seq(from=tm,to=tM,by=dt)
  ncols=length(x)
  nrows=length(t)
  A=matrix(NA,nrow=nrows,ncol=ncols)
  A[1,]=fi(x)
  A[,1]=0
  A[,ncols]=0
  #initialize second row using u_t 
  for(j in 1:101){
    A[2,j]=A[1,j]+dt*fi.prime(x[j])
  }
  for (i in 3:101){
    for (j in 2:100){
      A[i,j]=A[i-1,j-1]+A[i-1,j+1]-A[i-2,j]
    }
  }
  print(A)
  for(i in 1:nrows){
    plot(x,A[nrows-i,],xlim=c(xm,xM),ylim=c(-1,1),type='l')
  }
}