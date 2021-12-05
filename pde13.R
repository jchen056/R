#u_t=u_xx+u_x 
#x:[0,1];t:[0,0.02]
#epsx=0.1, epst-0.05
x=seq(from=0,to=1,by=0.1)
ncols=length(x)
t=seq(from=0,to=0.02,by=0.005)
nrows=length(t)
A=matrix(NA,nrow=nrows,ncol=ncols)
A[1,]=sqrt(x*(1-x))
A[,1]=0
A[,ncols]=0
for(i in 2:nrows){
  for (j in 2:(ncols-1)){
    A[i,j]=0*A[i-1,j]+0.475*A[i-1,j-1]+0.525*A[i-1,j+1]
  }
}
persp(t,x,A)#length(x):nrow of A
A
