#u_t=u_xx+u_x x:[0,1];t:[0,0.02]
#epsx=0.1, epst-0.05
x=seq(from=0,to=1,by=0.1)
t=seq(from=0,to=0.02,by=0.005)
A=matrix(NA,nrow=5,ncol=11)
A[1,]=sqrt(x*(1-x))
A[,1]=0
A[,11]=0
for(i in 2:5){
  for (j in 2:10){
    A[i,j]=0*A[i-1,j]+0.525*A[i-1,j-1]+0.475*A[i-1,j+1]
  }
}
persp(t,x,A)
