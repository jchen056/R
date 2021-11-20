#partial differential equations with initial conditions
#u_x+u_y+u=e^(x+2y)
#u(x,0)=0

eps=0.1
x=seq(from=0,to=1,by=eps)#len(x)=11
n=length(x)
g=function(x,y){exp(x+2*y)}
#create a matrix which stores the info for u(i,j+1) 
A=matrix(NA,nrow=5,ncol=11)
A[1,]=0#u(x,0)=0 at row1, all 0 representing the time
for (i in 1:10){
  A[2,i]=(2-eps)*A[1,i]-A[1,i+1]+g((i-1)*eps,1*eps)*eps
}
for(i in 1:9){#i is the position
  A[3,i]=(2-eps)*A[2,i]-A[2,i+1]+g((i-1)*eps,2*eps)*eps
}
for (i in 1:8){
  A[4,i]=(2-eps)*A[3,i]-A[3,i+1]+g((i-1)*eps,3*eps)*eps
}

number_time=5#number of time intervals
for(i in (2:number_time)){
  for(j in (1:(n-i+1))){
    A[i,j]=(2-eps)*A[i-1,j]-A[i-1,j+1]+g((j-1)*eps,(i-1)*eps)*eps
  }
}

#exact mathematical solution
u=function(x,y){(1/4)*exp(x+2*y)-(1/4)*exp(x-2*y)}
A[4,7]#matrix index starts at 0, row and col are switched compared with real func
u(6*eps,3*eps)