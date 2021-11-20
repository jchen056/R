#application of partial differential equations(pde)
#diffusion equations u_t=u_xx
#initial conditions: u(x,0)=x^2
#u(0,t)=2*t(boundary conditions)
#u(5,t)=25+2*t

#example1
A=matrix(NA,nrow=6,ncol=6)
x=0:5
A[1,]=x*x#intial condition 1
t=seq(from=0,to=0.5,by=0.1)
A[,1]=2*t
A[,6]=25+2*t
for (i in 2:6){
  for(j in 2:5){
    A[i,j]=0.8*A[i-1,j]+0.1*A[i-1,j-1]+0.1*A[i-1,j+1]
  }
}
persp(x,t,A)

#a function u=u(x,t) that solves the following pde
#u_t=ku_xx with domain [x0,x1],[t0,t1]
#intial conditions u(x,0),u(0,t),u(5,t)
#eps1 is for x,esp2 is for t
diffusion_eq=function(x0,x1,t0,t1,f,g,h,eps1,eps2,k=1){
  x=seq(from=x0,to=x1,by=eps1)
  ncols=length(x)
  t=seq(from=t0,to=t1,by=eps2)
  nrows=length(t)
  A=matrix(NA,nrow=nrows,ncol=ncols)
  A[1,]=f(x)#f is u(x,0)
  A[,1]=g(t)#g is u(x0,t)
  A[,ncols]=h(t)#h is u(x1,0)
  for (i in 2:nrows){
    for (j in (2:(ncols-1))){
      A[i,j]=eps2*k/(eps1*eps1)*A[i-1,j+1]+(1-2*eps2*k/(eps1*eps1))*A[i-1,j]+k*eps2/(eps1*eps1)*A[i-1,j-1]
      
    }
  }
  print(A)
  persp(x,t,A)
}
f=function(x){x*x}
g=function(t){2*t}
h=function(t){25+2*t}
diffusion_eq(0,5,0,0.5,f,g,h,1,0.1,1)
