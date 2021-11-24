#diffusion equation: boundless case
#u_t=k*u_xx
#given u(x,0)=dirac delta function(x)

#rho=dt/(dx*dx)
diffusion_boundless=function(k,dx,rho,xm,xM,tM){
  dt=rho*dx*dx
  x=seq(from=xm,to=xM,by=dx)
  t=seq(from=0,to=tM,by=dt)
  row_number=length(t)
  col_number=length(x)
  A=matrix(NA,nrow=row_number,ncol=col_number)
  A[1,]=0
  A[1,(col_number+1)/2]=1/dx
  for(i in 2:row_number){
    for(j in 2:(col_number-1)){
      A[i,j]=(-2*rho*k+1)*A[i-1,j]+rho*k*A[i-1,j-1]+rho*k*A[i-1,j+1]
    }
  }
  #for(i in 1:10){
   # plot(x,A[11-i,],type='l')
  #}
  A
}
diffusion_boundless(1,0.1,0.25,-5,5,0.5)