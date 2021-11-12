#f and g are differential equaltions
euler.system=function(f,g,t0,x0,y0,t.f,n){
  eps=(t.f-t0)/n
  t=rep(NA,n+1)
  t[1]=t0
  x=rep(NA,n+1)
  x[1]=x0
  y=rep(NA,n+1)
  y[1]=y0
  for(i in 2:(n+1)){
    x[i]=x[i-1]+eps*f(t[i-1],x[i-1],y[i-1])
    y[i]=y[i-1]+eps*g(t[i-1],x[i-1],y[i-1])
    t[i]=t[i-1]+eps
  }
  plot(t,x,type="l")
  plot(t,y,type="l")
  plot(x,y,type="l")
}
f=function(t,x,y){y}
g=function(t,x,y){-x}
#intial condistions: x(0)=0,y(0)=1
euler.system(f,g,0,0,1,2*pi,2000)