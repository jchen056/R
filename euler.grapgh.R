Euler.method.graph=function(f,t0,y0,t.f,n){
  eps=(t.f-t0)/n#epsilon
  t=rep(NA,n+1)
  t[1]=t0
  y=rep(NA,n+1)
  y[1]=y0
  for(i in 2:(n+1)){
    y[i]=y[i-1]+eps*f(t[i-1],y[i-1])
    t[i]=t[i-1]+eps
  }
  plot(t,y,type='l')
}
#y'=sin(t)*sqrt(y) with ic y(0)=2, find y(5)
f=function(t,y){sin(t)*sqrt(y)}
Euler.method.graph(f,0,2,5,5000)
exact.f=function(t){
  (1/4)*(9 + 4*sqrt(2) - 2*(1 + 2*sqrt(2))*cos(t) + cos(t)^2)}
curve(exact.f(x),add=TRUE,col="red")
