
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
  #plot(t,x,type='l')
  plot(t,y,type='l')#we are solving for this one
  #plot(x,y,type="l")
}

#second order linear differential equation
#y′′+ a(t)y′ + b(t)y = c(t) with y(t0) = y0 and y′(t0) = y′


SODE=function(a,b,c,t0,y0,y0.,t.f,n=20){
  f=function(t,x,y){c(t)-a(t)*x-b(t)*y}
  g=function(t,x,y){x}
  euler.system(f,g,t0,y0.,y0,t.f,n)
}


euler.system.ans=function(f,g,t0,x0,y0,t.f,n){
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
  #plot(t,x,type='l')
  #plot(t,y,type='l')#we are solving for this one
  print(y[n+1])
}

SODE.ans=function(a,b,c,t0,y0,y0.,t.f,n=20){
  f=function(t,x,y){c(t)-a(t)*x-b(t)*y}
  g=function(t,x,y){x}
  euler.system.ans(f,g,t0,y0.,y0,t.f,n)
}
#y''-4ty'+(4t^2-2)y=0; y(0)=1,y'(0)=0
a=function(t){-4*t}
b=function(t){4*t*t-2}
c=function(t){0}
SODE(a,b,c,0,1,0,3,30)
ans=function(t){exp(t*t)}
curve(ans,add=TRUE,col='red')
SODE.ans(a,b,c,0,1,0,1,20)#approxiate y(1)
