#Runge-Kutaa method(for first order DE): faster
by.eps=function(t0,y0,eps,f){
  c1=f(t0,y0)
  c2=f(t0+1/2*eps,y0+1/2*eps*c1)
  c3=f(t0+1/2*eps,y0+1/2*eps*c2)
  c4=f(t0+eps,y0+eps*c3)
  y=y0+1/6*eps*(c1+2*c2+2*c3+c4)
  y
}
Runge.kutta=function(f,t0,t.f,y0,n){
  t=t0
  y=y0
  eps=(t.f-t0)/n
  for(i in 1:n){
    y=by.eps(t,y,eps,f)
    t=t+eps
    #print(y)
  }
  y
}
#Euler method
Euler.method=function(f,t0,y0,t.f,n){
  eps=(t.f-t0)/n
  t=t0
  y=y0
  for(i in 1:n){
    y=y+eps*f(t,y)
    t=t+eps
  }
  y
}

df=function(t,y){t*y}
Runge.kutta(df,0,1,1,20)
Euler.method(df,0,1,1,20)