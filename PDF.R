# Write a code for approximating the PDF from a sample X of some distribution
# where n is the number of sub-intervals you will use
# The default window on the vertical axis is from 0 to 1

#a good n will help you decide which kind of PDF equation to use
PDF = function(X,n,y.min = 0,y.max = 1){
  a = min(X)
  b = max(X)
  N = length(X)
  s = (b-a)/n # spacing of the sub-intervals
  x = rep(NA,n+1)
  x.mid = rep(NA,n)
  for(i in 1:(n+1)){
    x[i] = a + (i-1)*s
  }
  
  for(i in 1:n){
    x.mid[i] = (.5)*( x[i+1] + x[i] )
  }
  
  f = rep(NA,n)
  for(i in 1:n){
    f[i] = sum( X > x[i] & X < x[i+1] )/(N*s)
  }
  plot(x.mid,f,type = "l",ylim = c(y.min,y.max) )
}
x=runif(1e6,min=-2,max=3)
PDF(x,10,y.min=0,y.max=2)

x=runif(50000)
y=runif(50000)
s=x+y
PDF(s,10)

x=runif(1e16)
y=x*x
PDF(y,10)