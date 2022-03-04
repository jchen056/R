#hw5: question1
X1=runif(100000,min=0,max=1)
X2=runif(100000,min=0,max=1)
Z=X1*X2#product of two indepedent random variables
PDF(Z,20)
curve(log(1/x),col="red",add=TRUE)

#cumulative probability
Z=Z[Z>0 & Z<0.5]
c=length(Z)#get the number of elements in Z
print("P(0<Z<1/2):")
print(c/100000)


#hw #2: CDF.norm: calculate the cumulative prob up to a value t using n iterations
oddp=function(n){
  x=rep(NA,n)
  x[1]=1
  if(n>1){
    for(i in 2:n){
      x[i]=(2*i-1)*x[i-1]}
    x}
  else{
    x
  }
}
expp=function(t,n){
  x=rep(NA,n)
  for(i in 1:n){
    x[i]=t^((2*i-1))
  }
  x
}
#CDF.norm
CDF.norm=function(t,n){
  x=oddp(n)
  y=expp(t,n)
  z=0.5+1/(sqrt(2*pi))*exp(-t*t/2)*sum(y/x)
  z
}
print(CDF.norm(1,5))
pnorm(1)