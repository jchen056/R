#hw1
# S=rep(NA,11)
# #i=0, index 1
# S[1]=sin(0*0)*cos(0*0)
# #i=1
# S[2]=sin(1)*cos(0)+sin(1)*cos(1)
# #i=2 index 3
# S[3]=sin(4)*cos(0)+sin(4)*cos(1)+sin(4)*cos(4)
SS=NULL
for(i in 0:10){
  S=0
  for(j in 0:i){
    S=S+sin(i^2)*cos(j^2)
  }
  SS=c(SS,S)
}
SS
sum(SS)


#question: a function called ID that takes a vector as input
#and output a "anti-diagnal matrix"
#anti-diagonal matrix goes from the upper-right to the lower-left
ID=function(x){
  n=length(x)
  M=matrix(0,nrow=n,ncol=n)
  for(i in 1:n){
      M[i,n-i+1]=x[i]
  }
  M
}
x=c(4,5,6,7,8)
ID(x)
  