#HW8(1):Multinomial distribution X->Multinomial(N,p)


#Generate multinomially distributed random number vectors 
X=rmultinom(1e3,24,prob=c(0.1,0.2,0.3,0.4) )
X[,1]#the first sample
Y=t(X)#do the transpose on X to make it easier for our use
#dmultinom(c(2,5,5,12),24, prob = c(.1,.2,.3,.4) )
L2=function(p){
  s=0
  for(i in 1:1e3){
   s=s+dmultinom(Y[i,],24,prob=c(p[1],p[2],p[3],p[4]),log=TRUE) }
  -s
}
optim(c(0.1,0.2,0.3,0.4),L2)