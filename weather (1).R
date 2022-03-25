A = rbind( c(.7,.2,.1), c(.3,.4,.3), c(.2,.5,.3) )
day = c("sun","clouds","rain")
rownames(A) = day
colnames(A) = day

x = 1
for(i in 1:100){
  for(j in 1:3){
    if(x[i] == j){#what is the current state: sun 1; clouds 2; rain: 3
      x = c(x,sample(c(1,2,3),size=1,prob=A[j,]))
      print(x)
    }
  }
}
plot(x)

sum(x==1)#number of sunny days
sum(x==2)#number of cloudy days
sum(x==3)#number of rainy days