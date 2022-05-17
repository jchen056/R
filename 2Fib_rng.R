#fibonacci-style rng
x=seq(from=1,to=10,by=1)
#5000 terms of fibonacci: having 10 already
for(i in(11:5000)){
  x[i]=(x[i-3]*x[i-5])%%1093
}
x=x/1093
mean(x)
var(x)