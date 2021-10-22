#function you want to minimize
f=c(1.06,0.37,0.42,0.30,3.08)
#constraints in term of matrix
A=rbind(c(5.4,4,3.6,5.9,3.9),c(30,0,1,50,4),c(62,100,75,20,8),c(5,0,15,25,85),diag(5))
#vector of inequality direction
s=rep(">=",9)
#rhs
rhs=c(3,20,30,20,0,0,0,0,0)
lp("min",f,A,s,rhs)
lp("min",f,A,s,rhs)$solution

#a: price vector of food
#p.vec:protein vector
#c.vec:carbohydrates vector
#f.vec:fats vector
#p:the mininum protein requirements
#c:min carbohydrates requirements
#f: min fats requi
#cal.high: min cal requ
#cal.high:max cal requ
# Minimize the cost
calories=function(p,c,f){
  #Each gram of carbohydrate and protein yield 4 calories/gram. 
  #Each gram of fat yields 9 calories
  n=length(p)
  cal=rep(0,n)
  for(i in 1:n){
    cal[i]=4*p[i]+4*c[i]+9*f[i]
  }
  cal
}
pro=c(5,0,15,25,84)
carb=c(62,100,75,20,8)
fat=c(30,0,1,50,4)
calorie=calories(pro,carb,fat)

matrix_A=function(p.vec,c.vec,f.vec){
  n=length(p.vec)
  cal=calories(p.vec,c.vec,f.vec)
  B=rbind(p.vec,c.vec,f.vec,diag(n),cal,cal)
  B
}
B=matrix_A(pro,carb,fat)
optimize.food=function(a,p.vec,c.vec,f.vec,p,c,f,cal.low,cal.high){
  #a is function to minimize
  n=length(a)#number of items
  cal=calories(p.vec,c.vec,f.vec)
  B=matrix_A(p.vec,c.vec,f.vec)
  dir=c(rep(">=",3),rep(">=",n),">=","<=")
  rhs=c(c(p,c,f),rep(0,n),cal.low,cal.high)
  lp("min",a,B,dir,rhs)$solution
}
cost=c(1.06,0.37,0.42,0.30,3.08)
optimize.food(cost,pro,carb,fat,20,30,20,300,1000)