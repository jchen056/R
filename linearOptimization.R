#linear optimization
#maximize 6x1+8x2+5x3+9x4
#constraints 
#(1)x1,x2,x3,x4>=0
#(2)2x1+1x2+1x3+3x4<=5
#(3)1x1+1x2+1x3+2x4<=3

#install lpsolve
#let us specify the coefficient vector for the linear function
f=c(6,8,5,9)
#write constraints in <= form of a matirx
A=rbind(c(2,1,1,3),c(1,1,1,2),-diag(4))
#vector inequality directions
d=rep("<=",6)
#vector of right hand side
b=c(5,3,0,0,0,0)
#this will gives the maximum value of the linear function
lp("max",f,A,d,b);
lp("max",f,A,d,b)$solution

#--------install boot
simplex(f,A,b,maxi=TRUE)
#-------install pracma, linprog
linprog(f,A,b,maximize=TRUE)
