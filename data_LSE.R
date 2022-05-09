#squat score
s = c( 1036 , 942 , 903 , 1014, 925, 833, 903, 881, 837, 766, 705, 799, 804,
       826, 837, 782, 760, 892, 793, 716 )
#the bigger the squat, the smaller deadlift for people of extreme size

#bench score
b = c( 534, 556, 562, 595, 584, 557, 512, 507, 462, 551, 567, 573, 518, 462, 
       518, 611, 529, 468, 496, 490 )

#deadlift score
d = c( 832, 804, 832, 672, 672, 766, 738, 738, 815, 782, 826, 727, 771, 793,
       727, 677, 782, 705, 777, 848 )

t = s + b + d 
plot(d,b)


A=cbind(d,rep(1,20))
#to find xmin: (t(A)*A)^-1*(t(A)*b
B=solve(t(A)%*%A)%*%(t(A)%*%b)
curve(B[1]*x+B[2],add=TRUE,col="red")

#relationship between s,d, and t--------------
A1=cbind(s,b,1)
solve(t(A1)%*%A1)%*%(t(A1)%*%t)
#total=0.8712962*squat+0.5586698*bench+1108.3546617