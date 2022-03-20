
#diff in height between two groups------------------------------
height=bb$height
hitpitind=bb$hitpitind

#hitter and pitcher height vectors
hitht=height[hitpitind==1]
pitht=height[hitpitind==0]

x=mean(hitht)
s1=var(hitht)
n1=length(hitht)

y=mean(pitht)
s2=var(pitht)
n2=length(pitht)

#find t critical value: qt
qt(p=0.05,df=19)#when alpha=0.05 and dof=19
qt(p=0.025,df=19)#when alpha=0.025 and dof=19
qt(p=0.005,df=19)#when alpha=0.005 and dof=19

#find the z-score using qnorm
qnorm(0.95)

#to see whether there is a diff between two groups
#if the pvalue is small<0.05(), reject the null hypothesis of no diff between two groups
t.test(pitht,hitht,var.equal=T,conf.level = 0.95)

#diff in weight between two gps---------------------------------
weight=bb$weight
W2=weight[hitpitind==1]#hitters
W1=weight[hitpitind==0]#pitchers
av1=mean(W1)
ss1=var(W1)
n11=length(W1)

av2=mean(W2)
ss2=var(W2)
n12=length(W2)

#assume x,y normally distributed
qt(p=0.05,df=57)
t.test(W1,W2,var.equal=T,conf.level = 0.95)