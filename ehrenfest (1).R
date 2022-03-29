A = rbind( c(0,1,0,0,0,0) , 
           c(1/5,0,4/5,0,0,0),
           c(0,2/5,0,3/5,0,0),
           c(0,0,3/5,0,2/5,0),
           c(0,0,0,4/5,0,1/5),
           c(0,0,0,0,1,0)
           )

number = c("0","1","2","3","4","5")
#there are two boxes, box1 and box2; box1 is of our interest
#pick a particle(only one), move it to other box

rownames(A) = number
colnames(A) = number

x = 1
for(i in 1:1000){
  for(j in 1:6){
    if(x[i] == j){#what is the current state
      x = c(x,sample(1:6,size=1,prob=A[j,]))
    }
  }
}

plot(x)

#raise the matrix to a power
#p^l is a matrix of transitioning between states when your path has length l
A%*%A
(A%*%A)%*%A

B=A
for(n in 1:20){
  B=B%*%A
}
C=A
for(i in 1:21){
  C=C%*%A
}
#B,C even and odd; we care about the average bet the two
#period of markov chain, has a period of 2 because it is oscillating btw two states
D=(B+C)/2
pi1=eigen(t(A))$vector[,2]#we only care the sec col b/c it has an eigenvalue of 1
pi1=pi1/sum(pi1)#normalized pi1
