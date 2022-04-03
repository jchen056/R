#HW8(2):Markove Chain: "symmetric random walk with a reflecting boundary"

N=5
X0=2#initial state, x0 is between 1 and N, an integer
x=rep(NA,1e5+1)#Markov chain with 10^5 steps
x[1]=X0
for(i in 2:(1e5+1)){
  #reflecting boundary
  if(x[i-1]==N){#you can exceed the max; markov reflects away from its boundary back down to N-1
    x[i]=N-1
  }
  else if(x[i-1]==1){#reflect from its boundary back up to 2
    x[i]=1+1
  }
  else{
    coin_flip=sample(c(1,-1))#1 means head and -1 means tail, fair coin
    if(coin_flip==1){#if head, go up
      x[i]=x[i-1]+1
    }
    else{#if tail, go down
      x[i]=x[i-1]-1
    }
  }
}
plot(x,type='l')
sum(x==1)
sum(x==2)
sum(x==3)
sum(x==4)
sum(x==5)

 
plot_markov=function(N,X0,n){
  x=rep(NA,n+1)#markov chain with n steps
  x[1]=X0
  for(i in 2:(n+1)){
    #reflecting boundary
    if(x[i-1]==N){#you can exceed the max; markov reflects away from its boundary back down to N-1
      x[i]=N-1
    }
    else if(x[i-1]==1){#reflect from its boundary back up to 2
      x[i]=1+1
    }
    else{
      coin_flip=sample(c(1,-1))#1 means head and -1 means tail, fair coin
      if(coin_flip==1){#if head, go up
        x[i]=x[i-1]+1
      }
      else{#if tail, go down
        x[i]=x[i-1]-1
      }
    }
  }
  plot(x,type='l')
  x}
y=plot_markov(100,50,1e6)

