###########################################################################
# This file contains the code for the test suite specified in Qtns 6 - 8  #
###########################################################################

FileMM1 <- function(lambda, mu, D)
{
  total <- 0
  arrivee <- c()
  
  repeat{
    
    T <- rexp(1,lambda)
    total <- total + T
    
    if(total > D){
      break
    }
    
    arrivee <- c(arrivee, total)
  }
  
  depart <- c()
  prevDep <- 0
  
  for(a in arrivee)
  {
    
    T <- rexp(1,mu)
    nouvelDepart <- a + T
    
    if(nouvelDepart < prevDep){
      nouvelDepart <- prevDep + T
    }
    
    if(nouvelDepart > D){
      break
    }
    
    depart <- c(depart, nouvelDepart)
    prevDep <- nouvelDepart
    
  }
  
  return(
    list(arrivee=arrivee , depart=depart)
  )
}

evolutionFile <- function(arrivee, depart)
{
  a <- 1
  d <- 1
  
  N <- c(0)
  T <- c(0)
  
  repeat{
    if(arrivee[a] < depart[d]){
      N <- c(N, tail(N,1)+1)
      T <- c(T, arrivee[a])
      a <- a+1
    } else if(arrivee[a] > depart[d]){
      N <- c(N, tail(N,1)-1)
      T <- c(T, depart[d])
      d <- d+1
    } else {
      a <- a+1
      d <- d+1
    }
    
    if(a == length(arrivee) || d == length(depart)){
      break
    }
  }
  
  if(a == length(arrivee)){
    repeat{
      if(d == length(depart)){
        break
      }
      N <- c(N, tail(N,1)-1 )
      T <- c(T, depart[d] )
      d <- d+1
    }
  }
  
  if(d == length(depart)){
    
    repeat{
      if(a == length(arrivee)){
        break
      }
      
      N <- c(N, tail(N,1)+1 )
      T <- c(T, arrivee[a] )
      a <- a+1
    }
  }
  
  return(
    list(T=T, N=N)
  )
}

StatsMoyens <- function(arrivee, depart, lambda, mu)
{
  # E(N)
  alpha <- lambda/mu
  EN <- alpha/(1-alpha)
  
  if(alpha >=1) return(NULL)
  
  # E(W)
  W <- 0
  
  for(i in 1:length(depart))
  {
    W <- W + depart[i] - arrivee[i]
  }
  
  return(
    list(
      EW=W/length(depart), EN=EN
    )
  )
}
