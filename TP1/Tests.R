###########################################################################
# This file contains the code for the test suite specified in Qtns 3 - 5, #
# as well as functions that run those tests on given data.                #
###########################################################################

# Monobit Frequency test
Frequency <- function(x, nb)
{
  s <- 0
  totalN <- 0
  
  for (i in x){
    bin <- rev(binary(i))
    totalN <- totalN + nb
    
    for (n in 1:nb) {
      if (bin[n] == 1) s <- s+1
      else             s <- s-1
    }
  }
  
  s_obs <- abs(s) / sqrt(totalN)
  Pvaleur = 2 * (1 - pnorm(s_obs))
  
  return(Pvaleur)
}

# Runs test
Runs <- function(x, nb)
{
  # pre-test
  uns    <- 0
  totalN <- 0
  
  for(i in x)
  {
    totalN   <- totalN + nb
    
    bin <- rev(binary(i))
    
    for(n in 1:nb)
    {
      if(bin[n] == 1) uns <- uns + 1
    }
  }
  
  pi  <- uns/totalN
  tau <- 2/sqrt(totalN)
  
  if(abs(pi - 0.5) >= tau) return(0.0)
  # Vn(obs)
  V <- 1
  
  prevLastBit <- rev(binary(x[1]))[nb]
  
  for(i in x)
  {
    bin <- rev(binary(i))
    
    if(bin[1] != prevLastBit) V <- V + 1
    
    for(n in 1:(nb-1))
    {
      if(bin[n] != bin[n+1]) V <- V + 1
    }
    
    prevLastBit <- bin[nb]
    
  }
  
  pi2pi <- 2*pi*(1-pi)
  Pvaleur <- 2*(1 - pnorm( abs(V - totalN*pi2pi) / (sqrt(totalN)*pi2pi) ))
  
  return(Pvaleur)
}

# These functions run the specified test on a given function 'func'
# that generates numbers of size 'nb' bits.
# They return the average pValue after running the test 'numTest' times.

RunFrequencyTest <- function(func, numTests, nb)
{
  seeds <- sample.int(100000, numTests)
  avgPvalue <- 0
  for(i in 1:length(seeds)) {
    avgPvalue <- avgPvalue + Frequency(func(1000, seeds[i]), nb)
  }
  avgPvalue <- avgPvalue / numTests
}

RunRunsTest <- function(func, numTests, nb)
{
  seeds <- sample.int(100000, numTests)
  avgPvalue <- 0
  passCount <- 0
  
  for(i in 1:numTests) {
    # La plage des valeurs de randu: {0, ..., 2^31-1}
    # 2^31-1 s'écrit sur 31 bits
    Pvalue <- Runs(func(1000, seeds[i]), nb)
    avgPvalue <- avgPvalue + Pvalue
    if(Pvalue > 0.01){
      passCount <- passCount +1
    }
  }
  avgPvalue <- avgPvalue / numTests
  
  return(
    list(avgPvalue=avgPvalue, passRate=passCount/numTests)
  )
}

RunOrderTest <- function(func, numTests)
{
  seeds <- sample.int(100000, numTests)
  avgPvalue <- 0
  for(i in 1:length(seeds)) {
    u <- func(1000, seeds[i])
    u <- as.vector(u)
    avgPvalue <- avgPvalue + order.test(u, d=4, echo=FALSE)$p.value
  }
  avgPvalue <- avgPvalue / numTests
}

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
  
  # E(W)
  W <- 0
  
  for(i in 1:length(depart))
  {
    W <- W + depart[i] - arrivee[i]
  }
  
  # E(N)
  alpha <- lambda/mu
  EN <- alpha/(1-alpha)
  
  return(
    list(
      EW=W/length(depart), EN=EN
    )
  )
}
