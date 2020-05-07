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
  for(i in 1:length(seeds)) {
    # La plage des valeurs de randu: {0, ..., 2^31-1}
    # 2^31-1 s'écrit sur 31 bits
    avgPvalue <- avgPvalue + Runs(func(1000, seeds[i]), nb)
  }
  avgPvalue <- avgPvalue / numTests
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
