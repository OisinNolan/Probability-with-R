VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
      numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}


MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}


# Question 1
congruenceLineaire <- function(a, b, m, k, graine)
{
  v <- c()
  v[1] <- (a * graine + b) %% m 
  
  for (i in 2:k){
    v[i] <-  (a * v[i-1] + b) %% m  
  }
  
  return(v)
}

StandardMinimal <- function(k, graine)
{
  return(
    congruenceLineaire(a=16807, b=0, m=2^31-1, k=k, graine=graine)
  )
}

randu <- function(k, graine)
{
  return(
    congruenceLineaire(a=65539, b=0, m=2^31, k=k, graine=graine)
  )
}

# Question 3

binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}
