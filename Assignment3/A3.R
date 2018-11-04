#
# Returns the next number in the sequence,
# given the current number (seed), and 
# constants a, c, m.
#
lcg.next <- function(seed, a, c, m){
  ret <- ((a*seed)+c)
  ret <- ret%%m
  ret
}

#
# Returns a vector of n numbers based on
# the LCG algorithm, given the seed number, 
# and constants a, c, m.
#
lcg.generate <- function(n, seed, a, c, m){
  if(n > 0){
	v <- c(seed)
	for(i in 1:n){
		v <- append(v, lcg.next(v[i-1], a, c, m))
	}
	v
  }
  else{
	print(paste("Please use an n larger than 0"))
  }  
}
