#
# Simulates a queuing system with:
# - Poisson arrivals at a rate of lambda customers per time unit,
# - Exponential service time at a rate of mu customers per time unit,
# - w number of workers servicing the line,
# - over a duration of T units of time.
#
# Returns a vector containing T+1 integers corresponding to 
# the size of the waiting line at each unit of time from 0 to T
#

#lambda is arrival rate (customers per time)
#mu completion rate (time per customer)
q.sim <- function(lambda, mu, w = 1, T = 60){

  # Clock
  t <- 0
  a <-0
  b <-0
  c <- c()
  lineVector <- c()
  
  #change lambda and mu into minutes
  #lambda <- lambda/60 #amount of customers per hour to amount per minute
  print(paste(lambda))
  mu <- 1/mu 
  #DOn't think I need to flip it which means I need to fix the wrapper functions
  
  # Waiting Line
  line <- c()
  removal <- c()
  tempW <- 0
  i <- 1
  
  while(t <= T){
    # Make magic happen...
	#set.seed(12345)
	arrivalCount <- rpois(1, lambda)
	print(arrivalCount)
	a <- length(line)
	if(arrivalCount > 0){
		for(i in 1:arrivalCount){
		#here is where I put service time instead of 0
		#q.cServe / rexp(1,1/mu)
		#set.seed(12345)
		line <- append(line, round(rexp(1,mu),digits=0))
		
		}
	}
	
	#print(paste("Done Servicing!"))
	
	removal <- c()
	tempW <- w
	i <- 1
	while(i <= tempW){
		if(length(line) >= i){

			if(line[i] <= 0){
			#print(paste(tempW))
				tempW <- tempW + 1
				#print(paste(tempW))
			}
			line[i] <- line[i] - 1
			if(line[i] <= 0){
				#print(paste("Please use an n larger than 0"))
				#line <- line[-i]
				#i <- i - 1
				removal <- append(removal,i)
			}
		}
		i <- i + 1
	}
	if(length(removal) > 0){
		line <- line[-removal]
	}
	
	
	#print(paste(""))
	#print(paste(""))
	#print(paste(""))
	#print(paste(""))
	#print(paste(""))
	#print(line)
	print(paste("Length: "))
	print(length(line))
	lineVector <- append(lineVector,length(line))
	
    #b <- length(line)
	#c <- append(c,b-a)
    # Advance clock by 1 unit
    t <- t + 1
  }
  #median(c)
  #c
  #line
  lineVector
}

#taken from slides
q.sim.humanized <- function(custPerHour, minsPerCust, w=1){
	#Normalize to a one-hour simulation by-the-second
	lambda = custPerHour/3600
	mu = (minsPerCust * 60)
	ret<-q.sim(lambda, mu, w, 3600)
	ret
}

#take from slides
q.sim.hMinutes <- function(custPerHour, minsPerCust, w=1){
	#Normalize to a one-hour simulation by-the-minute
	lambda = custPerHour/60
	mu = (minsPerCust)
	ret<-q.sim(lambda, mu, w, 60)
	ret
}

ezRun <- function(custPerHour, minsPerCust, w=1){
	plot(q.sim.hMinutes(custPerHour, minsPerCust, w))
}

slideTest <- function(){
	seed <- 31243
	set.seed(seed)
	line1 <- q.sim.hMinutes(custPerHour=60, minsPerCust=1)
	set.seed(seed)
	line2 <- q.sim.humanized(custPerHour=60, minsPerCust=1)
	
	par(mfrow=c(1,2))
	plot(line1)
	plot(line2)
}

sample <- function(){
	set.seed(12345)
	print(rexp(1,50))
	print(rpois(1, 50))
	print(rexp(1,50))
	print(rexp(1,50))
	print(rpois(1, 50))
	print(rpois(1, 50))
	
	
	set.seed(12345)
	print(rexp(1,50))
	set.seed(12345)
	print(rexp(1,50))
	set.seed(12345)
	print(rexp(1,50))
	set.seed(12345)
	print(rpois(1, 50))
	set.seed(12345)
	print(rpois(1, 50))
	set.seed(12345)
	print(rpois(1, 50))
	
}


