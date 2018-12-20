# Evan Switzer
# A4
# 0971076
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
#
# q.sim assumes that at 0 there is still work needed to be done for the customers
# When a new worker arrives it starts working imidiately
# After the customer is finished the next customer starts the time after
#

q.sim <- function(lambda, mu, w = 1, T = 60){
	#Clock
	t <- 0
	
	#Arrival Amount
	arrivals <- 0
	
	#Waiting line
	line <- c()
	
	#Workers busy
	busyWorkers <- 0
	
	workerTimer <- c(rep(-1,w))
	
	while(t <= T){
		cat("[ t =",t,"]\n")
		
		#arrivals
		arrivals <- round(rpois(1, lambda),digits=0)
		cat(arrivals," customers arrived\n")
		
		if(t == 0){
			line <- c(arrivals)
		}
		else{
			line <- c(line,arrivals+line[length(line)])
		}
		
		cat(line[length(line)]," in line\n")
		
		for(i in 1:w){
			
			cat("[Worker ",i,"]\n")
			if(workerTimer[i] < 0){
				if(line[length(line)] > busyWorkers){
					workerTimer[i] <- round(rexp(1,mu),digits=0)
					cat("\tMay I help you? Time to completion: ",workerTimer[i],"\n")
					busyWorkers <- busyWorkers + 1
					line[length(line)] <- line[length(line)] - 1
				}
				
			}
			
			workerTimer[i] <- workerTimer[i] - 1
			
			if(workerTimer[i] == -1){
				busyWorkers <- busyWorkers - 1
				cat("\tThank you, come again\n")
			}
			else if(workerTimer[i] >= 0){
				cat("\tWorking...\n")
				cat("\tTime left: ",workerTimer[i],"\n")
			}
			else{
				cat("\tNobody to serve\n")
			}
			
		}
		
		t <- t + 1
	}


	line
}

q.sim.second <- function(custPerHour, minsPerCust, w=1){
	#Normalize to a one-hour simulation by-the-second
	lambda = custPerHour/3600
	mu = 1/(minsPerCust * 60)
	q.sim(lambda, mu, w, 3600)
}

q.sim.minute <- function(custPerHour, minsPerCust, w=1){
	#normalize to a one-hour simulation by-the-minute
	lambda = custPerHour/60
	mu = 1/(minsPerCust)
	q.sim(lambda, mu, w, 60)
}

q.plot <- function(line1, line2){
	par(mfrow=c(1,2))
	plot(line1,type="l")
	plot(line2,type="l")
	
}
