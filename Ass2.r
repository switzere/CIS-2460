#
# Monty Hall Play Function
#
# Accepts a 'switch' parameter to indicate 
# whether to switch doors or stay. (FALSE by default)
#
# Returns TRUE if outcome is a win, FALSE otherwise.
#
montyHall.play <- function(switch=FALSE){
  doors <- sample(c('Car', 'Goat', 'Goat'))
  choices <- 1:3
  
  choice <- round(runif(1,1,3))
  remaining <- choices[-choice]
  doors2 <- doors[remaining]
  i <- match('Goat', doors2)
  doors3 <- doors2[-i]
  final <- doors3[1]
  
  if(switch == TRUE){
    win <- final == 'Car'
  }
  else{
    win <- final == 'Goat'
  }
  
  win
}

run100<-function(fC = 0){	
	money<-0
	
	for(i in 1:100){
		money<-money+functionChooser(fC)
	}
	money<-money/100
	
	money
}

functionChooser<-function(fC = 0){

	money<-0

	if(fC == 0){
		print(paste("Please choose a correct choice 1-8"))
	}
	else if(fC == 1){
		for(i in 1:300){
			money<-money+moneyHall(TRUE)
		}
	}
	else if(fC == 2){
		for(i in 1:300){
			money<-money+moneyHall(FALSE)
		}
	}
	else if(fC == 3){
		tF<-TRUE
		moneyChange<-0
		for(i in 1:300){
			if(tF == TRUE){
				moneyChange<-moneyHall(tF)
				money<-money+moneyChange
			}
			else if(tF == FALSE){
				moneyChange<-moneyHall(tF)
				money<-money+moneyChange
			}
			if(moneyChange == -5){
				tF<-!tF
				#if(tF == FALSE){
				#	tF<-TRUE
				#}
				#else if(tF == TRUE){
				#	tF<-FALSE
				#}
			}
		}
	}
	else if(fC == 4){
		tF<-FALSE
		moneyChange<-0
		for(i in 1:300){
			if(tF == TRUE){
				moneyChange<-moneyHall(tF)
				money<-money+moneyChange
			}
			else if(tF == FALSE){
				moneyChange<-moneyHall(tF)
				money<-money+moneyChange
			}
			if(moneyChange == -5){
				tF<-!tF
				#if(tF == FALSE){
				#	tF<-TRUE
				#}
				#else if(tF == TRUE){
				#	tF<-FALSE
				#}
			}
		}
	}
	else if(fC == 5){
		multiplier<-0
		moneyChange<-0
		for(i in 1:300){
			moneyChange<-(moneyHall(TRUE)*(1.2^multiplier))
			money<-money+moneyChange
			if(moneyChange < 0){
				multiplier<-0
			}
			else if(moneyChange > 0){
				multiplier<-multiplier+1
			}
		}	
	}
	else if(fC == 6){
		multiplier<-0
		moneyChange<-0
		for(i in 1:300){
			moneyChange<-(moneyHall(FALSE)*(1.2^multiplier))
			money<-money+moneyChange
			if(moneyChange < 0){
				multiplier<-0
			}
			else if(moneyChange > 0){
				multiplier<-multiplier+1
			}
		}	
	}
	else if(fC == 7){
		multiplier<-0
		moneyChange<-0
		for(i in 1:300){
			moneyChange<-moneyHall(TRUE)
			if(moneyChange == 5){
				multiplier<-multiplier+1
				money<-money+(moneyChange*multiplier)
			}
			else if(moneyChange == -5){
				multiplier<-0
				money<-money+moneyChange#Are they supposed to lose money here?
			}
		}
	}
	else if(fC == 8){
		multiplier<-0
		moneyChange<-0
		for(i in 1:300){
			moneyChange<-moneyHall(FALSE)
			if(moneyChange == 5){
				multiplier<-multiplier+1
				money<-money+(moneyChange*multiplier)
			}
			else if(moneyChange == -5){
				multiplier<-0
				money<-money+moneyChange#Are they supposed to lose money here?
			}
		}
	}
	money
}

moneyHall<-function(switch=FALSE){
	money<-0
	result<-FALSE
	result<-montyHall.play(switch)
	if(result == TRUE){
		money<-money+5
	}
	else if(result == FALSE){
		money<-money-5
	}
	money	
}
