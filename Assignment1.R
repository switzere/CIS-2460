my.summary<-function(data){
	total<-sum(data)
	amount<-length(data)
	
	sortedList<-sort(data,decreasing=FALSE)
	
	minVar<-sortedList[1]
	maxVar<-sortedList[amount]
	
	
	if((amount %% 2) == 0){
		medVar<-(sortedList[amount/2]+sortedList[(amount/2)+1])/2
	}
	else{
		medVar<-sortedList[(amount/2)+1]
	}
	meanVar<-total/amount
	
	
	if(amount == 1){
		q1Var<-sortedList[1]
		q3Var<-sortedList[1]
	}
	else if((amount %% 2) == 0){
		if((amount %% 4) == 0){#numbers like 8, 12, 16 <- even when divided in half
			q1Var<-(sortedList[amount/4]+sortedList[(amount/4)+1])/2
			q3Var<-(sortedList[amount-(amount/4)]+sortedList[amount-(amount/4)+1])/2
		}
		else{#numbers like 10, 14, 18 <- odd when divided in half
			q1Var<-sortedList[(amount/4)+1]
			q3Var<-sortedList[amount-(amount/4)+1]
		}
	}
	else{#odds seem to work now
		if(((amount-1) %% 4) == 0){#numbers like 9, 13, 17
			q1Var<-(sortedList[(amount/4)]+sortedList[(amount/4)+1])/2
			q3Var<-(sortedList[amount-(amount/4)+2]+sortedList[amount-(amount/4)+1])/2
		}
		else{#numbers like 7, 11, 15
			q1Var<-sortedList[(amount/4)+1]
			q3Var<-sortedList[amount-(amount/4)+1]
		}
		
	}
	
	
	
	
	c(Min=minVar, Q1=q1Var, Median=medVar, Mean=meanVar, Q3=q3Var, Max=maxVar)
}