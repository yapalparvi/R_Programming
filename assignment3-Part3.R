rankall <-function(reason, num="best"){
      ### Reading outcome date
      ###Data is being saved as character (as per exercise instructions)
      outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      if((reason %in% c("heart attack", "heart failure", "pneumonia"))==FALSE){
            stop("invalid outcome")
      }
      ###Now checking the hospital
      states <- unique(outcome$State) 
      states <-states[! states %in% c("DC", "GU", "MP", "PR", "VI")]
      if((reason == "heart attack")== TRUE)
      { best <- data.frame()
        for(st in states){
              
              stateoutcome<-subset(outcome, outcome$State==st & !(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=="Not Available"))
              
              orderedstateoutcome<-stateoutcome[order(as.numeric(stateoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), stateoutcome$Hospital.Name),]
                length<-length(as.numeric(stateoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
               if(num == "best"){num <- 1}
                   if(num== "worst"){num <- length}
              # orderedstateoutcome$Hospital.Name[num]
              #best <-rbind(best, c(num, st))
              best <- rbind(best, data.frame(orderedstateoutcome$Hospital.Name[num], st))
        } 
        # num <-numfirm
        
        
      }
      if((reason == "heart failure")== TRUE)
      { best <- data.frame()
        for(st in states){
              
              stateoutcome<-subset(outcome, outcome$State==st & !(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=="Not Available"))
              
              orderedstateoutcome<-stateoutcome[order(as.numeric(stateoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), stateoutcome$Hospital.Name),]
               length<-length(as.numeric(stateoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
               if(num == "best"){num <- 1}
                  if(num== "worst"){num <- length}
              # orderedstateoutcome$Hospital.Name[num]
              #best <-rbind(best, c(num, st))
              best <- rbind(best, data.frame(orderedstateoutcome$Hospital.Name[num], st))
        } 
        # num <-numfirm
        
        
      }
      if((reason == "pneumonia")== TRUE)
      { best <- data.frame()
        for(st in states){
              
              stateoutcome<-subset(outcome, outcome$State==st & !(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=="Not Available"))
              
              orderedstateoutcome<-stateoutcome[order(as.numeric(stateoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), stateoutcome$Hospital.Name),]
                length<-length(as.numeric(stateoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
               if(num == "best"){num <- 1}
                    if(num== "worst"){num <- length}
              # orderedstateoutcome$Hospital.Name[num]
              #best <-rbind(best, c(num, st))
              best <- rbind(best, data.frame(orderedstateoutcome$Hospital.Name[num], st))
       
        } 
        # num <-numfirm
       # best<-rbind(best, data.frame(length))
        
}
      
colnames(best)<-c("hospital", "state")
 best
 
}
