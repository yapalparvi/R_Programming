#Write a function that takes a directory of data files and a threshold for complete cases 
#and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases
#(on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that
##meet the threshold requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.


corr<-function(directory,threshold=0){
	
	completes<-complete(directory,1:332)
	completes<-subset(completes,nobs > threshold)
	
	correlations<-vector()
##Get the list of filenames
filenames<-list.files(directory, full.names=TRUE)
	

#Loop over the ID's

for(i in completes$id){

##ids<-c(ids,i)

val<-rbind(read.csv(filenames[i]))
completeCases<-val[complete.cases(val),]
##counts<-c(counts,nrow(completeCases))
count<-nrow(completeCases)

if( count >= threshold ) {
correlations <- c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
}

}


correlations


}


