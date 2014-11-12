##Part 1
#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
# The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that 'monitors' particulate matter data from the directory specified in the 'directory' 
## argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA


pollutantmean<-function(directory,pollutant,id=1:332){
##Get the list of filenames
filenames<-list.files(directory, full.names=TRUE)
	
#Initializing the dataframe to store the values
	
	val<-data.frame()

#Loop over the ID's

for(i in id){

val<-rbind(val,read.csv(filenames[i]))

}
d<-val[,pollutant]
z<-mean(d,na.rm=TRUE)
z
}
