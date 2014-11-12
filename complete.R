#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file.
#The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.

complete<-function(directory,id=1:332){
##Get the list of filenames
filenames<-list.files(directory, full.names=TRUE)
	
#Initializing the dataframe to store the values
	ids=vector()
	counts=vector()
#Loop over the ID's

for(i in id){

ids<-c(ids,i)

val<-rbind(read.csv(filenames[i]))
completeCases<-val[complete.cases(val),]
counts<-c(counts,nrow(completeCases))

}


data.frame(id=ids,nobs=counts)


}
