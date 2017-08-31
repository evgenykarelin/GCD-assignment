library(data.table)
library(plyr)
library(dplyr)

#Read the data
activities<-fread("activity_labels.txt")
features<-fread("features.txt")

xtrain<-fread("./train/X_train.txt")
subtrain<-fread("./train/subject_train.txt")
ytrain<-fread("./train/y_train.txt")

xtest<-fread("./test/X_test.txt")
subtest<-fread("./test/subject_test.txt")
ytest<-fread("./test/y_test.txt")

#Rename variables and merging data
names(subtrain)[1]<-"subject"
names(subtest)[1]<-"subject"
names(ytest)[1]<-"activity"
names(ytrain)[1]<-"activity"

bindtest<-cbind(subtest, ytest, xtest)
bindtrain<-cbind(subtrain, ytrain, xtrain)
mdata<-rbind(bindtest,bindtrain)

varnames<-c("subject","activity",unlist(features[,2]))
names(mdata)<-varnames

#Subset the columns with measurements of mean and std 
mdata<-as.data.frame(mdata)
subcols<-grep("mean|std", varnames)
subdata<-mdata[,c(1,2, subcols)]

#Assign descriptive activity names to the activities in the data set
activitynames<-unlist(activities[,2])
subdata$activity<-sapply(subdata$activity,f<-function(x){ activitynames[x]})

#Rename columns according to tidy data principles 
names(subdata)<-tolower(names(subdata))
names(subdata)<-gsub("-",".",names(subdata))
names(subdata)<-gsub("[()]","",names(subdata))

#Calculate averages for every subject and activity
subdata<-as.data.table(subdata)
tidydata<-subdata[,lapply(.SD, mean),by=list(subject, activity)]
tidydata<-tidydata[order(subject)]

#Write result to a file
write.table(tidydata, "tidydata.txt", row.names = FALSE, quote = FALSE)