setwd("UCI HAR Dataset")
#1 . Merges the train and the test data sets to create one data set.  
##1.a. READ Test Data into onedataset   

setwd("test")
x <-read.table("X_test.txt",header = FALSE)
activity <-read.table("Y_test.txt",header = FALSE)
sub <- read.table("subject_test.txt")
setwd("..")
onedataset <- data.frame(sub,activity,x)

##1.b. READ Train Data into onedataset  

setwd("train")
x <-read.table("X_train.txt",header = FALSE)
activity <-read.table("Y_train.txt",header = FALSE)
sub <- read.table("subject_train.txt")
setwd("..")
onedataset <- rbind(onedataset,data.frame(sub,activity,x))

##1.c. Set Column name for onedataset

temp <- read.table("features.txt")
column <- c("subject","activity")
for( v in temp[,2]) column <- c(column,v) # append features
colnames(onedataset) <- column
#1 is complete 

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
##2.a. column indices dealing with mean and std deviation indentified. Index of relevant columns finalised 

meanindex <- grep("mean()",colnames(onedataset),fixed = TRUE)
stdindex <- grep("std()",colnames(onedataset),fixed = TRUE)
index <- c(1,2,meanindex,stdindex)
##2.b Extracts relevant columns only.

onedataset <- onedataset[,index]
#2 is complete

#3.Uses descriptive activity names to name the activities in the data set
## Read the activity discription file and add a column to the dataset describing the activity

activity <- read.table("activity_labels.txt",col.names = c("activity","activitynames"))
onedataset <- merge(onedataset, activity, by.x = "activity",by.y = "activity", all = FALSE)

#3 is complete
#4 Appropriately labels the data set with descriptive variable names. Use the features_info

column <- colnames(onedataset)
f <- TRUE
##Descriptive t => time f =>frequency
for(i in 1:length(column)) 
  if(substr(column[i],1,1) == "t")
    column[i] = paste("time",substr(column[i],2,nchar(column[i])))
else if(substr(column[i],1,1) == "f")
  column[i] = paste("frequency",substr(column[i],2,nchar(column[i])))
## Descriptive Acc => accelerator Gyro => gyroscope
column <- sub("Acc","accelerator",column)
column <- sub("Gyro","gyroscope",column)
## Descriptive std
column <- sub("std","strandarddeviation",column)
## Remove space and symbols
column <- gsub(" ","",column)
column <- gsub("()", "",column)
column <- gsub("_","",column)
column <- gsub("-","",column)
column <- tolower(column)
colnames(onedataset) <- column

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##5.a define groupid as subject,activity

colc <- function (x,col1,col2){
  col <- character(nrow(x)) 
  for(i in 1:nrow(x)) col[i] <- paste(x[i,col1],x[i,col2])
  return(col)
}
onedataset$groupid <- colc(onedataset,1,2)


##5.b New tidy data non numeric valuers determined
sub <- unique(onedataset$subject)
act <- unique(onedataset$activity)
newtidydata <- data.frame(expand.grid(sub,act))
colnames(newtidydata)<- c("subject","activity")
newtidydata <- merge(newtidydata,activity, by.x = "activity",by.y = "activity", all = FALSE)
newtidydata$groupid <- colc(newtidydata,1,2) 



##5.c Avg of every variable is calculated

for(i in 3:68){
  col <- tapply(onedataset[,i],onedataset$groupid,mean)
  coldf <- data.frame(dimnames(col),col)
  colnames(coldf) <- c("groupid",colnames(onedataset)[i]) 
  newtidydata <- merge(newtidydata,coldf)
}
write.table(newtidydata,"USIHARDataset.txt",sep = "\t",row.names = FALSE)
#5 is complete

setwd("..")  