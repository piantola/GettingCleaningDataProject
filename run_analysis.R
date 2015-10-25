## This script read data from unzipped file: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## This section read data from files to datasets

trainX <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainY <- read.table("./UCI HAR Dataset/train/y_train.txt")

testX <- read.table("./UCI HAR Dataset/test/X_test.txt")
testY <- read.table("./UCI HAR Dataset/test/y_test.txt")

subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

## 1. Merges the training and the test sets to create one dataset.

allX <- rbind(trainX,testX)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

colnames(allX) <- c(as.character(features[,2]))
meanX <- grep("mean()",colnames(allX),fixed=TRUE)
sdX <- grep("std()",colnames(allX),fixed=TRUE)
meanSd <- allX[,c(meanX,sdX)]

## 3. Uses descriptive activity names to name the activities in the data set.

allY <- rbind(trainY,testY)
allActivity <- cbind(allY,meanSd)
colnames(allActivity)[1] <- "Activity"

## 4. Appropriately labels the data set with descriptive variable names.

activityLabels[,2] <- as.character(activityLabels[,2])
for(k in 1:length(allActivity[,1]))
{
  allActivity[k,1] <- activityLabels[allActivity[k,1],2]
}

## 5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.

subjectAll <- rbind(subjectTrain,subjectTest)
all <- cbind(subjectAll,allActivity)
colnames(all)[1] <- "Subject"
tidyData <- aggregate( all[,3] ~ Subject+Activity, data = all, FUN= "mean" )
for(k in 4:ncol(all))
{
  tidyData[,k] <- aggregate( all[,k] ~ Subject+Activity, data = all, FUN= "mean" )[,3]
}
colnames(tidyData)[3:ncol(tidyData)] <- colnames(meanSd)

write.table(tidyData, file = "result.txt", row.name=FALSE)