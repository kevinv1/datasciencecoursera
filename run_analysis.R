if(!file.exists("./project")){dir.create("./project")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./project/galaxy.zip")
setwd("~/2014-2015/Coursera/Getting and Cleaning Data/project")
unzip(zipfile="./galaxy.zip")
list.dirs()
allfiles <- list.files(pattern = "txt",all.files = T, full.names = T, recursive = T, include.dirs = T)
#subsetfiles <- list.files(pattern = "^(body|total)(.*)txt",all.files = T, full.names = T, recursive = T, include.dirs = T)

trainset <- read.table(allfiles[27])
trainlabels <- read.table(allfiles[28])
trainsubject <- read.table(allfiles[26])

testset <- read.table(allfiles[15])
testlabels <- read.table(allfiles[16])
testsubject <- read.table(allfiles[14])

features <- read.table(allfiles[2])

#Add names to the columns of sets
names(testset) <- features[,2]
names(trainset) <- features[,2]

#label names that go with label numbers
activitylabels <- read.table(allfiles[1], stringsAsFactors=FALSE)

#add label names to test
testlabels$activity <- ""
for(i in seq_along(testlabels$V1)) {
    testlabels[i, "activity"] = activitylabels[testlabels[i,"V1"], "V2"]
}

#add label names to train
trainlabels$activity <- ""
for(i in seq_along(trainlabels$V1)) {
    trainlabels[i, "activity"] = activitylabels[trainlabels[i,"V1"], "V2"]
}

#merge data with subject
testset$subject <- testsubject[,1] #if the last part is not added, we have a list inside df for each element which causes error at rbind
trainset$subject <- trainsubject[,1] #if the last part is not added, we have a list inside df for each element which causes error at rbind

#merge data with activity
testset$activity <- testlabels$activity
trainset$activity <- trainlabels$activity

#concatenate both data sets into one
data <- rbind(trainset, testset) #this gives error because there are dfs inside the df.

#subset only mean and standard deviation columns from data
data2 <- data[,grep("mean|std|activity|subject", colnames(data), value=T)]



#Create tidy data set with averages of each variable for each activity and each subject

# Create factors that are a combination of activity and subject
#tidy$act_sub <- transform(tidy,act_sub = paste0(subject," ", activity)) #this is automatically a factor

library(plyr)
library(dplyr)
#tidy <- mutate(data2, act_sub = paste0(subject," ", activity))
#tidy$act_sub <- factor(tidy$act_sub)
tidy <- data2

#ddply(tidy, .(act_sub), summarise, mean.count = mean(tBodyAcc-mean()-X)) #this would work if the col names didn't have hyphens and ()
#Make labels more descriptive: 
#change names
tempname <- gsub("\\(\\)","",names(tidy))
names(tidy) <- gsub("-","_",tempname)

tidy <- group_by(tidy,subject,activity)
tidy <- summarise_each(tidy, funs(mean))
tidy <- as.data.frame(tidy)
write.table(tidy,file="tidydata.txt", row.name=F)

#To read data back
x <- read.table("tidydata.txt", stringsAsFactors=FALSE, header=T)

#tidy is now back to a normal data frame after dplyr manipulations.
#See following link for other ways to do this using apply instead of dplyr
#http://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/
#http://www.r-bloggers.com/using-r-quickly-calculating-summary-statistics-from-a-data-frame/
#http://stats.stackexchange.com/questions/8225/how-to-summarize-data-by-group-in-r

