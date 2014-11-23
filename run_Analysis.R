library(plyr)
library(reshape2)

# Refer to the ReadMe.md file for descriptive details regarding the code.

setwd("/Volumes/Shikhar's HardDisk 1/Shikhar/Coursera/Data Science /Getting and Cleaning Data /data/UCI HAR Dataset")

# Mergeing the training and the test sets to create one data set.

widths <- rep(c(16),561)
train_data <- read.fwf("./train/X_train.txt",widths,header=FALSE,sep="\t",skip=0,buffersize=15)
test_data <- read.fwf("./test/X_test.txt",widths,header=FALSE,sep="\t",skip=0,buffersize=15)
test_activity <- read.fwf("./test/Y_test.txt",c(5),header=FALSE,sep="\t",skip=0,buffersize=15)
train_activity <- read.fwf("./train/Y_train.txt",c(5),header=FALSE,sep="\t",skip=0,buffersize=15)
test_subject <- read.fwf("./test/subject_test.tx",c(5),header=FALSE,sep="\t",skip=0,buffersize=15)
train_subject <- read.fwf("./train/subject_train.txt",c(5),header=FALSE,sep="\t",skip=0,buffersize=15)
names(train_subject) <- c("Subject")
names(test_subject) <- c("Subject")
names(train_activity) <- c("Activity")
names(test_activity) <- c("Activity")
final_train_data <- cbind(train_data,train_activity,train_subject)
final_test_data <- cbind(test_data,test_activity,test_subject)
final_data <- rbind(final_train_data,final_test_data)

# Extracts only the measurements on the mean and standard deviation for each measurement. 

feature_data3 <- read.fwf("features.txt",c(3,100),header=FALSE,sep="\t",buffersize=15,n=462,skip=99)
feature_data2 <- read.fwf("features.txt",c(2,100),header=FALSE,sep="\t",buffersize=15,n=90,skip=9)
feature_data1 <- read.fwf("features.txt",c(1,30),header=FALSE,sep="\t",skip=0,buffersize=15,n=9)
feature_data <- rbind(feature_data1,feature_data2,feature_data3)
mean_values<- unlist(apply(feature_data,1,function(x) if(length(grep("mean+",as.character(x[2]),perl=TRUE,value=TRUE))>0){print(return(x[1]))} ))
mean_values <- laply(mean_values,function(l) as.numeric(l[1]))
std_values<- unlist(apply(feature_data,1,function(x) if(length(grep("std+",as.character(x[2]),perl=TRUE,value=TRUE))>0){print(return(x[1]))} ))
std_values <- laply(std_values,function(l) as.numeric(l[1]))
col_num <- c(mean_values,std_values)
final_data_mean_std <- final_data[,c(col_num,562,563)]

# Uses descriptive activity names to name the activities in the data set

activity_data <- read.fwf("activity_labels.txt",widths=c(2,30),header=FALSE,sep="\t")
names(activity_data) <- c("S.No","Activity_Type")
mergedData = merge(final_data_mean_std,activity_data,by.x="Activity",by.y="S.No",all=TRUE)
final_merged_data <- mergedData[,c(-1)]
cols <- names(final_merged_data)[3:81]
final_merged_data <- final_merged_data[,c("Subject","Activity_Type",cols)]
final_merged_data <- final_merged_data[,c(-82,-83)]

#Appropriately labels the data set with descriptive variable names. 

col_names <- lapply(col_num,function(x) feature_data[x,][2])
col_names <- laply(col_names,function(l) as.character(l))
colnames(final_merged_data)[3:81]<- col_names

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
final_merged_sorted_data <- final_merged_data[order(final_merged_data$Subject),]
melt_cast_fun <- function(x) {
melted_data <- melt(x,id=c("Subject","Activity_Type"),measure.vars=names(proxy_data)[3:81])
dcast(melted_data,Activity_Type~variable,mean)
}
final_list_data <- lapply(unique(final_merged_sorted_data$Subject),function(x) melt_cast_fun(final_merged_sorted_data[final_merged_sorted_data$Subject==x,]))
final_mean_data <- do.call(rbind.data.frame,final_list_data)
x <- seq(from=1,to=30,by=1)
final_mean_data$Subject=sort(rep(x,6))
new_col_name <- names(final_mean_data)
final_mean_data <- final_mean_data[,c("Subject",new_col_name[1:80])]



