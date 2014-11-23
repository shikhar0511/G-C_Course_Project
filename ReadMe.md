---
title: "ReadMe"
author: "Shikhar Malik"
date: "23 November 2014"
output: html_document
---

# Introduction

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# Objective

 You should create one R script called run_analysis.R that does the following. 
1) Merges the training and the test sets to create one data set.
2) Extracts only the measurements on the mean and standard deviation for each measurement. 
3) Uses descriptive activity names to name the activities in the data set
4) Appropriately labels the data set with descriptive variable names. 
5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Analysis of the R Code

For this project all the required R Code is submitted in the script file - run_Analysis.R in the github repository. 
If you would like to view it you can access it below:

https://github.com/shikhar0511/

Below is the detailed analysis of the R Code

1) Merging the training and the test sets to create one data set.

    DO NOT FORGET TO LOAD THE REQUIRED LIBRARIES FIRST 
    
    Upon reviewing the feature file, we came to observe that there were 561 features and each feature 
    had a fixed width of 16 digits (both in the test and training data set).Hence we use read.fwf with 
    above parameters to extract all the features data, out of both the test and training data set. Now 
    this might take considerable time as the data might have more than 10000 rows.
    
    On similar lines we have extracted the test and training activity columns. This is an indicator of 
    what activities lead to what all feature values,row wise. Similary, we have extracted the test and 
    the training subjects. We have appropriatly named the test and training subjects and activities.
    
    Next we bind the test and traing specific feature data with the specific subject and activity data 
    set. In the end we bind the both test and training data into one single data. Notice the different 
    rbind() and cbind() function used to perform these operations. Also notice that the final data is 
    ordered with respect to subject.

```{r echo=TRUE}
library(plyr)
library(reshape2)
setwd("/Volumes/Shikhar's HardDisk 1/Shikhar/Coursera/Data Science /Getting and Cleaning Data /data/UCI HAR Dataset")
widths <- rep(c(16),561)
train_feature_data <- read.fwf("./train/X_train.txt",widths,header=FALSE,sep="\t",skip=0,buffersize=15)
test_feature_data <- read.fwf("./test/X_test.txt",widths,header=FALSE,sep="\t",skip=0,buffersize=15)
test_activity <- read.fwf("./test/Y_test.txt",c(5),header=FALSE,sep="\t",skip=0,buffersize=15)
train_activity <- read.fwf("./train/Y_train.txt",c(5),header=FALSE,sep="\t",skip=0,buffersize=15)
test_subject <- read.fwf("./test/subject_test.txt",c(5),header=FALSE,sep="\t",skip=0,buffersize=15)
train_subject <- read.fwf("./train/subject_train.txt",c(5),header=FALSE,sep="\t",skip=0,buffersize=15)
names(train_subject) <- c("Subject")
names(test_subject) <- c("Subject")
names(train_activity) <- c("Activity")
names(test_activity) <- c("Activity")
final_train_data <- cbind(train_feature_data,train_activity,train_subject)
final_test_data <- cbind(test_feature_data,test_activity,test_subject)
final_data <- rbind(final_train_data,final_test_data)    
```


2) Extracts only the measurements on the mean and standard deviation for each measurement. 

    We extract the feature names from feature.txt file in the first three codes and then merge the
    data sets into one feauture data set. Next we use apply function to go through the feature data 
    row wise and extract only the features containing mean and std in their names. Here we make use of 
    laply function from plyr to convert the list values into a data frame. Next we extract only the
    column numbers pertaining to the mean and the std values from the final data set.

```{r echo=TRUE}
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
```

3) Use descriptive activity names to name the activities in the data set

    We extract the decriptive activity names from the activity text file in the repository. After 
    merging the activity and the final data set according to their S.NO. and activity_no respectively,
    we change the sequence of columns for a more meaningful interpretation of the final data set. Note
    the merger has changed the ordering of data . The data is no longer ordered according to the     subjects  
    
    
```{r echo=TRUE}
activity_data <- read.fwf("activity_labels.txt",widths=c(2,30),header=FALSE,sep="\t")
names(activity_data) <- c("S.No","Activity_Type")
mergedData = merge(final_data_mean_std,activity_data,by.x="Activity",by.y="S.No",all=TRUE)
final_merged_data <- mergedData[,c(-1)]
cols <- names(final_merged_data)[1:79]
final_merged_data <- final_merged_data[,c("Subject","Activity_Type",cols)]
final_merged_data <- final_merged_data[,c(-82,-83)]
```


4) Appropriately labels the data set with descriptive variable names. 

    Here we reaname the variable columns with appropriate substitutions. For more 
    information regarding the descriptive names given to the variables, refer to 
    the CodeBook.md

```{r echo=TRUE}
regx_function <- function(x) {
    if(length(grep("^ t",x))>0){x <- sub("^ t","time_",x)}
    if(length(grep("^ f",x))>0){x <- sub("^ f","freq_",x)}
    if(length(grep("Acc",x))>0){x <- sub("Acc","_Acceleration",x)}
    if(length(grep("BodyBody",x))>0){x <- sub("BodyBody","Body",x)}
    return(x)}
col_names <- lapply(col_num,function(x) regx_function(as.character(feature_data[x,][2][[1]])) )
col_names <- laply(col_names,function(l) as.character(l))
colnames(final_merged_data)[3:81]<- col_names
```

5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

    Here we first sort the data according to the subjects. Next we use melt() and dcast() functions to 
    get the average values according to the activity of every subject. Here we use lapply function on 
    subject wise data and pass the user defind melt_cast_fun on every subjects data, returning the mean
    of every features subject wise.We use do.call on rbind to restructure the data from a list into a                
    data frame. The final data is converted from Narrow format to wide format using this code.
       
    Note that when converting the dcasted data to a data frame, there are no subject values. It should 
    be implicitly assumed that the subject values are in an increasing order, as we have sorted the data 
    before melting it.Hence we use this information later to recreate the subject column in the end. We 
    reorder the columns for a meaningful raw data. At the end we write a txt file for the tidy data.
    
    If you would like to read the txt file containing the tidy and processed data use the following code:
    
    data <- read.table("output.txt",header=TRUE,sep=",")

```{r echo=TRUE}
final_merged_sorted_data <- final_merged_data[order(final_merged_data$Subject),]
melt_cast_fun <- function(x) {
melted_data <- melt(x,id=c("Subject","Activity_Type"),measure.vars=names(x)[3:81])
dcast(melted_data,Activity_Type~variable,mean)
}
final_list_data <- lapply(unique(final_merged_sorted_data$Subject),function(x) melt_cast_fun(final_merged_sorted_data[final_merged_sorted_data$Subject==x,]))
final_mean_data <- do.call(rbind.data.frame,final_list_data)
x <- seq(from=1,to=30,by=1)
final_mean_data$Subject=sort(rep(x,6))
new_col_name <- names(final_mean_data)
final_mean_data <- final_mean_data[,c("Subject",new_col_name[1:80])]
write.table(final_mean_data,file="output1.txt",row.names=FALSE,sep=",")
```

    




