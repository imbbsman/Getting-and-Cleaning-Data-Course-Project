Requirements:

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

 You should create one R script called run_analysis.R that does the following. 
Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement. 
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names. 
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#How the function works#
1. download the data from the given URL and extract it
2. Load the "train" and test files using LoadMergeData = function()
3. Select variables using fuction ExtractData=function(df)
4. Uses descriptive activity names to name the activities in the data set using function SetActivityNames = function(df)
5. Appropriately labels the data set with descriptive variable names
   to be specific, does the following:

   substitute "-mean" with ".mean"
   
   substitute "-std" with ".std"
   
   substitute "-meanFreq()" with ".mean.freq"
   
   substitute "-" with "."
   
   remove "()"
   
   convert upper to lower case
   
6. Write the tidy data set using MakeTidy = function(df)

For specified codes and variables, please refer to CodeBook.md and run_analysis.R
