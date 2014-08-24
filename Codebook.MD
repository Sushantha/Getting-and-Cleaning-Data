
Exploration:

	The first step I performed in my data analysis is to explore which of the various data files provided need to be used and what do each of these files contain. 
By going through the ReadMe file, and loading the files into R, I came to the following understanding:

	1. The files within the folder "Inertial Signals" are not needed for the purpose of this analysis.
	2. The files features_info.txt and README.txt are informational
	3. The files activity_labels.txt and features.txt contain the descriptions of the activities involved and the column headings for the readings listed. 
	4. The files X_Test and X_Train contain the actual data
	5. The files y_Test and Y_Train contain the activities for the corresponding reading. 
	6. The files subject_test and subject_train lists the subject for the readings listed. 

Data tranformations:

	The code performs the following steps

	 1.Merges the training and the test sets to create one data set.
	 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
	 3.Uses descriptive activity names to name the activities in the data set
	 4.Appropriately labels the data set with descriptive variable names. 
	 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

	 This is however performed in slightly different sequence namely step1, step 4, step 2 and 5 and then step 3. 


Data dictionary:

	The tidy data set has following fields

 [1] "Activity"                             "Activity_Desc"                        "tBodyAcc-mean()-Y"                    "tBodyAcc-mean()-Z"                   
 [5] "tBodyAcc-std()-X"                     "tBodyAcc-std()-Y"                     "tBodyAcc-std()-Z"                     "tGravityAcc-mean()-X"                
 [9] "tGravityAcc-mean()-Y"                 "tGravityAcc-mean()-Z"                 "tGravityAcc-std()-X"                  "tGravityAcc-std()-Y"                 
[13] "tGravityAcc-std()-Z"                  "tBodyAccJerk-mean()-X"                "tBodyAccJerk-mean()-Y"                "tBodyAccJerk-mean()-Z"               
[17] "tBodyAccJerk-std()-X"                 "tBodyAccJerk-std()-Y"                 "tBodyAccJerk-std()-Z"                 "tBodyGyro-mean()-X"                  
[21] "tBodyGyro-mean()-Y"                   "tBodyGyro-mean()-Z"                   "tBodyGyro-std()-X"                    "tBodyGyro-std()-Y"                   
[25] "tBodyGyro-std()-Z"                    "tBodyGyroJerk-mean()-X"               "tBodyGyroJerk-mean()-Y"               "tBodyGyroJerk-mean()-Z"              
[29] "tBodyGyroJerk-std()-X"                "tBodyGyroJerk-std()-Y"                "tBodyGyroJerk-std()-Z"                "tBodyAccMag-mean()"                  
[33] "tBodyAccMag-std()"                    "tGravityAccMag-mean()"                "tGravityAccMag-std()"                 "tBodyAccJerkMag-mean()"              
[37] "tBodyAccJerkMag-std()"                "tBodyGyroMag-mean()"                  "tBodyGyroMag-std()"                   "tBodyGyroJerkMag-mean()"             
[41] "tBodyGyroJerkMag-std()"               "fBodyAcc-mean()-X"                    "fBodyAcc-mean()-Y"                    "fBodyAcc-mean()-Z"                   
[45] "fBodyAcc-std()-X"                     "fBodyAcc-std()-Y"                     "fBodyAcc-std()-Z"                     "fBodyAcc-meanFreq()-X"               
[49] "fBodyAcc-meanFreq()-Y"                "fBodyAcc-meanFreq()-Z"                "fBodyAccJerk-mean()-X"                "fBodyAccJerk-mean()-Y"               
[53] "fBodyAccJerk-mean()-Z"                "fBodyAccJerk-std()-X"                 "fBodyAccJerk-std()-Y"                 "fBodyAccJerk-std()-Z"                
[57] "fBodyAccJerk-meanFreq()-X"            "fBodyAccJerk-meanFreq()-Y"            "fBodyAccJerk-meanFreq()-Z"            "fBodyGyro-mean()-X"                  
[61] "fBodyGyro-mean()-Y"                   "fBodyGyro-mean()-Z"                   "fBodyGyro-std()-X"                    "fBodyGyro-std()-Y"                   
[65] "fBodyGyro-std()-Z"                    "fBodyGyro-meanFreq()-X"               "fBodyGyro-meanFreq()-Y"               "fBodyGyro-meanFreq()-Z"              
[69] "fBodyAccMag-mean()"                   "fBodyAccMag-std()"                    "fBodyAccMag-meanFreq()"               "fBodyBodyAccJerkMag-mean()"          
[73] "fBodyBodyAccJerkMag-std()"            "fBodyBodyAccJerkMag-meanFreq()"       "fBodyBodyGyroMag-mean()"              "fBodyBodyGyroMag-std()"              
[77] "fBodyBodyGyroMag-meanFreq()"          "fBodyBodyGyroJerkMag-mean()"          "fBodyBodyGyroJerkMag-std()"           "fBodyBodyGyroJerkMag-meanFreq()"     
[81] "angle(tBodyAccMean,gravity)"          "angle(tBodyAccJerkMean),gravityMean)" "angle(tBodyGyroMean,gravityMean)"     "angle(tBodyGyroJerkMean,gravityMean)"
[85] "angle(X,gravityMean)"                 "angle(Y,gravityMean)"                 "angle(Z,gravityMean)"                 "Subject"           
		
		 
	where the columns 3 through 87 are the means of the respective variable names written. 

	
