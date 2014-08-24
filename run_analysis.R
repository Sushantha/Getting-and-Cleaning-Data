run_analysis <- function() {

	# Function to do the following
	# 1.Merges the training and the test sets to create one data set.
	# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
	# 3.Uses descriptive activity names to name the activities in the data set
	# 4.Appropriately labels the data set with descriptive variable names. 
	# 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

	# Perform step 1 - Merge datasets 
		# Load all datasets
			dfTest <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/X_test.txt")
			dfTrain <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/X_train.txt")
			dfActivity_Test <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/Y_test.txt")
			dfActivity_Train <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/Y_train.txt")
			dfSubject_Test <- read.table("./getdata_projectfiles_UCI HAR Dataset/test/subject_test.txt")
			dfSubject_Train <- read.table("./getdata_projectfiles_UCI HAR Dataset/train/subject_train.txt")

		# Merge them
			dfAll       <- rbind(dfTest, dfTrain)
			dfActivity  <- rbind(dfActivity_Test, dfActivity_Train)
			dfSubject   <- rbind(dfSubject_Test, dfSubject_Train)
	
	# Load descriptions as required
		dfLabels <- read.table("./getdata_projectfiles_UCI HAR Dataset/activity_labels.txt")
		dfFeature <- read.table("./getdata_projectfiles_UCI HAR Dataset/features.txt")
	
	# Perform step 4 - Label with descriptive variable names 
		columnNames <- dfFeature$V2
		colnames(dfAll) <- columnNames
		colnames(dfSubject) <- "Subject"
		colnames(dfActivity) <- "Activity"
		dfList <- cbind(dfActivity, dfSubject, dfAll)

	# Perform step 2 and step 5 - Exract only measurements on mean and standard deviation and calculate their averages
		# Identify all column names with Mean and standard deviation through partial matching. Sort this list of columns.
			cols <- c(agrep("Mean", dfFeature$V2), agrep("std", dfFeature$V2)) + 2
			cols <- sort(cols)
		
		# Create the averages for the columns above by activity and subject using split and sapply 
			s <- split(dfList, list(dfList$Activity, dfList$Subject), drop = TRUE) 		
			tidy_step1 <- sapply(s, function(x) colMeans(x[,cols]))

		# Transform the result such that activity performed and subject can be added back as columns to the summarized data set. 
		tidy_step2 <- t(tidy_step1[,1:ncol(tidy_step1)])
		colnames(tidy_step2) <- rownames(tidy_step1)
		c1 <- as.integer(substring(rownames(tidy_step2),1,1))
		c2 <- as.integer(substring(rownames(tidy_step2),3,3))
		tidy_step3 <- cbind(tidy_step2,c2,c1 )

		colnames(tidy_step3)[ncol(tidy_step3)] <- "Activity"
		colnames(tidy_step3)[ncol(tidy_step3)-1] <- "Subject"

	# Perform step 3 - Add descriptive activity names
		colnames(dfLabels)[1] = "Activity"
		colnames(dfLabels)[2] = "Activity_Desc"
		tidy <- (merge(dfLabels, tidy_step3, by = 'Activity'))[,-3]
	# Write out the tidy data set to disk 
		write.table(tidy3, file="assignment_cleaning_data.txt", sep="," , row.names=FALSE)
}


