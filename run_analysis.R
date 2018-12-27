library(plyr)
library(dplyr)

#### Features ####
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
features$V2 <- gsub("-"," ",features$V2)
features$V2 <- gsub(",","",features$V2)
features$V2 <- gsub("\\(","",features$V2)
features$V2 <- gsub("\\)","",features$V2)

#### Train Volunteers ####
train_vol <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep = " ", col.names = "ID")

#### Activity of Train Set ####
train_label <- read.table("./UCI HAR Dataset/train/y_train.txt", sep = " ", col.names = "Activity")

#### Train Set ####
train_set <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, col.names = features$V2)

#### Test Volunteers ####
test_vol <- read.table("./UCI HAR Dataset/test/subject_test.txt", sep = " ", col.names = "ID")

#### Activity of Test Set ####
test_label <- read.table("./UCI HAR Dataset/test/y_test.txt", sep = " ", col.names = "Activity")

#### Test Set ####
test_set <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, col.names = features$V2)

#### Step 1: Merging of Datasets ####
train <- cbind(train_vol, cbind(train_label, train_set))
test <- cbind(test_vol, cbind(test_label, test_set))
measure <- rbind(train, test)

#### Step 2: Getting Mean and Std Columns ####
i <- grep("[Mm][Ee][Aa][Nn]",names(measure))
j <- grep("[Ss][Tt][Dd]",names(measure))
measure2 <- measure[,c(1,2,i,j)]

#### Step 3: Renaming Activity Column ####
activity <- read.table("./UCI HAR Dataset/activity_labels.txt", sep = " ")
activity_fac <- as.factor(activity$V2)
measure2$Activity <- factor(measure2$Activity, labels = activity_fac)
measure2 <- arrange(measure2, ID, Activity)

#### Step 4: Renaming of Data Set ####
# Done with the reading of the text files

#### Step 5: Computation of Average of Each Variable ####

measure3 <- measure2 %>%
    group_by(ID, Activity) %>%
    summarize(Average.tBodyAcc.mean.X = mean(tBodyAcc.mean.X, na.rm = TRUE),
              Average.tBodyAcc.mean.Y = mean(tBodyAcc.mean.Y, na.rm = TRUE),
              Average.tBodyAcc.mean.Z = mean(tBodyAcc.mean.Z, na.rm = TRUE),
              Average.tGravityAcc.mean.X = mean(tGravityAcc.mean.X, na.rm = TRUE),
              Average.tGravityAcc.mean.Y = mean(tGravityAcc.mean.Y, na.rm = TRUE),
              Average.tGravityAcc.mean.Z = mean(tGravityAcc.mean.Z, na.rm = TRUE),
              Average.tBodyAccJerk.mean.X = mean(tBodyAccJerk.mean.X, na.rm = TRUE),
              Average.tBodyAccJerk.mean.Y = mean(tBodyAccJerk.mean.Y, na.rm = TRUE),
              Average.tBodyAccJerk.mean.Z = mean(tBodyAccJerk.mean.Z, na.rm = TRUE),
              Average.tBodyGyro.mean.X = mean(tBodyGyro.mean.X, na.rm = TRUE),
              Average.tBodyGyro.mean.Y = mean(tBodyGyro.mean.Y, na.rm = TRUE),
              Average.tBodyGyro.mean.Z = mean(tBodyGyro.mean.Z, na.rm = TRUE),
              Average.tBodyGyroJerk.mean.X = mean(tBodyGyroJerk.mean.X, na.rm = TRUE),
              Average.tBodyGyroJerk.mean.Y = mean(tBodyGyroJerk.mean.Y, na.rm = TRUE),
              Average.tBodyGyroJerk.mean.Z = mean(tBodyGyroJerk.mean.Z, na.rm = TRUE),
              Average.tBodyAccMag.mean = mean(tBodyAccMag.mean, na.rm = TRUE),
              Average.tGravityAccMag.mean = mean(tGravityAccMag.mean, na.rm = TRUE),
              Average.tBodyAccJerkMag.mean = mean(tBodyAccJerkMag.mean, na.rm = TRUE),
              Average.tBodyGyroMag.mean = mean(tBodyGyroMag.mean, na.rm = TRUE),
              Average.tBodyGyroJerkMag.mean = mean(tBodyGyroJerkMag.mean, na.rm = TRUE),
              Average.fBodyAcc.mean.X = mean(fBodyAcc.mean.X, na.rm = TRUE),
              Average.fBodyAcc.mean.Y = mean(fBodyAcc.mean.Y, na.rm = TRUE),
              Average.fBodyAcc.mean.Z = mean(fBodyAcc.mean.Z, na.rm = TRUE),
              Average.fBodyAcc.meanFreq.X = mean(fBodyAcc.meanFreq.X, na.rm = TRUE),
              Average.fBodyAcc.meanFreq.Y = mean(fBodyAcc.meanFreq.Y, na.rm = TRUE),
              Average.fBodyAcc.meanFreq.Z = mean(fBodyAcc.meanFreq.Z, na.rm = TRUE),
              Average.fBodyAccJerk.mean.X = mean(fBodyAccJerk.mean.X, na.rm = TRUE),
              Average.fBodyAccJerk.mean.Y = mean(fBodyAccJerk.mean.Y, na.rm = TRUE),
              Average.fBodyAccJerk.mean.Z = mean(fBodyAccJerk.mean.Z, na.rm = TRUE),
              Average.fBodyAccJerk.meanFreq.X = mean(fBodyAccJerk.meanFreq.X, na.rm = TRUE),
              Average.fBodyAccJerk.meanFreq.Y = mean(fBodyAccJerk.meanFreq.Y, na.rm = TRUE),
              Average.fBodyAccJerk.meanFreq.Z = mean(fBodyAccJerk.meanFreq.Z, na.rm = TRUE),
              Average.fBodyGyro.mean.X = mean(fBodyGyro.mean.X, na.rm = TRUE),
              Average.fBodyGyro.mean.Y = mean(fBodyGyro.mean.Y, na.rm = TRUE),
              Average.fBodyGyro.mean.Z = mean(fBodyGyro.mean.Z, na.rm = TRUE),
              Average.fBodyGyro.meanFreq.X = mean(fBodyGyro.meanFreq.X, na.rm = TRUE),
              Average.fBodyGyro.meanFreq.Y = mean(fBodyGyro.meanFreq.Y, na.rm = TRUE),
              Average.fBodyGyro.meanFreq.Z = mean(fBodyGyro.meanFreq.Z, na.rm = TRUE),
              Average.fBodyAccMag.mean = mean(fBodyAccMag.mean, na.rm = TRUE),
              Average.fBodyAccMag.meanFreq = mean(fBodyAccMag.meanFreq, na.rm = TRUE),
              Average.fBodyBodyAccJerkMag.mean = mean(fBodyBodyAccJerkMag.mean, na.rm = TRUE),
              Average.fBodyBodyAccJerkMag.meanFreq = mean(fBodyBodyAccJerkMag.meanFreq, na.rm = TRUE),
              Average.fBodyBodyGyroMag.mean = mean(fBodyBodyGyroMag.mean, na.rm = TRUE),
              Average.fBodyBodyGyroMag.meanFreq = mean(fBodyBodyGyroMag.meanFreq, na.rm = TRUE),
              Average.fBodyBodyGyroJerkMag.mean = mean(fBodyBodyGyroJerkMag.mean, na.rm = TRUE),
              Average.fBodyBodyGyroJerkMag.meanFreq = mean(fBodyBodyGyroJerkMag.meanFreq, na.rm = TRUE),
              Average.angletBodyAccMeangravity = mean(angletBodyAccMeangravity, na.rm = TRUE),
              Average.angletBodyAccJerkMeangravityMean = mean(angletBodyAccJerkMeangravityMean, na.rm = TRUE),
              Average.angletBodyGyroMeangravityMean = mean(angletBodyGyroMeangravityMean, na.rm = TRUE),
              Average.angletBodyGyroJerkMeangravityMean = mean(angletBodyGyroJerkMeangravityMean, na.rm = TRUE),
              Average.angleXgravityMean = mean(angleXgravityMean, na.rm = TRUE),
              Average.angleYgravityMean = mean(angleYgravityMean, na.rm = TRUE),
              Average.angleZgravityMean = mean(angleZgravityMean, na.rm = TRUE),
              Average.tBodyAcc.std.X = mean(tBodyAcc.std.X, na.rm = TRUE),
              Average.tBodyAcc.std.Y = mean(tBodyAcc.std.Y, na.rm = TRUE),
              Average.tBodyAcc.std.Z = mean(tBodyAcc.std.Z, na.rm = TRUE),
              Average.tGravityAcc.std.X = mean(tGravityAcc.std.X, na.rm = TRUE),
              Average.tGravityAcc.std.Y = mean(tGravityAcc.std.Y, na.rm = TRUE),
              Average.tGravityAcc.std.Z = mean(tGravityAcc.std.Z, na.rm = TRUE),
              Average.tBodyAccJerk.std.X = mean(tBodyAccJerk.std.X, na.rm = TRUE),
              Average.tBodyAccJerk.std.Y = mean(tBodyAccJerk.std.Y, na.rm = TRUE),
              Average.tBodyAccJerk.std.Z = mean(tBodyAccJerk.std.Z, na.rm = TRUE),
              Average.tBodyGyro.std.X = mean(tBodyGyro.std.X, na.rm = TRUE),
              Average.tBodyGyro.std.Y = mean(tBodyGyro.std.Y, na.rm = TRUE),
              Average.tBodyGyro.std.Z = mean(tBodyGyro.std.Z, na.rm = TRUE),
              Average.tBodyGyroJerk.std.X = mean(tBodyGyroJerk.std.X, na.rm = TRUE),
              Average.tBodyGyroJerk.std.Y = mean(tBodyGyroJerk.std.Y, na.rm = TRUE),
              Average.tBodyGyroJerk.std.Z = mean(tBodyGyroJerk.std.Z, na.rm = TRUE),
              Average.tBodyAccMag.std = mean(tBodyAccMag.std, na.rm = TRUE),
              Average.tGravityAccMag.std = mean(tGravityAccMag.std, na.rm = TRUE),
              Average.tBodyAccJerkMag.std = mean(tBodyAccJerkMag.std, na.rm = TRUE),
              Average.tBodyGyroMag.std = mean(tBodyGyroMag.std, na.rm = TRUE),
              Average.tBodyGyroJerkMag.std = mean(tBodyGyroJerkMag.std, na.rm = TRUE),
              Average.fBodyAcc.std.X = mean(fBodyAcc.std.X, na.rm = TRUE),
              Average.fBodyAcc.std.Y = mean(fBodyAcc.std.Y, na.rm = TRUE),
              Average.fBodyAcc.std.Z = mean(fBodyAcc.std.Z, na.rm = TRUE),
              Average.fBodyAccJerk.std.X = mean(fBodyAccJerk.std.X, na.rm = TRUE),
              Average.fBodyAccJerk.std.Y = mean(fBodyAccJerk.std.Y, na.rm = TRUE),
              Average.fBodyAccJerk.std.Z = mean(fBodyAccJerk.std.Z, na.rm = TRUE),
              Average.fBodyGyro.std.X = mean(fBodyGyro.std.X, na.rm = TRUE),
              Average.fBodyGyro.std.Y = mean(fBodyGyro.std.Y, na.rm = TRUE),
              Average.fBodyGyro.std.Z = mean(fBodyGyro.std.Z, na.rm = TRUE),
              Average.fBodyAccMag.std = mean(fBodyAccMag.std, na.rm = TRUE),
              Average.fBodyBodyAccJerkMag.std = mean(fBodyBodyAccJerkMag.std, na.rm = TRUE),
              Average.fBodyBodyGyroMag.std = mean(fBodyBodyGyroMag.std, na.rm = TRUE),
              Average.fBodyBodyGyroJerkMag.std = mean(fBodyBodyGyroJerkMag.std, na.rm = TRUE))

write.table(measure3, file = "./average.txt", row.names = FALSE)
