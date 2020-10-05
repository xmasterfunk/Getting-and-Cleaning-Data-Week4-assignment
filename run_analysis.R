library(dplyr)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "UCI HAR Dataset.zip"
if (!file.exists(fileName)) {
  download.file(url, fileName, mode = "wb")
}
datasetPath <- gsub(".zip","",fileName)
if (!file.exists(datasetPath)) {
  unzip(fileName)
}
################################################################################
# get training data
trainingSubject <- read.table(file.path(datasetPath, "train", "subject_train.txt"))
trainingX <- read.table(file.path(datasetPath, "train", "X_train.txt"))
trainingY <- read.table(file.path(datasetPath, "train", "y_train.txt"))
# get test data
testSubject <- read.table(file.path(datasetPath, "test", "subject_test.txt"))
testX <- read.table(file.path(datasetPath, "test", "X_test.txt"))
testY <- read.table(file.path(datasetPath, "test", "y_test.txt"))
# get features
features <- read.table(file.path(datasetPath, "features.txt"), as.is = TRUE)
# get activity data
activities <- read.table(file.path(datasetPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")
################################################################################
# concatenate individual data tables to make single data table
humanActivity <- rbind(
  cbind(trainingSubject, trainingX, trainingY),
  cbind(testSubject, testX, testY)
)
rm(trainingSubject, trainingX, trainingY, 
   testSubject, testX, testY)
colnames(humanActivity) <- c("subject", features[, 2], "activity")
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]
################################################################################
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])
################################################################################
# Make var for column names
humanActivityCols <- colnames(humanActivity)
# format it
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
# correct a typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
# use new labels as column names
colnames(humanActivity) <- humanActivityCols
# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))
# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)