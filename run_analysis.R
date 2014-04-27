# Source of data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# This R script does the following:

# 1. Merges the training and the test sets to create one data set.

Xtrain <- read.table("train/X_train.txt")
Xtest  <- read.table("test/X_test.txt")
xx <- rbind(Xtrain, Xtest)

Strain <- read.table("train/subject_train.txt")
Stest  <- read.table("test/subject_test.txt")
Subjects <- rbind(Strain, Stest)
names(Subjects) <- "subject"

ytrain <- read.table("train/y_train.txt")
ytest  <- read.table("test/y_test.txt")
yy <- rbind(ytrain, ytest)
names(yy) <- "activity"

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
ii_mean <- grep("-mean\\(\\)", features[, 2])
ii_std  <- grep("-std\\(\\)", features[, 2])
ii_all  <- union(ii_mean, ii_std)

xx <- xx[, ii_all]
names(xx) <- features[ii_all, 2]

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
yy[,1] = activities[yy[,1], 2]

# 4. Appropriately labels the data set with descriptive activity names.

merged <- cbind(Subjects, yy, xx)
write.table(merged, "merged_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(Subjects[,1])
numSubjects = length(uniqueSubjects)
numActivities = length(activities[,1])
numCols = dim(merged)[2]
result = merged[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- merged[merged$subject==s & merged$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt")

