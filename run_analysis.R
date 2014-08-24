# You should create one R script called run_analysis.R that does the following. 

# 1. Merges the training and the test sets to create one data set.

train_data <- read.table("train/X_train.txt")
test_data <- read.table("test/X_test.txt")
x_data <- rbind(train_data, test_data)

train_data <- read.table("train/subject_train.txt")
test_data <- read.table("test/subject_test.txt")
subj_data <- rbind(train_data, test_data)

train_data <- read.table("train/y_train.txt")
test_data <- read.table("test/y_test.txt")
activity_data <- rbind(train_data, test_data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")

# find required features: mean, std

used_features <- grep("-mean\\(\\)|-std\\(\\)",features[,2])

# create data frame with only used features + format and clean

x_data <- x_data[,used_features]
names(x_data) <- tolower(features[used_features, 2])
names(x_data) <- gsub("\\(|\\)","",names(x_data))

# 3. Uses descriptive activity names to name the activities in the data set

names(activity_data) <- "activity"
activity <- read.table("activity_labels.txt")
activity_data[,1] = activity[activity_data[,1],2]

# 4. Appropriately labels the data set with descriptive activity names. 

names(subj_data) <- "subject"
tidy <- cbind(subj_data, activity_data, x_data)
write.table(tidy, "tidy_data.txt", row.names=FALSE)

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

numActivities <- nrow(activity)
numCols <- dim(tidy)[2]
uniqueSubjects <- unique(subj_data)[,1]
numSubjects <- length(uniqueSubjects)

data_summary <- tidy[1:(numSubjects*numActivities), ]

i = 0

# aggregate by subject and activity 

for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    data_summary[i, 1] = uniqueSubjects[s]
    data_summary[i, 2] = activity[a, 2]
    idxs <- tidy[tidy$subject==s & tidy$activity==activity[a, 2], ]
    data_summary[i, 3:numCols] <- colMeans(idxs[, 3:numCols])
    i = i + 1
  }
}
write.table(data_summary, "tidy_aggregate_data.txt", row.names=FALSE)
