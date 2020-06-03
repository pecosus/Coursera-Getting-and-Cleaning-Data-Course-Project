filename <- "Coursera_DS3_Final.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}

#Loading library
library(dplyr)

#Assigning data
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Merging data from both test and train datasets. (1)
X_data <- rbind(x_test, x_train)
Y_data <- rbind(y_test, y_train)
subject_bound <- rbind(test_subject, train_subject)
merged_data <- cbind(subject_bound, X_data, Y_data)

#Extracting columns with mean and stdev information
data_extracted <- select(merged_data, subject, code, contains("mean"), contains("std"))

#Descriptive names. Since code in Y label is shared with code in "activities" dataframe, is possible
#directly replace the number by the labels
data_extracted$code <- activities[data_extracted$code, 2]

#Appropriately labels
names(data_extracted)[2] = "activity"
names(data_extracted)<-gsub("Acc", "Accelerometer", names(data_extracted))
names(data_extracted)<-gsub("Gyro", "Gyroscope", names(data_extracted))
names(data_extracted)<-gsub("BodyBody", "Body", names(data_extracted))
names(data_extracted)<-gsub("Mag", "Magnitude", names(data_extracted))
names(data_extracted)<-gsub("^t", "Time", names(data_extracted))
names(data_extracted)<-gsub("^f", "Frequency", names(data_extracted))
names(data_extracted)<-gsub("tBody", "TimeBody", names(data_extracted))
names(data_extracted)<-gsub("-mean()", "Mean", names(data_extracted), ignore.case = TRUE)
names(data_extracted)<-gsub("-std()", "STD", names(data_extracted), ignore.case = TRUE)
names(data_extracted)<-gsub("-freq()", "Frequency", names(data_extracted), ignore.case = TRUE)
names(data_extracted)<-gsub("angle", "Angle", names(data_extracted))
names(data_extracted)<-gsub("gravity", "Gravity", names(data_extracted))

#Constructing an independent dataset, with mean of data grouped by subject and activity

final_dataset <- data_extracted %>%
        group_by(subject, activity) %>%
        summarise_all(list(mean = mean))
write.table(final_dataset, "final_dataset.txt", row.name=FALSE)

