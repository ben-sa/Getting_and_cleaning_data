# Load packages used in analysis
library(tidyverse)

# Reading of the Train data .txt files (X_train.txt and Y_train.txt) and
# the subject Train data.
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")

# Loading of the subject_train.txt. Each row identifies the subject who
# performed the activity. Its range is from 1 to 30.
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Reading of the Test data .txt files (X_test.txt and Y_test.txt) and
# the subject Train data.
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")

# Loading of the subject_test.txt. Each row identifies the subject who
# performed the activity. Its range is from 1 to 30.
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# load the column names for both training and test data
# since the names are in column 2 of the tabel (column 1
# just contains and index number), select column 2 in second
# step.
features <- read.table("./UCI HAR Dataset/features.txt")
features <- features[,2]

# Update the column names of the x_train and y_train with those stored
# in features. dim() function shows there are 561 columns in x_train,
# y_train and variables. So we can sure the number of features matches
# with the number of columns in x_train and y_train.
colnames(x_train) <- features
colnames(x_test) <- features

# Store only relevant features, that include the word "mean" or "std"
# (Mean or standard deviation) in a vector. Reduce x_train and
# x_test in next step to relevant features
relevant_features <- str_detect(features,"mean|std")
x_train <- x_train[,relevant_features]
x_test <- x_test[,relevant_features]

# Loading the activity labels from the "acitivity_labels.txt"
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# Actvivities in y_test and y_train are stored in numbers 1-5.
# The labels are stored in activity_labels, which will be used to add a second
# column to y_test and y_train containing the naming of the activities from
# acitivity labels.In a second step the columns are renamed.

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

y_test[,2] = activity_labels[y_test[,1]]
y_train[,2] = activity_labels[y_train[,1]]

names(y_test) = c("Activity_ID", "Activity_Label")
names(y_train) = c("Activity_ID", "Activity_Label")

# Rename the column of subject_test and subject_train
names(subject_test) = "subject"
names(subject_train) = "subject"


# Merge the y_test and x_test data
test_data <- cbind(subject_test, y_test, x_test)

# Merge the y_train and x_train data
train_data <- cbind(subject_train, y_train, x_train)

# Merge the test_data and train_data
data = rbind(test_data, train_data)

# Write tidy data into new txt file.
write.table(data, file = "./data.txt")

# Reshape dataset to have one single column
# containing the measurements and one single
# column containing the value for that
# measurement
data <- data %>% pivot_longer("tBodyAcc-mean()-X":"fBodyBodyGyroJerkMag-meanFreq()", names_to = "Measurement", values_to = "value")

# Create second dataset "data_2" with mean values for each acivity and each subject.
data_2 <- data %>% select(-Activity_ID) %>% group_by(subject, Activity_Label, Measurement) %>% summarize(mean_values = mean(value, na.rm=TRUE))

# Write data_2 into a txt file
write.table(data_2, file = "./data_2.txt")
