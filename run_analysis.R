# directory structures and file names
# here I have the UCI HAR Dataset directory under my current working directory
data_directory  <- "UCI HAR Dataset"
train_directory <- "train"
test_directory  <- "test"

train_set_path <- paste("UCI HAR Dataset", "train", "X_train.txt", sep="/")
test_set_path  <- paste("UCI HAR Dataset", "test", "X_test.txt",   sep="/")

y_train_path   <- paste("UCI HAR Dataset", "train", "y_train.txt", sep="/")
y_test_path    <- paste("UCI HAR Dataset", "test", "y_test.txt",   sep="/")

subject_train_path <- paste("UCI HAR Dataset", "train", "subject_train.txt", sep="/")
subject_test_path  <- paste("UCI HAR Dataset", "test", "subject_test.txt",   sep="/")

features_path  <- paste("UCI HAR Dataset", "features.txt",  sep="/")
activity_path  <- paste("UCI HAR Dataset", "activity_labels.txt",  sep="/")

# 1. Merge both the data sets - training and the test sets to create one data set.
nrows <- 100
train_first_look <- read.table(train_set_path, nrows=nrows, header = FALSE, dec = ".", fill = TRUE, comment.char = "")
classes_train <- sapply(train_first_look, class)
rm(train_first_look)
train <- read.table(train_set_path, colClasses = classes_train, header = FALSE, dec = ".", comment.char = "")

test_first_look <- read.table(test_set_path, nrows=nrows, header = FALSE, dec = ".", fill = TRUE, comment.char = "")
classes_test <- sapply(test_first_look, class)
rm(test_first_look)

test <- read.table(test_set_path, colClasses = classes_test, header = FALSE, dec = ".", fill = TRUE, comment.char = "")
features <- read.table(features_path, colClasses = c("numeric","character"), col.names = c("Variable.id","Variable.Name"), header = FALSE, comment.char = "")

subject_train_ids <- read.table(subject_train_path, colClasses = c("numeric"), col.names = c("Subject.id"), header = FALSE, comment.char = "")
activity_train_ids <- read.table(y_train_path, colClasses = c("numeric"), col.names = c("Activity id"), header = FALSE, comment.char = "")

subject_test_ids <- read.table(subject_test_path, colClasses = c("numeric"), col.names = c("Subject.id"), header = FALSE, comment.char = "")
activity_test_ids <- read.table(y_test_path, colClasses = c("numeric"), col.names = c("Activity.id"), header = FALSE, comment.char = "")

test <- cbind(subject_test_ids,activity_test_ids,test)
train <- cbind(subject_train_ids,activity_train_ids,train)
all_data <- rbind(test,train)

names(all_data) <- c("Subject.id","Activity.id",features[,2])

all_data <- all_data[order(all_data$Subject.id,all_data$Activity.id),]

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_string <- "mean"
std_string <- "std"

for (i in seq_along(features$Variable.Name)) {
  if (!grepl(mean_string,features$Variable.Name[i]) & !grepl(std_string,features$Variable.Name[i])) {
    all_data[,features$Variable.Name[i]] <- NULL
  }
}

# 3. Uses descriptive activity names to name the activities in the data set

activity <- read.table(activity_path, colClasses = c("numeric","character"), col.names = c("Activity.id","Activity.Name"), header = FALSE, comment.char = "")

library(plyr)
all_data <- join(all_data, activity, by = "Activity.id")

all_data$Activity.id <- NULL

# 4. Appropriately labels the data set with descriptive variable names.
labels <- names(all_data)
labels <- labels[complete.cases(labels)]

for (i in seq_along(labels)) {
  labels[i] <- gsub("mean","Mean",labels[i])
  labels[i] <- gsub("std","Std",labels[i])
  labels[i] <- gsub("\\()","",labels[i]) # '(' is a special character in regex so we need to escape it
  labels[i] <- gsub("-","",labels[i])
}

names(all_data) <- labels

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy <- ddply(all_data, .(Subject.id, Activity.Name), numcolwise(mean))
labels  <- names(tidy)
for (i in 3:length(labels)) { #skips the 2 first
  labels[i] <- paste0("Mean.",labels[i])
}
names(tidy) <- labels

write.table(tidy,"my_tidy_data_set.txt",row.name=FALSE)