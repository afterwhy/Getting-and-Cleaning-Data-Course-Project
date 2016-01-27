library(stringr)
library(dplyr)

## Capitalizes text and returns it as result
## 
## Args:
##   word: Text to capitalize
capitalize <- function(text) {
    result <- strsplit(text, " ")[[1]]
    paste(toupper(substring(result, 1,1)), substring(result, 2), sep="", collapse=" ")
}

## Creates data frame from corresponding Samsung measurement folder
## 
## Args:
##   name: folder name (can be "test" or "train")
##   columnNames: column names for resulting data frame
createDataSet <- function(name, columnNames) {
    folder <- paste0("./UCI HAR Dataset/", name)
    data <- read.csv(paste0(paste0(paste0(folder, "/X_"), name), ".txt"), sep = "", header = F, stringsAsFactors = F, col.names = columnNames)
    activities <- read.csv(paste0(paste0(paste0(folder, "/y_"), name), ".txt"), sep = "", header = F, stringsAsFactors = F, col.names = "activity")
    subjects <- read.csv(paste0(paste0(paste0(folder, "/subject_"), name), ".txt"), sep = "", header = F, stringsAsFactors = F, col.names = "subject")
    data <- cbind(activity = activities$activity, data)
    data <- cbind(subject = subjects$subject, data)
    data
}

## Changes name to more tidy one.
## Function removes all unnecessery symbols, and reorganize variable name components.
## In case if variable name doesn't need to be simplified, function returns capitalized variable name.
## 
## Args:
##   name: Name of the variable
getTidyName <- function(name) {
    matches <- str_match(name, "^([t|f])(.*)\\.(mean|std).*([X|Y|Z])")
    if (any(is.na(matches))) {
        capitalize(name)
    } else {
        type <- if (matches[1, 2] == "t") "Time" else "Freq"
        varName <- matches[1, 3]
        varMeasurement <- matches[1, 4]
        varAxis <- matches[1, 5]
        sprintf("%s%s%s%s", capitalize(varMeasurement), type, varName, varAxis)
    }
}

#Step 0. Check if dataset was already downloaded and extracted. If it wasn't, download and extract it.
if (!dir.exists("./UCI HAR Dataset")) {
    temp <- tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
    unzip(temp)
    unlink(temp)
}

#Step 1. Merge the training and the test sets to create one data set (rawData).
activityNames <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep="", stringsAsFactors = F, header = F, col.names = c("id", "label"))
columns <- read.csv("./UCI HAR Dataset/features.txt", sep="", stringsAsFactors = F, header = F, col.names = c("index", "name"))
testData <- createDataSet("test", columns$name)
trainData <- createDataSet("train", columns$name)
rawData <- merge(testData, trainData, all = T)

#Step 2. Extract only the measurements on the mean and standard deviation for each measurement (requiredData variavle).
requiredColumnIndices <- union(1, union(2, columns$index[grepl("-(mean\\(\\)|std\\(\\))-", columns$name)] + 2))
requiredData <- rawData[, requiredColumnIndices]
activityNames <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep="", stringsAsFactors = F, header = F, col.names = c("id", "label"))
requiredData <- subset(requiredData, select = c(subject, activity, tBodyAcc.mean...X:fBodyGyro.std...Z))

#Step 3. Replace activities with their actual names
requiredData <- merge(requiredData, activityNames, by.x = "activity", by.y = "id")
requiredData <- select(requiredData, -activity) %>%
    rename(activity = label)

#Step 4. Update data set labels with descriptive variable names.
properColumnNames <- sapply(colnames(requiredData), getTidyName)
names(properColumnNames) <- NULL
colnames(requiredData) <- properColumnNames

#Step 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- group_by(requiredData, Subject, Activity) %>%
    summarise_each(funs(mean))

#Step 6. Write result to the file "tidy_data.csv"
write.table(tidyData, "./tidy_data.txt", row.names = F)