setwd("C:/Users/Kristin.Butler/Desktop/Coursera/DataAquisition/W4/Final Project/UCI HAR Dataset")
library(dplyr)
library(tidyr)
library(plyr)
library(reshape2)

## 1.Merges the training and the test sets to create one data set.
#Import of data from the test folder 
x_test <- read.table("./test/X_test.txt") %>% as.data.frame
names(x_test)

#Create a vector out of tests and subjects for the test data set
y_test <- read.table("./test/y_test.txt")
y_test
y_testV <- y_test[[1]]

sub_test <- read.table("./test/subject_test.txt")
sub_test
sub_testV <- sub_test[[1]]

#Add test and subject columns to test data frame
data_test <- mutate(x_test, test = y_testV,  subject = sub_testV)

#Import of data from the train folder  
x_train <- read.table("./train/X_train.txt")
x_train
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#Create a vector out of tests and subjects for the train data set
y_train <- read.table("./train/y_train.txt")
y_trainV <- y_train[[1]]
y_trainV
sub_train <- read.table("./train/subject_train.txt")
sub_trainV <- sub_train[[1]]
sub_trainV

#Add columns to the train data frame
data_train <- mutate(x_train, test = y_trainV,  subject = sub_trainV) #%>% select(562, 563, 1:561) %>% as.data.frame


 
# Combine data from test and train in one table to make one data set
all_dataQ1 <- rbind(data_train, data_test)
dim(all_dataQ1)
names(all_dataQ1)

#Write the file for submission
write.csv(all_dataQ1, file = "data_combined_Q1.csv")
write.table(all_dataQ1, file = "data_combined_Q1.txt")


##2.Extracts only the measurements on the mean and standard deviation for each measurement.

#Read in  updated feature data
featuresU <- read.table("./features_updated.txt")
featuresU

#Make a vector of features
Vfeatures <- featuresU[, 2]
Vfeatures



#Make a vector of the column number of std and mean and add new columns
Vmeanstd <- grep("mean|std", x = Vfeatures)
Vmeanstd <- append(Vmeanstd, c(562, 563))
Vmeanstd

#Extract names for data
VfeaturesC <- Vfeatures[Vmeanstd]
VfeaturesC

#Select columns that only have mean and std.  Label columns by name
all_data_meanstd_Q2 <- select(all_dataQ1, x = Vmeanstd)
names(all_data_meanstd_Q2) <- VfeaturesC
names(all_data_meanstd_Q2)
dim(all_data_meanstd_Q2)
head(all_data_meanstd_Q2)

#Write the file for submission
write.csv(all_data_meanstd_Q2, file = "data_meanstd_Q2.csv")
write.table(all_data_meanstd_Q2, file = "data_meanst_Q2.txt")


#3.	Uses descriptive activity names to name the activities in the data set

#Import data and make a vector of the labels and make a vector
activity_label <- read.table("./activity_labels.txt")
activity_labelV <- activity_label[,2]
activity_labelV


label <- mapvalues(x=all_dataQ1$test, c("1", "2", "3", "4", "5", "6"), c("LAYING", "SITTING", "STANDING", "WALKING", "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS"))
all_dataQ1$test <- label

all_dataQ1$test
head(all_dataQ1)

#Write the file for submission
write.csv(all_data, file = "data_acitivy_Q3.csv")
write.table(all_data, file = "data_activity_Q3.txt")



##4.	Appropriately labels the data set with descriptive variable names. 
names(all_dataQ1) <- Vfeatures 
names(all_dataQ1)[562] <- "test"
names(all_dataQ1)[563] <- "subject"
names(all_dataQ1)
all_dataQ1

#Write the file for submission
write.csv(all_dataQ1, file = "data_descript_Q4.csv")
write.table(all_dataQ1, file = "data_descript_Q4.txt")



# 5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

all_data_sum <- all_dataQ1 %>% group_by(test, subject) %>% summarize_each(funs(mean))


#Write the file for submission
write.csv(all_data_groups, file = "data_grouped_Q5.csv")
write.table(all_data_groups, file = "data_grouped_Q5.txt")

