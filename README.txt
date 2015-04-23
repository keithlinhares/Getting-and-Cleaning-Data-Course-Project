==================================================================
Keith Linhares
Getting and Cleaning Data Course Project

My R Code is listed below with an explanation of what each section does.

Basically, the code required rbind() and cbind() to link the data together.
The columns were named appropriately based on the Features data

From there, columns were limited to just the column names with mean and std.

Next, the Activities were names WALKING instead of 1, WALKING_UPSTAIRS instead of 2, 
and so forth.

Then, the column names were updated with a for loop to be a bit easier to read and
understand.

Beneath my code, there is the original readme file downloaded with the project.

Thanks for looking!


# Bring in the test and train sets. 

xtest <- read.table("./test/X_test.txt")
ytest <- read.table("./test/Y_test.txt")
xtrain <- read.table("./train/X_train.txt")
ytrain <- read.table("./train/y_train.txt")

# Bring in the subjects
subjecttrain <-read.table("./train/subject_train.txt")
subjecttest <-read.table("./test/subject_test.txt")

#Bring in the features. These will become the column names for the test/train data
features <- read.table("./features.txt")

# Read in activity labels
activities <- read.table("./activity_labels.txt")

######STEP 1 Merges the training and the test sets to create one data set.
#Row Bind together x train and test
datax <- rbind(xtest,xtrain)

#Row Bind together y train and test
datay <- rbind(ytest,ytrain)

#Row Bind Subject train and test
datasub <- rbind(subjecttest, subjecttrain)
                 
#Column Bind datasub to datax
dataxysub <- cbind(datax, datasub)
                 
#Column Bind the data x and data x table
mergeddata <- cbind(dataxysub, datay)

#Give the columns in mergeddata the names from features
names(mergeddata) <- features$V2

#The last two columns are not named (checked out below with Test), so 
# I'm going to name them Subject and Activity
test <- mergeddata[,561:563]
View(test)
names(mergeddata)[562] <- "Subject"
names(mergeddata)[563] <- "Activity"


######STEP 2 Extracts only the measurements on the mean and standard deviation 
######for each measurement. 

#Keep only the columns with names we want:
# mean or std or Subject or Activity
# First, I create a variable called cols with the names of the columns in merged data
# Then I search for just the columns that we want in final analysis
# searching mean with "\\(\\)" limits to entries with mean() and excludes the FreqMean variables
# Then I create a new table that takes mergeddata and is just the select column names we want

cols <- names(mergeddata)
selectcols <- grep("mean\\(\\)|std|Subject|Activity",cols, value = TRUE)
mydfselectcols <- mergeddata[,selectcols]


#### STEP 3 Descriptive Activity Names
# I am doing this manually since there are only 6 variables
# I am interested to learn a better way to do this if there are more variables
# In Excel, I would do this with VLOOKUP, not sure how to do it here.

mydfselectcols$Activity[mydfselectcols$Activity == "1"] <- "WALKING"
mydfselectcols$Activity[mydfselectcols$Activity == "2"] <- "WALKING_UPSTAIRS"
mydfselectcols$Activity[mydfselectcols$Activity == "3"] <- "WALKING_DOWNSTAIRS"
mydfselectcols$Activity[mydfselectcols$Activity == "4"] <- "SITTING"
mydfselectcols$Activity[mydfselectcols$Activity == "5"] <- "STANDING"
mydfselectcols$Activity[mydfselectcols$Activity == "6"] <- "LAYING"

#### STEP 4 Descriptive Variable Names. Improve the column names to be more descriptive 
# Go through the list and update the names with a for statement

colNames <- colnames(mydfselectcols)
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("$std$","StdDev",colNames[i])
  colNames[i] = gsub("-std-","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Make the column name in mydfselectcols show the updated column names
colnames(mydfselectcols) = colNames

##STEP 5 From the data set in step 4, creates a second, independent tidy data set with 
## the average of each variable for each activity and each subject.
# use aggregate on all variables by subject and activity, then write the table

tidydata <- aggregate(.~Subject+Activity, mydfselectcols,mean)
View(tidydata)
write.table(tidydata, file = "tidydata.txt", row.names = FALSE)
 

# Make the codebook

library(knitr)
knit2html("codebook.Rmd")
















==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
