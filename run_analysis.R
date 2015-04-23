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



