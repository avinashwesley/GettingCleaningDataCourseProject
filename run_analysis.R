#install.packages("dplyr")
library(dplyr)
#read the training dataset
setwd("C:/Users/h122459/Desktop/Research/Coursera/course 3 Getting and cleaning data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
# 1. Merges the training and the test sets to create one data set.
setwd("./train")
trainDF = read.table("X_train.txt")
setwd("../test")
testDF = read.table("X_test.txt")
df = rbind(trainDF,testDF)
setwd("..")
features = read.table("features.txt")
names(df) = features$V2

#2 Extracts only the measurements on the mean and standard deviation for each measurement. 
index = grep("mean|std",names(df),ignore.case = T)
subsetDF=df[,index]
setwd("./train")
y_trainDF = read.table("y_train.txt")
setwd("../test")
y_testDF = read.table("y_test.txt")
labelDF = rbind(y_trainDF,y_testDF)
names(labelDF) = "Class"
# 3. Uses descriptive activity names to name the activities in the data set
setwd("..")
activityLabels = read.table("activity_labels.txt")
names(activityLabels) = c("Class","Activity")
labelActivityDF = merge(labelDF,activityLabels,by.x="Class",by.y="Class",ALL=TRUE)
subsetDF$Activity = labelActivityDF$Activity
setwd("./train")
subjectTrainLabel = read.table("subject_train.txt")
setwd("../test")
subjectTestLabel = read.table("subject_test.txt")
subjectLabel = rbind(subjectTrainLabel,subjectTestLabel)
names(subjectLabel) = "subjectLabel"
subsetDF$SubjectLabel = subjectLabel$subjectLabel

#4. Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
cleanDF=subsetDF %>% group_by(SubjectLabel,Activity) %>% summarise_each(funs(mean,mean(.,na.rm=TRUE))) %>% arrange(SubjectLabel)
# Updating the colNames vector to include the new column names after merge
colNames  = colnames(cleanDF); 


# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
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

# Reassigning the new descriptive column names to the finalData set
colnames(cleanDF) = colNames;


# Write data
write.table(cleanDF,"cleanData.txt",row.names = FALSE)
