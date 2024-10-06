
run_analysis <- function() {

     if (!("reshape2" %in% rownames(installed.packages())) ) {
        stop ("Please install required package: reshape2!\n")
     } 

     library(reshape2)

     cat("\n")
     cat("Step1: Merges the training and the test set to create on data set.\n")

     traindata <- read.table("./train/X_train.txt")
     testdata  <- read.table("./test/X_test.txt")
     joindata  <- rbind(traindata, testdata) 


     dim(traindata) 
     dim(testdata)  
     dim(joindata)  


     trainlabel <- read.table("./train/y_train.txt")
     testlabel  <- read.table("./test/y_test.txt")
     joinlabel  <- rbind(trainlabel, testlabel)


     dim(trainlabel) 
     dim(testlabel) 
     dim(joinlabel) 


     trainsubject <- read.table("./train/subject_train.txt")
     testsubject  <- read.table("./test/subject_test.txt")
     joinsubject  <- rbind(trainsubject, testsubject)


     dim(trainsubject)
     dim(testsubject) 
     dim(joinsubject)  


     cat("\n")
     cat("Step2: Extracts only the measurements on the mean and standard deviation for each measurement.\n")

     features <- read.table("features.txt")
     dim(features) 

     meanstdindex <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])

     length(meanstdindex) 

     joindatanew <- joindata[, meanstdindex] 

     dim(joindatanew) 

     colnames(joindatanew) <- features[meanstdindex, 2] 


     colnames(joindatanew) <- gsub("\\(|\\)", "", colnames(joindatanew)) 
     colnames(joindatanew) <- gsub("-", ".", colnames(joindatanew))
     colnames(joindatanew) <- tolower(colnames(joindatanew))

     cat("\n")
     cat("Step3: Uses descriptive activity names to name the activities in the data set.\n")

     activity <- read.table("activity_labels.txt")

     activity[, 2] <- tolower(gsub("_", "", activity[, 2]))

     activitylabel <- activity[joinlabel[, 1], 2]

     joinlabel[, 1] <- activitylabel 

    colnames(joinlabel) <- "activity"

     cat("\n")
     cat("Step4: Appropriately labels the data set with descriptiv activity names.\n")

     colnames(joinsubject) <- "subject"

     cleandata <- cbind(joinsubject, joinlabel, joindatanew)

     dim(cleandata) 

     write.table(cleandata, "combinedcleandata.txt")

     cat("\n")
     cat("Step5: Creates a independent tidy data set with the average of each variable for each activity and each subject.\n")

     meltdfrm <- melt(cleandata, id=c("activity", "subject"))

     tidydfrm <- dcast(meltdfrm, activity + subject ~ variable, mean)

     write.table(tidydfrm, "tidy_average_data.txt", row.names = F, col.names= T, sep = "\t")

     cat("")
     cat("DONE: a tidy data file has been created in the working directory!\n")
     cat("")
     workdone <- "TRUE"

} 
