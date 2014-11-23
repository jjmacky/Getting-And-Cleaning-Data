#################################################################################################
# James McCammon
# Getting and Cleaning Data
# Course Project Version 1
# 20 November 2014
#################################################################################################

# Note: Running this script will download data from the internet, create folders in your working
# directory, unzip files, store files of a significant size (> 50 MB) on your computer, create
# several R objects of significant size (> 50 MB), and completely remove all objects in your
# enviornment as a cleanup procedure. Please do not run this script without reading and 
# understanding the code and disabling any necessary components.

#################################################################################################
# Define custom functions to get data from the web and load data into R
#################################################################################################

# Get data function
getData = function(fileURL, mainDir, subDir, rawFilePath) {
  # If the data doesn't exist download it and setup a folder to store it  
  if (!file.exists(subDir)) {
    print("Downloading data and creating file directory...")  
    download.file(fileURL, destfile = rawFilePath)
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    unzip(rawFile, exdir = subDir)
  }
  else {
    print("Files already download. Ready to read into R memory.")
  }
}

# Load data function
loadData = function(fileNames, requiredFiles, envir = environment()) {
  numFiles = length(requiredFiles)
  pb = txtProgressBar(min = 0, max = numFiles, style = 3)
  # Load each file into R
  print("Loading files into memory. Please wait.")
  for (file in 1:numFiles) {
    elem = requiredFiles[file]
    filePath = paste0("/",elem)
    R_Object = gsub(".txt", "", elem)
    assign(R_Object, read.table(fileNames[grep(filePath, fileNames)], header=FALSE), envir)
    setTxtProgressBar(pb, file)
  }
  close(pb)
}

#################################################################################################
# Get, load, combine, and clean the data
#################################################################################################

################## Download data from the web and setup in folder ###############################
# Specify directories and file names
fileURL = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
mainDir = getwd()
subDir = "Raw_Extracted"
rawFile = 'CourseProj_Raw.zip'
rawFilePath = file.path(mainDir, rawFile)
# Download data and put in directory using custom function
getData(fileURL, mainDir, subDir, rawFilePath) 



############################# Load required files into R ########################################
# Get file names
fileNames = list.files(subDir, full.names = TRUE, recursive = TRUE)
# Specify required files
requiredFiles = c("features.txt",
                  "subject_train.txt",
                  "subject_test.txt",
                  "X_train.txt",
                  "X_test.txt",
                  "y_train.txt",
                  "y_test.txt")
# Get required files from the list of files using custom function
loadData(fileNames, requiredFiles, envir = .GlobalEnv)



############################### Combine and clean data ##########################################
# Combine X data
x_data = rbind.data.frame(X_train, X_test)
# Use names specified in feature list
names(x_data) = as.character(features[[2]])
# Cleanup variable names
names(x_data) = gsub("\\(\\)", "", names(x_data))

# Combine y data
y_data = rbind.data.frame(y_train, y_test)
# Specify y factor names and set factors
y_factors = c("Walking",
              "Walking_Upstairs",
              "Walking_Downstairs",
              "Sitting",
              "Standing",
              "Laying")
y_data = factor(y_data$V1,labels = y_factors)

# Combine subject data
subject_data = rbind.data.frame(subject_train, subject_test)
# Specify subject factor names and set factors
subject_factors = paste0(rep("Subject"), "_", seq(1:max(subject_data)))
subject_data = factor(subject_data$V1, labels = subject_factors)

# Each row of a tidy dataset should be one observation.
# Currently there are multiple observations for each subject
# for each activity, but no index to specify the measurment
# number. We must create a measurment index.
seqVar = paste0(as.character(y_data), as.character(subject_data))
measurement = unlist(sapply(table(seqVar), seq))

# Combine all data together
Tidy_HARUS_All = cbind.data.frame("Subject" = subject_data, 
                                  "Activity" = y_data, 
                                  "Measurement" = measurement, 
                                  x_data)

# Remove unneeded data from memory
rm(seqVar, x_data, y_data, subject_data)
rm(X_test, X_train, features, y_test, y_train, subject_test, subject_train)



############################# Create second tidy data set ####################################### 
means_and_stds_index = which(grepl("mean|std", ignore.case = TRUE, x = names(Tidy_HARUS_All)))
columns = c(1:3, means_and_stds_index)
Tidy_HARUS_Means_And_Stds = Tidy_HARUS_All[,columns]



############################## Create third tidy data set #######################################
# Calculate column means
Tidy_HARUS_Col_Means = 
aggregate(x = Tidy_HARUS_All[,means_and_stds_index], 
          by = list(Tidy_HARUS_All[["Subject"]], 
                    Tidy_HARUS_All[["Activity"]]), 
          FUN = mean)
names(Tidy_HARUS_Col_Means)[1:2] = names(Tidy_HARUS_All)[1:2]



################################# Write files to disk ########################################### 
# write txt files
write.table(Tidy_HARUS_All, file = "Tidy_HARUS_All.txt", row.name = FALSE)
write.table(Tidy_HARUS_Means_And_Stds, file = "Tidy_HARUS_Means_And_Stds.txt", row.name = FALSE)
write.table(Tidy_HARUS_Col_Means, file = "Tidy_HARUS_Col_Means.txt", row.name = FALSE)

# Write csv files
write.csv(Tidy_HARUS_All, file = "Tidy_HARUS_All.csv")
write.csv(Tidy_HARUS_Means_And_Stds, file = "Tidy_HARUS_Means_And_Stds.csv")
write.csv(Tidy_HARUS_Col_Means, file = "Tidy_HARUS_Col_Means.csv")



################################# Cleanup enviornment ########################################### 
rm(list = ls())

