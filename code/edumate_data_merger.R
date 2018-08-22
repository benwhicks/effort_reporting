# This script takes two (maybe three later) csv files that are downloaded
# from Edumate and merges them into the relevant output file, ready to be 
# uploaded to the Effort Tracking website

############# CHANGE THESE AS REQUIRED ###################
Enrolment_Data_Path <- "data/2018T3_edumate_enrolment_data.csv"
Student_Info_Path <- "data/2018T3_edumate_student_data.csv"
Effort_Tracking_Uploader_Filename <- "2018T3_merged_enrolment_data.csv"
##########################################################

# Reading in Edumate data
enrolData <- read.csv(Enrolment_Data_Path)
studentData <- read.csv(Student_Info_Path)

# Fixing the names of fields that need to match between the two data frames
# If Edumate changes how it outputs fields this may need to be adjusted!
enrolData <- plyr::rename(enrolData, replace = c(
  "STUDENT_NUMBER" = "StudentID"))
studentData <- plyr::rename(studentData, replace = c(
  "Student.." = "StudentID"
))

# Merging data
uploadData <- merge(enrolData, studentData, all.x = TRUE)

# Creating GenderID and Year
uploadData$GenderID <- substring(uploadData$Gender, 1, 1)
uploadData$Form <- as.character(uploadData$Form)
uploadData$Year <- as.numeric(substring(uploadData$Form, nchar(uploadData$Form) - 1, nchar(uploadData)))
# Writing data to csv
write.csv(uploadData, file = paste0("data/",Effort_Tracking_Uploader_Filename), row.names = F)
