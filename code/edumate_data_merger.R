# This script takes two (maybe three later) csv files that are downloaded
# from Edumate and merges them into the relevant output file, ready to be 
# uploaded to the Effort Tracking website

############# CHANGE THESE AS REQUIRED ###################
Enrolment_Data_Path <- "data/edumate_enrolment_data_2018t3.csv"
Student_Info_Path <- "data/edumate_student_data_2018t3.csv"
Effort_Tracking_Uploader_Filename <- "effort_tracking_data_2018t3.csv"
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
# Upload data should have the following fields:
  # Student ID	# Gender	# Gender ID	# Student Firstname	# Student Surname	# StudentEmail	
  # YEAR	# Tutor Group	# Cohort	# Course	# CLASS	  # CLASS_CODE	
  # Teacher Title	# Teacher Firstname	# Teacher Surname	# Teacher Email
# Merging data
uploadData <- merge(enrolData, studentData, all.x = TRUE)
# Writing data to csv
write.csv(uploadData, file = paste0("data/",Effort_Tracking_Uploader_Filename), row.names = F)
