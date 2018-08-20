# Set up
source("effort.functions.R")
datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2018 Term 2/"
effort.survey.date <- as.Date("2018-06-04")
edumate.enrolment.path <- paste0(datdir,"edumate_enrolment_export_20180530.csv")
past.effort.path <- paste0(datdir, "Past Effort Data/")
edumate.data.path <- paste0(datdir, "edumate_student_data_180530.csv")
student.data.path <- paste0(datdir, "student_responses_09.json")
teacher.data.path <- paste0(datdir, "teacher_responses_10.json")
teacher.code.email.path <- paste0(datdir, "teacherCodeAndEmail.csv")

##### Cleaning functions #####
getEnrolmentData <- function(enrolment.path, student.info, teacher.code.email) {
  df <- read.csv(enrolment.path, col.names = c("Form", "Student.code", "RegNum", "Firstname", "Surname", "Student.email", "Class", "Teacher.title", "Teacher.firstname", "Teacher.surname", "Teacher.email", "Class.code"))
  df <- merge(df, student.info, all.x = T)
  df$Teacher.email <- tolower(df$Teacher.email)
  teacher.code.email$Teacher.email <- tolower(teacher.code.email$Teacher.email)
  df <- merge(df, teacher.code.email, all.x = T)
  # creating teacher name
  df$Teacher.name <- paste(df$Teacher.firstname, df$Teacher.surname)
  # Converting 'class' to 'subject' by removing fixed characters
  df$Subject <- gsub("Year ", "", df$Class)
  df$Subject <- gsub("\\s[A-Z]$", "", df$Subject)
  df$Subject <- gsub("^[0-9]\\s", "", df$Subject)
  df$Subject <- gsub("^[0-9][0-9]\\s", "", df$Subject)
  # extracting class year
  df$Class.year <- as.numeric(gsub("[A-z]","", substr(df$Class.code, start = 1, stop = 2)))
  # extracting form year
  df$Year <- as.numeric(substr(df$Form, 11, 12))
  return(df)  
}

makeGoogleScriptUpload <- function(enrolmentData) {
  df <- plyr::rename(enrolmentData, c("Student.code" = "student_code", 
                                      "Subject" = "subject",
                                      "Firstname" = "student_firstname",
                                      "Surname" = "student_surname",
                                      "Year" = "student_year",
                                      "Student.email" = "student_email",
                                      "Class.code" = "class_code",
                                      "Class.year" = "class_year",
                                      "Teacher.code" = "teacher_code",
                                      "Teacher.name" = "teacher_name"
                                      ))
  return(df)
}

makeEffortTrackingUpload <- function(enrolmentData) {
  # Needs the following: 
  # stuff
}

######################################################
# Reading csv data
edumateData <- read.csv(edumate.data.path, col.names = c("Student.code", "Student.email", "Firstname","Lastname","Student.name", "Sort.name","House","Gender","Form","Tutor.group","Reports.email"))
studentInfo <- edumateData[,c("Student.code","Student.name","House","Form","Tutor.group")]
teacherCodeEmail <- read.csv(teacher.code.email.path, col.names = c("Teacher.code", "Teacher.email"))
enrolmentData <- getEnrolmentData(enrolment.path = edumate.enrolment.path, student.info = studentInfo, teacher.code.email = teacherCodeEmail)

############# Removing stupid stuff, adding missing stuff ##################
enrolmentData <- enrolmentData[!(enrolmentData$Class.code %in% c("12ENX2.L","12ENX2.X","11MAT.X","12MAT.X")),]
enrolmentData[enrolmentData$Teacher.name == "William Braddock", "Teacher.code"] <- "BRAD"
enrolmentData[enrolmentData$Teacher.name == "Iain Maitland", "Teacher.code"] <- "MAI"

############## Making google upload ##################
googleUpload <- makeGoogleScriptUpload(enrolmentData)
googleUpload <- googleUpload[order(googleUpload$student_surname, googleUpload$student_surname),]
write.csv(googleUpload, file = paste0(datdir, "googleUpload2018term2.csv"), row.names = F)

####################################
# Importing student data ###########
studentResponses <- jsonlite::fromJSON(student.data.path, flatten = T)
studentResponses <- plyr::rename(studentResponses, c("student_code"="Student.code",
                                               "subject"="Subject",
                                               "diligence"="Diligence",
                                               "engagement"="Engagement",
                                               "behaviour"="Behaviour",
                                               "time_submitted"="Timestamp",
                                               "empty"="empty"))
studentResponses$Student.code <- as.numeric(studentResponses$Student.code)

# Getting list of missing students
missingStudentData <- merge(studentResponses[studentResponses$empty == TRUE,],studentInfo, all.x = T)
missingStudentData <- subset(missingStudentData, select = c("Student.name","House","Form","Tutor.group","empty"))
missingStudentData <- missingStudentData[complete.cases(missingStudentData),]

# Working with complete student data
studentData <- studentResponses[!(is.na(studentResponses$Diligence)),]
studentData$empty <- NULL
studentData <- tidyJsonData(studentData)
studentData$Source <- "Student"
studentData <- merge(studentData, subset(enrolmentData, select = c("Student.code", "Subject", "Class.code","Teacher.code")), all.x = TRUE) 
####################################

####################################
# Importing teacher data ###########
teacherResponses <- jsonlite::fromJSON(teacher.data.path, flatten = T)
missingTeacherData <- teacherResponses[is.na(teacherResponses$student),c("teacher_code","class_code")]
missingTeacherData <- missingTeacherData[missingTeacherData$teacher_code != "BULL",] # teacher ill, not completing this cycle
teacherResponses <- plyr::rename(teacherResponses, c("teacher_code"="Teacher.code",
                                                     "subject"="Subject",
                                                     "diligence"="Diligence",
                                                     "engagement"="Engagement",
                                                     "behaviour"="Behaviour",
                                                     "time_submitted"="Timestamp",
                                                     "empty"="empty",
                                                     "student"="Student.name",
                                                     "class"="Class.code",
                                                     "class_code"="Missing.class"))
teacherResponses <- merge(teacherResponses, studentInfo[,c("Student.name","Student.code")], all.x = TRUE)
# getting latest teacher responses, making "teacherResponses" df
teacherResponses <- teacherResponses[,!(names(teacherResponses) %in% c("Missing.class","empty"))]
df <- unique(data.frame("Student.code" = teacherResponses$Student.code,
                        "Class.code" = teacherResponses$Class.code,
                        "Teacher.code" = teacherResponses$Teacher.code,
                        "Diligence" = teacherResponses$Diligence,
                        "Engagement" = teacherResponses$Engagement,
                        "Behaviour" = teacherResponses$Behaviour,
                        "Timestamp" = teacherResponses$Timestamp))
                        
teacherData <- tidyJsonData(df)
teacherData$Source <- "Teacher"
teacherData <- merge(teacherData, subset(enrolmentData, select = c("Student.code", "Subject", "Class.code","Teacher.code")), all.x = TRUE) 

#####################################################
##               CLEANING                         ##
####################################################
######### Changing staff data #######################
teacherData[(teacherData$Student.code == 2066 & teacherData$Class.code == "12MAG2.O" & teacherData$Category == "Diligence"),]$Score <- 4
teacherData[(teacherData$Student.code == 7206 & teacherData$Class.code == "12MAG2.O" & teacherData$Category == "Diligence"),]$Score <- 2


# Ignoring the code above
effort.data <- unique(rbind(teacherData, studentData))
# effort.data[effort.data$Teacher.code == "NA","Teacher.code"] <- NA
# adding some useful fields with enrolment data
effort.data <- merge(effort.data, enrolmentData[,c("Class.code","Teacher.code","Student.code","Student.name")], all.x = T)
effort.data <- merge(effort.data, studentInfo[,c("Student.code","Student.name")],all.x = T)
effort.data$Date <- as.Date(effort.survey.date)
## fixing missing teacher code
effort.data[effort.data$Class.code %in% c("9BIG.L","10CRN.L","12MHI.O","11MHI.O","12MHI.X","11MHI.X"), "Teacher.code"] <- "BRAD"
effort.data[effort.data$Class.code == "12MAX2.O","Teacher.code"] <- "MAI"
## fixing subject names
effort.data[grepl("Chemistry",effort.data$Subject),"Subject"] <- "Chemistry"
effort.data[grepl("Legal",effort.data$Subject),"Subject"] <- "Legal Studies"
effort.data[grepl("CRN.",effort.data$Class.code),"Subject"] <- "Cornerstone"
effort.data[grepl("MHI.",effort.data$Class.code),"Subject"] <- "Modern History"
effort.data[grepl("BIG.",effort.data$Class.code),"Subject"] <- "Big History"
effort.data[grepl("12MAX2.",effort.data$Class.code),"Subject"] <- "Mathematics Extension 2"
## removing data not reported on
effort.data <- effort.data[!(effort.data$Student.code %in% c(21080,7072,23891)),]
effort.data <- effort.data[!(effort.data$Student.code == 2649 & effort.data$Subject == "French"),]

# Checking subject list
sub_miss <- setdiff(unique(effort.data$Subject), subject.order.list)
if (length(sub_miss) == 0) {
  print('Subjects all in subject.order.list')
} else {
  print('Subjects needed to be added to subject.order.list in effort.functions.R:')
  print(sub_miss)
}

# Extrating old effort data for timelines
old_effort_files <- list.files(past.effort.path, pattern = "*.csv", full.names = T)
past.effort.data <- do.call(rbind,lapply(old_effort_files, read.csv))
past.effort.data$Date <- as.Date(past.effort.data$Date)

all.effort.data <- rbind(past.effort.data, effort.data)
effort.data.wide <- effLongToWide(effort.data)
#View(effort.data.wide[!(complete.cases(effort.data.wide)),])

write.csv(effort.data, file = paste0(datdir, "2018 Term 2 Effort Data.csv"), row.names = F)

# fixing stuff
l <- list.files(paste0(report.dir, "/Student Reports"))
missing <- c()
for (n in unique(effort.data$Student.name)) {
  if (sum(grepl(n, l)) > 0) {
    print(n)
  } else {
    missing <- c(missing, n)
  }
  }