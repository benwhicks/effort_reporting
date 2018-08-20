# Set up
source("effort.functions.R")
datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2018 Term 1/"
effort.survey.date <- as.Date("2018-03-12")
edval.data.path <- paste0(datdir,"2018 Term 1 enrollment data.csv")
past.effort.path <- paste0(datdir, "Past Effort Data/")
edumate.data.path <- paste0(datdir, "edumate_student_data_180312.csv")
student.data.path <- paste0(datdir, "student_responses_10.json")
teacher.data.path <- paste0(datdir, "teacher_responses_05.json")

######################################################
######################################################
# Reading csv data
enrollmentData <- read.csv(edval.data.path, col.names = c("Student.code","Surname","Firstname","Student.year","Student.email","Class.code","Class.year","Teacher.code","Teacher.name","Subject"))
edumateData <- read.csv(edumate.data.path, col.names = c("Student.code", "Student.email", "Firstname","Lastname","Student.name","House","Gender","Form","Tutor.group","Reports.email"))
# Making some useful data frames
studentInfo <- edumateData[,c("Student.code","Student.name","House","Form","Tutor.group")]
enrollmentsShort <- unique(data.frame(Class.code = enrollmentData$Class.code, Subject = enrollmentData$Subject, Teacher.code = enrollmentData$Teacher.code, Student.code = enrollmentData$Student.code))
######################################################

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
studentResponses$Timestamp <- as.POSIXct(studentResponses$Timestamp, format = "%a %b %d %Y %H:%M:%S")
studentResponses$Student.code <- as.numeric(studentResponses$Student.code)

# Getting list of missing students
missingStudentData <- merge(studentResponses[studentResponses$empty == TRUE,],studentInfo, all.x = T)
missingStudentData <- subset(missingStudentData, select = c("Student.name","House","Form","Tutor.group","empty"))
missingStudentData <- missingStudentData[complete.cases(missingStudentData),]

# Working with complete student data
studentData <- studentResponses[!(is.na(studentResponses$Diligence)),]
studentData$empty <- NULL
studentDataLatest <- aggregate(Timestamp ~ ., studentData[, !(names(studentData) %in% c("Score"))], max)
studentData <- merge(studentDataLatest, studentData, all.x = T)
studentData$Timestamp <- NULL
studentData <- tidyJsonData(studentData)
studentData$Source <- "Student"
studentData <- merge(studentData, subset(enrollmentData, select = c("Student.code", "Subject", "Class.code","Teacher.code")), all.x = TRUE) 
####################################

####################################
# Importing teacher data ###########
teacherResponses <- jsonlite::fromJSON(teacher.data.path, flatten = T)
if ("empty" %in% names(teacherResponses)) {
  names(teacherResponses) <- c("Teacher.code", "Class.code", "Student.name", "Diligence", "Engagement", "Behaviour", "Timestamp", "Missing.Class.code", "empty")
  # Creating missing teacher data frame
  missingTeacherData <- subset(teacherResponses, teacherResponses$empty == TRUE, select = c("Teacher.code","Missing.Class.code","empty"))
  missingTeacherData <- missingTeacherData[!(is.na(missingTeacherData$Missing.Class.code)),]
  # Working with complete teacher data
  teacherData <- teacherResponses[is.na(teacherResponses$Missing.Class.code),] #stupid hack, empty != TRUE not working
  teacherData$empty <- NULL
  teacherData$Missing.Class.code <- NULL
} else {
  names(teacherResponses) <- c("Teacher.code", "Class.code", "Student.name", "Diligence", "Engagement", "Behaviour", "Timestamp")
  teacherData <- teacherResponses
}

teacherResponses$Timestamp <- as.POSIXct(teacherResponses$Timestamp, format = "%a %b %d %Y %H:%M:%S")
teacherData <- tidyJsonData(teacherData)
teacherData <- merge(teacherData,   # getting student number from the name
                          unique(data.frame(
                            Student.name = paste(enrollmentData$Firstname, enrollmentData$Surname), 
                            Student.code = enrollmentData$Student.code)),
                          by = "Student.name",
                          all.x = TRUE)
teacherDataLatest <- aggregate(Timestamp ~ ., teacherData[,!(names(teacherData) %in% c("Score"))], max)
teacherData <- merge(teacherDataLatest, teacherData, all.x = T)
teacherData$Timestamp <- NULL
teacherData$Source <- "Teacher"
teacherData$Student.name <- NULL # will add student name later from Edumate data
teacherData <- merge(teacherData, subset(enrollmentData, select = c("Student.code", "Subject", "Class.code","Teacher.code")), all.x = TRUE) 

######### Changing staff data #######################
teacherData[(teacherData$Student.code == 7155 & teacherData$Class.code == "9FRN.O" & teacherData$Category == "Engagement"),]$Score <- 4


######################################################
# Merging student and teacher data
effort.data <- rbind(teacherData, studentData)
effort.data$Date <- as.Date(effort.survey.date)
effort.data <- merge(effort.data, studentInfo[,c("Student.code","Student.name")],all.x = T)
######################################################


######### Removing incorrect data!! #################
removed <- subset(effort.data, (Student.code == 7043 & Subject == "French"))
effort.data <- subset(effort.data, !(Student.code == 7043 & Subject == "French"))
effort.data <- subset(effort.data, !(Student.code == 2552))
#####################################################



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

write.csv(effort.data, file = paste0(datdir, "2018 Term 1 Effort Data.csv"), row.names = F)

# Looking at timeline of effort distribution
library(ggplot2)
sifp <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2017 Term 3/StudentInfo.csv"
si <- read.csv(sifp)
df <- merge(all.effort.data, si[,names(si) %in% c("Student.code","Gender","Cohort")])
g <- ggplot(df, aes(Score)) + 
  geom_histogram(binwidth = 1, position = "dodge") 
g + facet_grid(Cohort ~ Source)+ ggthemes::theme_tufte()

g2 <- ggplot(effort.data[effort.data$Source == "Teacher",], aes(Score)) + geom_histogram(binwidth = 1)
g2 + facet_wrap(~ Teacher.code) + ggthemes::theme_tufte()

humanities_sub_list <- c("Global Perspectives", "Geography", "History Extension", "History Modern", "History", "Economics", "Business Studies", "History Ancient", "Commerce", "Studies of Religion 1", "Studies of Religion 2", "Big History","Legal Studies")
maths_sub_list <- c("Mathematics", "Mathematics Extension", "Mathematics Extension 1", "Mathematics Extension 2", "Mathematics General", "Mathematics Advanced", "Mathematics Standard")
english_sub_list <- c("English", "English Standard", "English Advanced", "English Extension", "English Extension 1", "English Extension 2")
science_sub_list <- c("Science", "Science Compaction", "Biology", "Chemistry", "Physics")
df_humanities <- df[df$Subject %in% humanities_sub_list,]

g_department <- function(df,sub_list) {
  require(ggplot2)
  df.temp <- df[df$Source == "Teacher",]
  df.temp <- df.temp[df.temp$Subject %in% sub_list,]
  g_dep <- ggplot(df.temp, aes(Score)) + geom_histogram(binwidth = 1) + facet_wrap(~ Teacher.code) + ggthemes::theme_tufte()
  return(g_dep)
}