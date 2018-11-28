#######################################
###### Effort Report Script ###########
#######################################

# This is one of several files to generate the effort reports
# The files required are:
#    1. effort.functions.R
#    2. effort_report_script.R -- this file
#    3. student_effort_report_markdown.Rmd
#    4. teacher_effort_report_markdown.Rmd 
#    5. school_effort_report_markdown.Rmd
#    6. effort_report_data.csv

# Loading packages and functions
library(knitr)
library(rmarkdown)
library(markdown)
library(tidyverse)
source("code/effort.functions.R")

# Changeable fields
SURVEY_DATE <- '2018-11-19'
EXPORT_FILENAME <- '2018 Term 4 Effort Data.csv'
REPORTING_PERIOD <- '2018 Term 4'
PATH_TO_NEW_EFFORT_DATA <- "~/Documents/Data Analysis/Oxley Effort Data/2018 Term 4/efforttracking_20181127-0107.csv"
PATH_TO_ALL_EFFORT_DATA <- "~/Documents/Data Analysis/Oxley Effort Data/oxley.all.effort.data.wide.201809.csv"
NEW_ALL_EFFORT_EXPORT_FILE <- "oxley.all.effort.data.wide.201811.csv"
PATH_TO_ALL_STUDENT_INFO <- "/home/hicks/Documents/Data Analysis/Oxley Effort Data/oxley.all.student.info.csv"
PATH_TO_EDUMATE_MAIL_DATA <- "/home/hicks/Documents/Data Analysis/Oxley Effort Data/2018 Term 4/edumate.student.data.201811.csv"
REPORT_DIR <- "~/Documents/Data Analysis/Oxley Effort Data/Reports/"


# Creates a directory called 'reports' if it does not already exist
if (!dir.exists(REPORT_DIR)) {dir.create(REPORT_DIR)}

# Reading in the data
effort.tracking.data <- read_csv(PATH_TO_NEW_EFFORT_DATA) # in the effort tracking form
effort.tracking.data$Teacher.name <- paste(effort.tracking.data$TeacherFirstname, effort.tracking.data$TeacherSurname)






# Need to change this bit 



# changing to long format
effort.data.long <- effort.tracking.data %>% 
  gather(key = Type, 
         value = Score, 
         Student_DILIGENCE:Teacher_BEHAVIOUR) %>% 
  mutate(Source = gsub("_.*","",Type), 
         Category = str_to_title(gsub("^.*_", "", Type)))
effort.data.long$Type <- NULL








effort.data$Subject <- gsub("Year\\s\\d+\\s","",effort.data$CLASS)
effort.data$Subject <- gsub("\\s[OXLEY]$","", effort.data$Subject)
effort.data$Subject <- gsub("\\s10A","", effort.data$Subject)
effort.data$Student.name <- paste(trim_pref_name(effort.data$StudentFirstname), effort.data$StudentSurname)
effort.data <- plyr::rename(effort.data, replace = c("StudentID" = "Student.code",
                                               "CLASS_CODE" = "Class.code"))
effort.data$Date <- as.Date(SURVEY_DATE)
write_csv(effort.data, path = paste0(REPORT_DIR, EXPORT_FILENAME))

past.effort.data <- read_csv(PATH_TO_ALL_EFFORT_DATA)
ednames <- c("Student.code","Student.name","Subject","Class.code","Date","Student.Behaviour","Student.Diligence","Student.Engagement","Teacher.Behaviour","Teacher.Diligence","Teacher.Engagement")
past.effort.data$Date <- as.Date(past.effort.data$Date)

# note that all
all.effort.data <- plyr::rbind.fill(past.effort.data[,names(past.effort.data) %in% ednames], 
                         effort.data[,names(effort.data) %in% ednames])
write_csv(all.effort.data, path = paste0(REPORT_DIR, NEW_ALL_EFFORT_EXPORT_FILE))






# Checking subject list
setdiff(unique(effort.data$Subject), subject.order.list)

# Other parameters
mailData <- read_csv(PATH_TO_EDUMATE_MAIL_DATA)
student.info <- mailData # used as student.info in pastoral_summary

student.numbers <- unique(effort.data$Student.code)
teachers <- unique(effort.data$Teacher.name) 

# Creating student reports  -change to student.numbers
for (ID in student.numbers[1:5]) {
  s.name <- unique(effort.data[effort.data$Student.code == ID,]$Student.name)
  studentFileName <- paste0(ID,"___Student_Effort_Report_", s.name , "_", REPORTING_PERIOD, ".pdf" )
  studentFilePath <- paste0(REPORT_DIR ,"/" , studentFileName)
  rmarkdown::render('markdown_templates/student_effort_report_markdown.Rmd',
                    output_file = studentFileName,
                    output_dir = REPORT_DIR,
                    quiet = TRUE)
  print(paste0("\nProgress at: ",which(student.numbers == ID)/length(student.numbers)))
}


# Creating teacher reports
for (tcode in teachers) {
  fn <- paste0("Teacher_Effort_Report_", tcode, "_", REPORTING_PERIOD, ".pdf" )
  fpath <- paste0(REPORT_DIR, "/", fn)
  tmail <- unique(effort.data[effort.data$Teacher.name == tcode,]$TeacherEmail)
  mailto <- paste0("<",tmail,">")
  rmarkdown::render('markdown_templates/teacher_effort_report_markdown.Rmd',
                    output_file = fn,
                    output_dir = REPORT_DIR,
                    quiet = TRUE)
}

# Creating teacher term review for reporting - currently conflicts with 
# teacher.codes or teachers (names) 
effort.data <- read.csv(effortPath)
effort.data <- effort.data[complete.cases(effort.data),]
year <- "11"
effort.data <-  effort.data[substr(effort.data$Class.code,1,2)==year,]
teacher.codes <- unique(effort.data$Teacher.code)
sendMail <- FALSE
for (tcode in teacher.codes) {
  fn <- paste0("Class_Effort_Average_Y11_2017T1_", tcode, "_", REPORTING_PERIOD, ".pdf" )
  fpath <- paste0(REPORT_DIR, "/", fn)
  rmarkdown::render('teacher_effort_averages.Rmd',
                    output_file = fn,
                    output_dir = REPORT_DIR)
  
  if (sendMail) {
    mailto <- paste0("<",teacher.info[teacher.info$Teacher.code == tcode,"Email"],">")
    tname <- gsub("<","",mailto)
    tname <- gsub("\\..*$", "", tname)
    mailR::send.mail(from = "<octemp@oxley.nsw.edu.au>",
                     to = c(mailto),
                     cc = c("<effort.grade@oxley.nsw.edu.au>"),
                     subject = paste0("Year ",year," effort summary for ",tcode),
                     body = paste0("Dear ",tname,",\n\nA summary of what you recorded for the year 11 effort reporting for term 1 is attached.\n\nRegards,\nBen Hicks"),
                     smtp = list(host.name = "mail.oxley.nsw.edu.au",
                                 user.name = "octemp",
                                 passwd = "[t3mP1522]"),
                     attach.files = fpath,
                     authenticate = TRUE,
                     send = TRUE)
  }
}

# Creating school reports
pfname <- paste0("Pastoral Summary ", REPORTING_PERIOD, '.pdf')
pfpath <- paste0(REPORT_DIR, "/", pfname)
rmarkdown::render('markdown_templates/pastoral_summary.Rmd',
                  output_file = pfname,
                  output_dir = REPORT_DIR
                  )
# To be implemented