#######################################
###### Effort Report Script ###########
#######################################

# This is one of four files to generate the effort reports
# The files required are:
#    1. effort.functions.R
#    2. effort_report_script.R
#    3. student_effort_report_markdown.Rmd
#    4. teacher_effort_report_markdown.Rmd 
#    5. school_effort_report_markdown.Rmd
#    6. effort_report_data.csv
#    7. student.names.csv   -- A csv file of student number to name conversions, fields are "Student.code" and "Student.name"
# The files need to be in the same folder. 

suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(markdown))
library(dplyr)
library(tidyr)
library(stringr)
library(plyr)
source("effort.functions.R")

# Changeable fields
SURVEYDATE <- '2018-08-27'
EXPORTFILENAME <- '2018 Term 3 Effort Data.csv'

# Paths to data files
datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2018 Term 3/"
pastEffortPath <- paste0(datdir,"Past Effort Data/")
effortPath <- paste0(datdir,"efforttracking_2018T3_final.csv")
mail.data.path <- paste0(datdir,"edumate_student_info_2018t3.csv")




# Reading in the data
effort.tracking.data <- read.csv(effortPath) # in the effort tracking form
effort.tracking.data$Teacher.name <- paste(effort.tracking.data$TeacherFirstname, effort.tracking.data$TeacherSurname)
# changing to long format

effort.data <- effort.tracking.data %>% 
  gather(key = Type, 
         value = Score, 
         Student_DILIGENCE:Teacher_BEHAVIOUR) %>% 
  mutate(Source = gsub("_.*","",Type), 
         Category = str_to_title(gsub("^.*_", "", Type)))
effort.data$Type <- NULL
effort.data$Subject <- gsub("Year\\s\\d+\\s","",effort.data$Course)
effort.data$Student.name <- paste(trim_pref_name(effort.data$StudentFirstname), effort.data$StudentSurname)
effort.data <- rename(effort.data, replace = c("StudentID" = "Student.code",
                                               "CLASS_CODE" = "Class.code"))
effort.data$Date <- as.Date(SURVEYDATE)
write.csv(effort.data, file = paste0(datdir, EXPORTFILENAME), row.names = F)

old_effort_files <- list.files(pastEffortPath, pattern = "*.csv", full.names = T)
past.effort.data <- do.call(rbind,lapply(old_effort_files, read.csv))
ednames <- c("Student.code","Student.name","Subject","Score","Category","Source","Class.code","Teacher.code","Date","Teacher.name")
past.effort.data$Date <- as.Date(past.effort.data$Date)

# note that all
all.effort.data <- rbind.fill(past.effort.data[,names(past.effort.data) %in% ednames], 
                         effort.data[,names(effort.data) %in% ednames])

# Checking subject list
setdiff()

# Other parameters
sendMail <- FALSE # change to false if you want to generate the reports but not send the emails
reportingPeriod <- "Term 3 2017"
mail_subject <- paste0("Effort Report",reportingPeriod)
mail_teacherBody <- paste0("Attached is your effort report for ",reportingPeriod ,".\nThanks,\nBen")
mail_studentBody <- paste0("Attached is your effort report for ",reportingPeriod ,".\n")
  
mailData <- read.csv(mail.data.path)
report.dir <- paste0(datdir,"Reports")
student.numbers <- unique(effort.data$Student.code)
teacher.codes <- unique(effort.data$Teacher.name) # changed to names, works the same
teacher.codes <- teacher.codes[!(is.na(teacher.codes))]
teacher.codes <- teacher.codes[!(teacher.codes == "NT")]
student.info <- read.csv(student.info.path)
teacher.info <- read.csv(teacher.mail.data.path) # this will be sourced from elsewhere

# Checking for student info
for (ID in student.numbers) {
  if (!(ID %in% unique(student.info$Student.code))) {
    print(paste0("No student info for ",ID, " ", getStudentName(ID)))
  }
}


# Creates a directory called 'reports' if it does not already exist
if (!dir.exists(report.dir)) {dir.create(report.dir)}

# Creating student reports  -change to student.numbers
for (ID in student.numbers) {
  s.name <- student.info[student.info$Student.code == ID,]$Student.name
  studentFileName <- paste0("Student_Effort_Report_", s.name , "_", Sys.Date(), ".pdf" )
  studentFilePath <- paste0(report.dir ,"/" , studentFileName)
  rmarkdown::render('student_effort_report_markdown.Rmd',
                    output_file = studentFileName,
                    output_dir = report.dir)
  if (sendMail) {
    # Do stuff to send mail
    mailTo <- as.character(mailData[mailData$Student.code == ID, ]$Email.student)
    mailCc <- strsplit(as.character(mailData[mailData$Student.code == ID, ]$Email.carers), split = "; ")
    mailSubject <- paste0("Effort report for ", s.name)
    mailBody <- paste0("Dear ", s.name,",\n\n",
                       mail_studentBody,
                       "Regards,\nBen Hicks")
    mailR::send.mail(from = "<octemp@oxley.nsw.edu.au>",
                     to = mailTo,
                     cc = mailCc,
                     bcc = c("effort.grade@oxley.nsw.edu.au"),
                     subject = mailSubject,
                     body = mailBody,
                     smtp = list(host.name = "mail.oxley.nsw.edu.au",
                                 user.name = "octemp",
                                 passwd = "[t3mP1522]"),
                     attach.files = studentFilePath,
                     authenticate = TRUE,
                     send = TRUE)
  }
  print(paste0("\nProgress at: ",which(student.numbers == ID)/length(student.numbers)))
}


# Creating teacher reports
for (tcode in teacher.codes) {
  fn <- paste0("Teacher_Effort_Report_", tcode, "_", Sys.Date(), ".pdf" )
  fpath <- paste0(report.dir, "/", fn)
  mailto <- paste0("<",teacher.info[teacher.info$Teacher.code == tcode,"Email"],">")
  rmarkdown::render('teacher_effort_report_markdown.Rmd',
                    output_file = fn,
                    output_dir = report.dir)
  if (sendMail) {
    mailR::send.mail(from = "<octemp@oxley.nsw.edu.au>",
              to = c(mailto),
              cc = c("<effort.grade@oxley.nsw.edu.au>"),
              subject = mail_subject,
              body = mail_teacherBody,
              smtp = list(host.name = "mail.oxley.nsw.edu.au",
                          user.name = "octemp",
                          passwd = "[t3mP1522]"),
              attach.files = fpath,
              authenticate = TRUE,
              send = TRUE)
  }
}

# Creating teacher term review for reporting
effort.data <- read.csv(effortPath)
effort.data <- effort.data[complete.cases(effort.data),]
year <- "11"
effort.data <-  effort.data[substr(effort.data$Class.code,1,2)==year,]
teacher.codes <- unique(effort.data$Teacher.code)
sendMail <- FALSE
for (tcode in teacher.codes) {
  fn <- paste0("Class_Effort_Average_Y11_2017T1_", tcode, "_", Sys.Date(), ".pdf" )
  fpath <- paste0(report.dir, "/", fn)
  rmarkdown::render('teacher_effort_averages.Rmd',
                    output_file = fn,
                    output_dir = report.dir)
  
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

# To be implemented