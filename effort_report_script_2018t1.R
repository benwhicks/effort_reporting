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
source("effort.functions.R")

# Reading in effort data file called '2018 Term 1 Effort Data.csv' or similar
# Data should have the following fields:
#   Student.code
#   Student.name
#   Teacher.code
#   Class.code
#   Subject
#   Category 
#   Score
#   Source
#   Date

# The following code will need to be edited each time the reports are run, to
# identify the folder and file to use

###############################################################
########## Reading in data, setting parameters ################
###############################################################
datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2018 Term 1/"
pastEffortPath <- paste0(datdir,"Past Effort Data/")
effortPath <- paste0(datdir,"2018 Term 1 Effort Data.csv")
# Reading edumate data
# Edumate data must have the following fields:
# Student.code, Email.student, Email.carers, Student.name
edumate.data.path <- paste0(datdir, "edumate_student_data_180312.csv")
# Teacher mail data needs to have teacher code and email, called 
teacher.mail.data.path <- paste0(datdir, "teacherCodeAndEmail.csv")

reporting.date <- as.Date("2018-03-12")
reportingPeriod <- "Term 1 2018"
sendMail <- FALSE # change to false if you want to generate the reports but not send the emails

# Reading in the data
effort.data <- read.csv(effortPath)
old_effort_files <- list.files(pastEffortPath, pattern = "*.csv", full.names = T)
past.effort.data <- do.call(rbind,lapply(old_effort_files, read.csv))
ednames <- c("Student.code","Subject","Score","Category","Source","Class.code","Teacher.code","Date")
past.effort.data$Date <- as.Date(past.effort.data$Date)
effort.data$Timestamp <- NULL  # may not be needed
all.effort.data <- rbind(past.effort.data[,names(past.effort.data) %in% ednames], 
                         effort.data[,names(effort.data) %in% ednames])
all.effort.data$Source <- factor(all.effort.data$Source, levels = c("Teacher", "Student"))
all.effort.data <- merge(all.effort.data, student.info[,c("Student.code","Student.name")], all.x = T)

# Other parameters
mail_subject <- paste0("Effort Report ",reportingPeriod)
mail_teacherBody <- paste0("Attached is your effort report for ",reportingPeriod ,".\nThanks,\nBen")
mail_studentBody <- paste0("Attached is your effort report for ",reportingPeriod ,".\n")
  
#mailData <- read.csv(edumate.data.path)
report.dir <- paste0(datdir,"Reports")
student.numbers <- unique(effort.data$Student.code)
teacher.codes <- unique(effort.data$Teacher.code)
teacher.codes <- teacher.codes[!(is.na(teacher.codes))]
teacher.codes <- teacher.codes[!(teacher.codes == "NT")]
student.info <- read.csv(edumate.data.path, col.names = c("Student.code", "Student.email", "Firstname","Lastname","Student.name","House","Gender","Form","Tutor.group","Reports.email"))
teacher.info <- read.csv(teacher.mail.data.path)
########################################################
############## ending input stage ######################
########################################################

# Checking for student info
for (ID in student.numbers) {
  if (!(ID %in% unique(student.info$Student.code))) {
    print(paste0("No student info for ",ID))
  }
}

# Checking subject list
if (length(setdiff(unique(effort.data$Subject), subject.order.list)) > 0) {
  print("Subjects missing from subject.order.list: ")
  print(setdiff(unique(effort.data$Subject), subject.order.list))
}
# Creates a directory called 'reports' if it does not already exist
if (!dir.exists(report.dir)) {dir.create(report.dir)}

# Creating student reports  -change to student.numbers
for (ID in student.numbers[301:length(student.numbers)]) {
  s.name <- student.info[student.info$Student.code == ID,]$Student.name
  studentFileName <- paste0("Student_Effort_Report_", s.name , "_", Sys.Date(), ".pdf" )
  studentFilePath <- paste0(report.dir ,"/" , studentFileName)
  rmarkdown::render('student_effort_report_markdown.Rmd',
                    output_file = studentFileName,
                    output_dir = report.dir)
  if (sendMail) {
    # Do stuff to send mail
    mailTo <- as.character(student.info[student.info$Student.code == ID, ]$Student.email)
    mailCc <- strsplit(as.character(student.info[student.info$Student.code == ID, ]$Reports.email), split = "; ")
    if (mailCc == "") {print(paste0("Carer mail for ", s.name, " not found"))}
    mailSubject <- paste0("Effort report for ", s.name)
    mailBody <- paste0("Dear ", s.name,",\n\n",
                       mail_studentBody,
                       "Regards,\nBen Hicks")
    mailR::send.mail(from = "<octemp@oxley.nsw.edu.au>",
                     to = mailTo,
                     cc = mailCc,
                     bcc = c("<effort.grade@oxley.nsw.edu.au>"),
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
for (tcode in teacher.codes[45:length(teacher.codes)]) {
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

# Creating teacher semester review for reporting

y12classes <- unique(all.effort.data[grepl("12",all.effort.data$Class.code),]$Class.code)
effort.data.2018Y12S1 <- all.effort.data[all.effort.data$Date > as.Date('2017-09-01'), ]
effort.data.2018Y12S1 <- effort.data.2018Y12S1[effort.data.2018Y12S1$Class.code %in% y12classes,]
y12.teacher.codes <- unique(effort.data.2018Y12S1$Teacher.code)
sendMail <- TRUE

# Building spreadsheet version
for (tcode in y12.teacher.codes) {
  fcsvpath = paste0(report.dir,"/2018Y12S1_Effort_Summay_",tcode,".csv")
  sem.df <- effort.data.2018Y12S1[effort.data.2018Y12S1$Teacher.code == tcode,]
  sem.df <- unique(sem.df[sem.df$Source == "Teacher",])
  sem.df.temp <- sem.df %>% unite("index", c(Category, Date)) %>% spread(index, Score)
  nms <- names(sem.df.temp)
  score_index <- grepl("Diligence", nms) | grepl("Engagement", nms) | grepl("Behaviour", nms)
  sem.df.temp$Effort <- rowMeans(sem.df.temp[,score_index], na.rm = T)
  #sem.df.temp$Effort <- (sem.df.temp[,8] + sem.df.temp[,8] +sem.df.temp[,9] +sem.df.temp[,10] +sem.df.temp[,12] +sem.df.temp[,13])/6.0
  sem.df <- sem.df.temp[order(sem.df.temp$Class.code, sem.df.temp$Student.name),]
  write.csv(sem.df, file = fcsvpath, row.names = F)
  if (sendMail) {
    mailto <- paste0("<",teacher.info[teacher.info$Teacher.code == tcode,"Email"],">")
    tname <- gsub("<","",mailto)
    tname <- gsub("\\..*$", "", tname)
    mailR::send.mail(from = "<octemp@oxley.nsw.edu.au>", 
                     to = c(mailto),
                     cc = c("<effort.grade@oxley.nsw.edu.au>"),
                     subject = paste0("2018 semester 1 effort summary for ",tcode),
                     body = paste0("Dear ",tname,",\n\nA summary of what you recorded for effort reporting for Semester 1 Year 12 is attached.\n\nRegards,\nBen Hicks"),
                     smtp = list(host.name = "mail.oxley.nsw.edu.au",
                                 user.name = "octemp",
                                 passwd = "[t3mP1522]"),
                     attach.files = fcsvpath,
                     authenticate = TRUE,
                     send = TRUE)
  }
}


# Creating school reports
# --- Teacher effort headmaster list
head_list <- effort.data[effort.data$Source == "Teacher",]
head_list <- head_list %>% group_by(Student.code) %>% summarise(mean(Score))
names(head_list) <- c("Student.code", "Effort.average")
student.info.short <- subset(student.info, select = c("Student.code","Student.name","Gender","Cohort"))
head_list <- merge(head_list, student.info.short, all.x = T, by = "Student.code")
write.csv(head_list, file = paste0(datdir,"headmasters_list.csv"), row.names = F)

# End of year Summary of CAME awards
# Using effort.data.2017 created from above
came_list <- effort.data.2017[effort.data.2017$Source == "Teacher",]
teacher_means <- came_list[complete.cases(came_list),] %>% 
  group_by(Teacher.code) %>% summarise(Teacher.mean = mean(Score))
came_list_means <- came_list %>%
  group_by(Student.code, Subject, Teacher.code) %>% summarise(Effort.mean = mean(Score))
came_list_means <- merge(came_list_means, teacher_means, all.x = T)
came_list_means$Relative.effort.score <- came_list_means$Effort.mean - came_list_means$Teacher.mean
student.info.veryshort <- subset(student.info, select = c("Student.code", "Student.name", "Cohort"))
came_list_means <- merge(came_list_means, student.info.veryshort, all.x = T, by = "Student.code")
came_list_means <- unique(came_list_means)
write.csv(came_list_means, file = paste0(datdir, "2017 Effort Means.csv"), row.names = F)  
  
# Semester 2 summary for report writing
# Using effort.data.2017 created from above
library(tidyr)
library(dplyr)
s2_list <- effort.data.2017[effort.data.2017$Source == "Teacher" & effort.data.2017$Date > as.Date("2017-06-15"),]
s2_list_means <- s2_list %>%
  group_by(Student.code, Category) %>% summarise(Mean = mean(Score))
s2_list_means <- s2_list_means %>% tidyr::spread(Category, Mean)
s2_list_means$Effort <- rowMeans(s2_list_means[,c("Behaviour","Diligence","Engagement")])
s.info.s2 <- subset(student.info, select = c("Student.code","Student.name", "Cohort","LASTNAME", "Tutor...Roll.Class"))
s2_list_means <- merge(s2_list_means, s.info.s2, all.x = T, by = "Student.code")
s2_list_means <- unique(s2_list_means)
write.csv(s2_list_means, file = paste0(datdir, "2017S2 Effort Means.csv"), row.names = F)  

  
# Play
library(reshape2)
mathsub <- c("Mathematics", "Mathematics Extension 1", "Mathematics Extension 2", "Mathematics Extension", "Mathematics General")
engsub <- c("English", "English Advanced", "English Standard", "English Extension", "English Extension 1", "English Extension 2")
d <- dcast(all.effort.data, 
           Student.code + Subject + Category + Class.code + Date ~ Source, 
           value.var = "Score", 
           fun.aggregate = mean)
a_level = 0.03
g <- ggplot(data = d[d$Subject %in% mathsub,], aes(Teacher, Student, frame = Date)) + 
  geom_jitter(alpha = a_level, width = .25, height = .25, size = 3.5) + 
  geom_rug(alpha = a_level/2.0, position = "jitter") +
 # geom_density2d(alpha = 0.2, colour = "White", size = 2) +
  ggthemes::theme_tufte()
g + facet_grid(. ~ Date)

