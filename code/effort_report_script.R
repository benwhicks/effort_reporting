#######################################
###### Effort Report Script ###########
#######################################

# This is one of several files to generate the effort reports
# The files required are:
#    1. effort.functions.R
#    2. effort_report_script.R -- this file
#    3. student_effort_report_markdown.Rmd
#    4. teacher_effort_report_markdown.Rmd 

# Loading packages and functions
library(knitr)
library(rmarkdown)
library(markdown)
library(tidyverse)
library(rdata.oxley) # Effort data stored here now in new system.
source("code/effort.functions.R")

# Changeable fields
SURVEY_DATE <- '2019-06-03'
EXPORT_FILENAME <- '2019 Term 2 Effort Data.csv'
REPORTING_PERIOD <- '2019 Term 2'
REPORT_DIR <- file.path('~', 'Effort Reporting')
NEW_ALL_EFFORT_EXPORT_FILE <- "oxley.all.effort.wide_2019T2.csv"
MAIL_MERGE_FILE <- "mail_merge_2019T2.csv"

# Creates a directory called 'reports' if it does not already exist
if (!dir.exists(REPORT_DIR)) {dir.create(REPORT_DIR)}

# Reading in the data
data("effort_tracking")
effort.tracking.data <- effort_tracking %>% filter(Date >= max(Date)) # in the effort tracking form
past.effort.data <- effort_tracking %>% filter(Date < max(Date))
all.effort.data.wide <- effort_tracking
all.effort.data <- all.effort.data.wide %>%
  gather(key = Type,
         value = Score,
         c(Student.Diligence,Student.Engagement,Student.Behaviour,Teacher.Diligence,Teacher.Engagement,Teacher.Behaviour)) %>%
  mutate(Source = gsub("\\..*","",Type), 
         Category = str_to_title(gsub("^.*\\.", "", Type)))
all.effort.data$Type <- NULL

effort.tracking.data$Teacher.name <- paste(effort.tracking.data$TeacherFirstname, effort.tracking.data$TeacherSurname)

# changing to long format
effort.data <- effort.tracking.data %>% 
  gather(key = Type, 
         value = Score, 
         Student.Diligence:Teacher.Behaviour) %>% 
  mutate(Source = gsub("\\..*","",Type), 
         Category = str_to_title(gsub("^.*\\.", "", Type)))
effort.data$Type <- NULL

effort.data$Student.name <- paste(trim_pref_name(effort.data$StudentFirstname), effort.data$StudentSurname)


# Checking subject list
setdiff(unique(effort.data$Subject), subject.order.list)

# Other parameters
data("student_info")
mailData <- student_info
student.info <- mailData # used as student.info in pastoral_summary

student.numbers <- unique(effort.data$Student.code)
teachers <- unique(effort.data$Teacher.name) 


# making empty data frame for mail merge
mailMerge <- data.frame(To = character(),
                        Cc =character(),
                        Name = character(),
                        Attachment = character())

# Creating student reports  -change to student.numbers
for (ID in student.numbers[72:length(student.numbers)]) {
  s.name <- unique(effort.data[effort.data$Student.code == ID,]$Student.name)
  studentFileName <- paste0(ID,"___Student_Effort_Report_", s.name , "_", REPORTING_PERIOD, ".pdf" )
  studentFilePath <- paste0(REPORT_DIR,studentFileName)
  studentEmail <- unique(mailData[mailData$Student.code == ID,]$Student.email)
  reportsEmail <- unique(mailData[mailData$Student.code == ID,]$Report.email)
  mailMerge <- rbind(mailMerge,
                     data.frame(To = studentEmail, Cc = as.character(reportsEmail), Name = s.name, Attachment = studentFileName)
  )
  rmarkdown::render('markdown_templates/student_effort_report_markdown.Rmd',
                    output_file = studentFileName,
                    output_dir = REPORT_DIR,
                    quiet = TRUE)
  print(paste0("Progress at: ",which(student.numbers == ID)/length(student.numbers)))
}


write_csv(mailMerge, path = MAIL_MERGE_FILE)

# Creating teacher reports
# joining data with student info for gender
effort.data <- left_join(effort.data, student.info %>% select(Student.code, Gender))
for (tcode in teachers) {
  fn <- paste0("Teacher_Effort_Report_", tcode, "_", REPORTING_PERIOD, ".pdf" )
  fpath <- file.path(REPORT_DIR, fn)
  rmarkdown::render('markdown_templates/teacher_effort_report_markdown.Rmd',
                    output_file = fn,
                    output_dir = REPORT_DIR,
                    quiet = TRUE)
  print(paste0("Progress at: ",which(teachers == tcode)/length(teachers)))
}

# Creating semester review for reporting
dates <- sort(unique(all.effort.data$Date))
dates <- dates[(length(dates)-1):length(dates)]
for (teacher in teachers) {
  fn <- paste0(teacher, " Effort Summary.csv")
  fpath <- file.path(REPORT_DIR, fn)
  class_codes <- unique(effort.data[effort.data$Teacher.name == teacher,]$Class.code)
  df <- all.effort.data[all.effort.data$Date %in% dates,]
  df$Class.code <- gsub("[.]", " ", df$Class.code)
  df <- df[df$Class.code %in% class_codes, ]
  df <- df[df$Source == "Teacher",]
  df <- df %>% group_by(Student.name, Class.code) %>% summarise(Effort = mean(Score))
  write_csv(df, path = fpath)
}

# Creating school reports
pfname <- paste0("Pastoral Summary ", REPORTING_PERIOD, '.pdf')
pfpath <- file.path(REPORT_DIR, pfname)
rmarkdown::render('markdown_templates/pastoral_summary.Rmd',
                  output_file = pfname,
                  output_dir = REPORT_DIR
                  )

# Creating means summary
mfname <- paste0("Effort Means ", REPORTING_PERIOD, ".csv")
mfpath <- file.path(REPORT_DIR, mfname)
mean.efforts <- effort.data %>%
  group_by(Student.code, StudentSurname, Student.name, Gender, Cohort, Source) %>%
  summarise(Effort = mean(Score, na.rm = TRUE)) %>%
  spread(key = Source, value = Effort)
write_csv(mean.efforts, path = mfpath)

# Creating annual summary
mafname <- paste0("Effort Means ", REPORTING_PERIOD, ".csv")
mafpath <- paste0(REPORT_DIR, mafname)
current.student.info <- effort.data[,c("Student.code","Student.name","Gender","StudentSurname","Cohort")]
mean.efforts.annual <- merge(
  all.effort.data[all.effort.data$Date > as.Date("2018-01-01"),],
  current.student.info) %>%
  group_by(Student.code, Student.name, Gender, Cohort, Source) %>%
  summarise(Effort = mean(Score, na.rm = TRUE)) %>%
  spread(key = Source, value = Effort)
write_csv(mean.efforts.annual, path = mafpath)
