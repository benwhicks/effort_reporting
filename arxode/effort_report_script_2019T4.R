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
library(rdata.oxley) # Effort data stored here now in new system.
source("code/effort.functions.R")

# Changeable fields
SURVEY_DATE <- '2019-03-11'
EXPORT_FILENAME <- '2019 Term 1 Effort Data.csv'
REPORTING_PERIOD <- '2019 Term 1'
PATH_TO_NEW_EFFORT_DATA <- "D:\\Users\\bhicks\\Documents\\Data\\Oxley\\2019 Term 1 Effort Reporting\\Term 1 2019 Effort Grade Data Complete.csv"
PATH_TO_ALL_EFFORT_DATA <- "D:\\Users\\bhicks\\Documents\\Data\\Oxley\\2019 Term 1 Effort Reporting\\oxley.all.effort.wide_end2018.csv"
#PATH_TO_ALL_STUDENT_INFO <- Depreciated, to be included in all effort csv
PATH_TO_EDUMATE_MAIL_DATA <- "D:\\Users\\bhicks\\Documents\\Data\\Oxley\\2019 Term 1 Effort Reporting\\edumate_student_details_2019T1.csv"
REPORT_DIR <- "D:\\Users\\bhicks\\Documents\\Data\\Oxley\\2019 Term 1 Effort Reporting\\"
NEW_ALL_EFFORT_EXPORT_FILE <- "oxley.all.effort.wide_2019T1.csv"
MAIL_MERGE_FILE <- "mail_merge_2019T1.csv"

# Creates a directory called 'reports' if it does not already exist
if (!dir.exists(REPORT_DIR)) {dir.create(REPORT_DIR)}

# Reading in the data
effort.tracking.data <- read_csv(PATH_TO_NEW_EFFORT_DATA) # in the effort tracking form
past.effort.data <- read_csv(PATH_TO_ALL_EFFORT_DATA)

effort.tracking.data$Teacher.name <- paste(effort.tracking.data$TeacherFirstname, effort.tracking.data$TeacherSurname)

# changing to long format and tidying up
effort.data <- effort.tracking.data %>% 
  gather(key = Type, 
         value = Score, 
         Student_DILIGENCE:Teacher_BEHAVIOUR) %>% 
  mutate(Source = gsub("_.*","",Type), 
         Category = str_to_title(gsub("^.*_", "", Type)))
effort.data$Type <- NULL
effort.data$Subject <- gsub("Year\\s\\d+\\s","",effort.data$CLASS)
effort.data$Subject <- gsub("\\s[OXLEY]$","", effort.data$Subject)
effort.data$Subject <- gsub("\\s10A","", effort.data$Subject)
effort.data$Subject <- gsub("\\s11A","", effort.data$Subject)
effort.data$Student.name <- paste(trim_pref_name(effort.data$StudentFirstname), effort.data$StudentSurname)
effort.data <- plyr::rename(effort.data, replace = c("StudentID" = "Student.code",
                                                     "CLASS_CODE" = "Class.code"))
effort.data$Date <- as.Date(SURVEY_DATE)
# pulling back into wide format
effort.data.wide <- effort.data %>%
  unite(Temp, c(Source, Category), sep = ".") %>%
  spread(key = Temp, value = Score)

# merging the old with the new
ednames <- c("Student.code","StudentFirstname","StudentSurname","StudentEmail",
             "Subject", "Class.code",
             "Year", "Cohort", "Date",
             "TeacherFirstname","TeacherSurname", "TeacherEmail", 
             "Student.Behaviour","Student.Diligence","Student.Engagement",
             "Teacher.Behaviour","Teacher.Diligence","Teacher.Engagement")
past.effort.data$Date <- as.Date(past.effort.data$Date)
all.effort.data.wide <- plyr::rbind.fill(past.effort.data[,names(past.effort.data) %in% ednames], 
                         effort.data.wide[,names(effort.data.wide) %in% ednames])
all.effort.data <- all.effort.data.wide %>%
  gather(key = Type,
         value = Score,
         c(Student.Diligence,Student.Engagement,Student.Behaviour,Teacher.Diligence,Teacher.Engagement,Teacher.Behaviour)) %>%
  mutate(Source = gsub("\\..*","",Type), 
         Category = str_to_title(gsub("^.*\\.", "", Type)))
all.effort.data$Type <- NULL


# Exporting data
write_csv(effort.data.wide, path = paste0(REPORT_DIR, EXPORT_FILENAME))
write_csv(all.effort.data.wide, path = paste0(REPORT_DIR, NEW_ALL_EFFORT_EXPORT_FILE))



# Checking subject list
setdiff(unique(effort.data$Subject), subject.order.list)

# Other parameters
mailData <- read_csv(PATH_TO_EDUMATE_MAIL_DATA)
student.info <- mailData # used as student.info in pastoral_summary
student.info$Cohort <- student.info$Form

student.numbers <- unique(effort.data$Student.code)
teachers <- unique(effort.data$Teacher.name) 


# making empty data frame for mail merge
mailMerge <- data.frame(To = character(),
                        Cc =character(),
                        Name = character(),
                        Attachment = character())

# Creating student reports  -change to student.numbers
for (ID in student.numbers) {
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
  for (n in seq(1,2)) {print("...")} # just for spacing
  print(paste0("Progress at: ",which(student.numbers == ID)/length(student.numbers)))
}


write_csv(mailMerge, path = MAIL_MERGE_FILE)

# Creating teacher reports
for (tcode in teachers) {
  fn <- paste0("Teacher_Effort_Report_", tcode, "_", REPORTING_PERIOD, ".pdf" )
  fpath <- paste0(REPORT_DIR, fn)
  rmarkdown::render('markdown_templates/teacher_effort_report_markdown.Rmd',
                    output_file = fn,
                    output_dir = REPORT_DIR,
                    quiet = TRUE)
}

# Creating semester review for reporting
dates <- sort(unique(all.effort.data$Date))
dates <- dates[(length(dates)-1):length(dates)]
for (teacher in teachers) {
  fn <- paste0(teacher, " Effort Summary.csv")
  fpath <- paste0(REPORT_DIR, fn)
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
pfpath <- paste0(REPORT_DIR, pfname)
rmarkdown::render('markdown_templates/pastoral_summary.Rmd',
                  output_file = pfname,
                  output_dir = REPORT_DIR
                  )

# Creating means summary
mfname <- paste0("Effort Means ", REPORTING_PERIOD, ".csv")
mfpath <- paste0(REPORT_DIR, mfname)
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
