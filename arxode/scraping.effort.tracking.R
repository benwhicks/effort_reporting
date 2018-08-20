# Set up
source("effort.functions.R")
datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2018 Term 2/"
effort.survey.date <- as.Date("2018-06-04")
enrolment.data.path <- paste0(datdir,"edumate_enrolment_export_20180530.csv")
past.effort.path <- paste0(datdir, "Past Effort Data/")
student.info.data.path <- paste0(datdir, "edumate_student_data_180530.csv")
teacher.code.email.path <- paste0(datdir, "teacherCodeAndEmail.csv")

makeEffortTrackingUpload <- function(enrolment.data.path, 
                                     student.info.data.path, 
                                     output.file = "effort.tracking.upload.csv") {
  enrdat <- read.csv(enrolment.data.path)
  enrdat <- plyr::rename(enrdat,c("FORM_RUN" = "Cohort",
                         "STUDENT_NUMBER" = "Student.ID",
                         "FIRSTNAME" = "Student.Firstname",
                         "SURNAME" = "Student.Lastname",
                         "EMAIL_ADDRESS" = "Student.Email",
                         "CLASS" = "Class", 
                         "TEACHER_TITLE" = "Teacher.Title",
                         "TEACHER_FIRSTNAME" = "Teacher.Firstname",
                         "TEACHER_SURNAME" = "Teacher.Lastname",
                         "TEACHER_EMAIL" = "Teacher.Email",
                         "class.code" = "Class.Code"))
  enrdat$REG_NUMBER <- NULL
  studat <- read.csv(student.info.data.path)
  studat <- plyr::rename(studat, c("Student.." = "Student.ID",
                                   "Email.Address" = "Student.Email",
                                   "Firstname" = "Student.Firstname",
                                   "Lastname" = "Student.Lastname",
                                   "Firstname.Lastname" = "Student.Name",
                                   "LASTNAME.Firstname" = "Student.Sortname",
                                   "House" = "House",
                                   "Gender" = "Gender",
                                   "Form" = "Cohort",
                                   "Tutor...Roll.Class" = "Tutor.Group"))
  outdat <- merge(enrdat, studat[,c("Student.ID", "Student.Name", "Student.Sortname","House","Gender","Tutor.Group")], all.x = T, by = "Student.ID")
  outdat$Year <- as.numeric(gsub("[0-9]{4} Year ", "", outdat$Cohort))
  outdat$Tutor.Group <- gsub("Tutor Group ","", outdat$Tutor.Group)
  write.csv(outdat, file = output.file, row.names = FALSE)
}

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
