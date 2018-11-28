# Cleaning Effort Tracking data
library(stringr)
library(tidyverse)

message("Choose the csv file from the Effort Tracking export to clean")
f <- file.choose()
df <- read.csv(f)

eff.fields <- c("StudentID", "Course","CLASS","CLASS_CODE","Teacher.name"
                ,"Student_DILIGENCE"
                ,"Student_ENGAGEMENT"
                ,"Student_BEHAVIOUR"
                ,"Teacher_DILIGENCE"
                ,"Teacher_ENGAGEMENT"
                ,"Teacher_BEHAVIOUR")
sinf.fields <- c("StudentID","Gender","GenderID",
                 "StudentFirstname","StudentSurname","StudentEmail",
                 "YEAR","TutorGroup","Cohort")
class.fields <- c("CLASS_CODE","TeacherTitle",
                  "TeacherFirstname","TeacherSurname","TeacherEmail")

eff.df <- unique(df[, names(df) %in% eff.fields]) # effort
sinf.df <- unique(df[, names(df) %in% sinf.fields]) # student info 
class.df <- unique(df[, names(df) %in% class.fields]) # class / teacher 

ed <- df %>% 
  gather(key = Type, 
         value = Score, 
         Student_DILIGENCE:Teacher_BEHAVIOUR) %>% 
  mutate(Source = gsub("_.*","",Type), 
         Category = str_to_title(gsub("^.*_", "", Type)))
ed$Type <- NULL
ed$Subject <- gsub("Year\\s\\d+\\s","",ed$Course)

write_csv(ed, path = "~/Documents/Data Analysis/Oxley Effort Data/2018 Term 4/2018 Term 4 Effort Data.csv")

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
