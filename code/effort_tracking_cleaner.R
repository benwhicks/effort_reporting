# Cleaning Effort Tracking data
library(stringr)


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

# Quick fix to get Y12 data
library(tidyr)
library(dplyr)
y12td <- ed[ed$Cohort == "2018 Year 12" & ed$Source == "Teacher",]
oldDat <- read.csv(file.choose())
oldDat <- plyr::rename(oldDat, replace = c("Student.code" = "StudentID"))
oldDat <- oldDat[oldDat$StudentID %in% y12td$StudentID & oldDat$Source == "Teacher",]
y12semester <- rbind(oldDat[,c("StudentID","Score")], y12td[,c("StudentID","Score")])
y12sum <- y12semester %>% group_by(StudentID) %>% summarise(Effort = mean(Score)) 
y12sum <- unique(merge(y12sum, y12td[,c("StudentID","StudentFirstname","StudentSurname")]))

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
