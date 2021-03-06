# Set up
source("effort.functions.R")
datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2017 Term 4/"
edCleanPath <- paste0(datdir,"edvalCleanData2017t4.csv")

######################################################
######################################################
# Reading in Raw files

#y12raw <- read.csv(paste0(datdir,"Year 12 Effort Survey.2.csv"))
#y11raw <- read.csv(paste0(datdir,"Year 11 Effort Survey.csv"))
#s5raw <- read.csv(paste0(datdir,"Year 9 and 10 Effort Survey.csv"))
#s4raw <- read.csv(paste0(datdir,"Year 7 and 8 Effort Survey.csv"))
#teacherRaw <- jsonlite::fromJSON(paste0(datdir,"teacher_responses.json"), flatten = T)
#teacherRaw1 <- jsonlite::fromJSON(paste0(datdir,"teacher_responses_old.json"), flatten = T)
#teacherRaw2 <- jsonlite::fromJSON(paste0(datdir,"teacher_responses_old3.json"), flatten = T)
edvalData <- read.csv(edCleanPath)

######################################################
######################################################
######################################################

studentDetails <- get.student.details(edCleanPath)

tdat <- cleanTeacherJson(jsonlite::fromJSON(paste0(datdir,"teacher_responses.json"), flatten = T))
effort.data <- getEffortData(datdir)
#old_effort_files <- list.files(pastEffortPath, pattern = "*.csv", full.names = T)
#past.effort.data <- do.call(rbind,lapply(old_effort_files, read.csv))
#past.effort.data$Date <- as.Date(past.effort.data$Date)

tutorGroups <- read.csv(paste0(datdir,"tutorGroups2017t4.csv"))
tutorGroups <- data.frame(Student.code = tutorGroups$Student.code, 
                          Tutor.group = tutorGroups$Subject, 
                          Tutor = tutorGroups$Teacher.name)
missingStudents <- merge(getMissingStudentData(), tutorGroups)
missingTeachers <- getMissingTeacherData()
missingStudentsShortList <- unique(data.frame(
  Student = missingStudents$Student.name, 
  TutorGroup = missingStudents$Tutor.group,
  Tutor = missingStudents$Tutor))
missingStudentsShortList <- missingStudentsShortList[order(missingStudentsShortList$TutorGroup),]
#effort.data <- read.csv(paste0(datdir, "2017 Term 3 Effort Data.csv"))

#all.effort.data <- rbind(past.effort.data, effort.data[,!names(effort.data) %in% c("Student.name")])
#d <- all.effort.data[all.effort.data$Student.code == 1963,]
#effort.data <- getEffortData()
#effort.data.wide <- effLongToWide(effort.data)

# Reminders
print("Billy Brittain maths check, not in BM class. Needs D - VG, E - O, B - O")
print("Add Archie Waters effort to 9MUS.O from SIND, might need to add Archie's as well")

write.csv(effort.data, file = paste0(datdir, "2017 Term 4 Effort Data - WORKING.csv"), row.names = F)

# Looking at timeline of effort distribution
library(ggplot2)
sifp <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2017 Term 3/StudentInfo.csv"
si <- read.csv(sifp)
df <- merge(all.effort.data, si[,names(si) %in% c("Student.code","Gender","Cohort")])
g <- ggplot(df, aes(Score)) + 
  geom_histogram(binwidth = 1, position = "dodge") 
g + facet_grid(Cohort ~ Source)+ ggthemes::theme_tufte()

g2 <- ggplot(df[df$Source == "Teacher",], aes(Score)) + geom_histogram(binwidth = 1)
g2 + facet_wrap(~ Teacher.code) + ggthemes::theme_tufte()

humanities_sub_list <- c("Global Perspectives", "Geography", "History Extension", "History Modern", "History", "Economics", "Business Studies", "History Ancient", "Commerce", "Studies of Religion 1", "Studies of Religion 2", "Legal Studies")
maths_sub_list <- c("Mathematics", "Mathematics Extension", "Mathematics Extension 1", "Mathematics Extension 2", "Mathematics General")
english_sub_list <- c("English", "English Standard", "English Advanced", "English Extension", "English Extension 1", "English Extension 2")
science_sub_list <- c("Science", "Science Compaction", "Biology", "Chemistry", "Physics")
df_humanities <- df[df$Subject %in% humanities_sub_list,]
g_humanities <- ggplot(df_humanities[df_humanities$Source == "Teacher",], aes(Score)) +
  geom_histogram(binwidth = 1, position = "dodge")
g_humanities + facet_wrap(~ Teacher.code)

g_department <- function(df,sub_list) {
  require(ggplot2)
  df.temp <- df[df$Source == "Teacher",]
  df.temp <- df.temp[df.temp$Subject %in% sub_list,]
  g_dep <- ggplot(df.temp, aes(Score)) + geom_histogram(binwidth = 1) + facet_wrap(~ Teacher.code) + ggthemes::theme_tufte()
  return(g_dep)
}