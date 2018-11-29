# Analysis - Oxley

# importing and setting up data

library(tidyverse)
library(wesanderson)

trim_pref_name <- function(x) {
  x <- gsub( " *\\(.*?\\) *", "", x)
  return(x)
}

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
NEW_ALL_EFFORT_EXPORT_FILE <- "/home/hicks/Documents/Data Analysis/Oxley Effort Data/oxley.all.effort.data.wide.201811.csv"
PATH_TO_ALL_STUDENT_INFO <- "/home/hicks/Documents/Data Analysis/Oxley Effort Data/oxley.all.student.info.csv"
PATH_TO_EDUMATE_MAIL_DATA <- "/home/hicks/Documents/Data Analysis/Oxley Effort Data/2018 Term 4/edumate.student.data.201811.csv"
REPORT_DIR <- "~/Documents/Data Analysis/Oxley Effort Data/Reports/"
MAIL_MERGE_FILE <- "/home/hicks/Documents/Data Analysis/Oxley Effort Data/mail_merge_2018T4.csv"

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
effort.data$Student.name <- paste(trim_pref_name(effort.data$StudentFirstname), effort.data$StudentSurname)
effort.data <- plyr::rename(effort.data, replace = c("StudentID" = "Student.code",
                                                     "CLASS_CODE" = "Class.code"))
effort.data$Date <- as.Date(SURVEY_DATE)
# pulling back into wide format
effort.data.wide <- effort.data %>%
  unite(Temp, c(Source, Category), sep = ".") %>%
  spread(key = Temp, value = Score)

# merging the old with the new
ednames <- c("Student.code","Student.name","Subject","Class.code","Date","Student.Behaviour","Student.Diligence","Student.Engagement","Teacher.Behaviour","Teacher.Diligence","Teacher.Engagement")
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

# Checking subject list
setdiff(unique(effort.data$Subject), subject.order.list)

# Other parameters
mailData <- read_csv(PATH_TO_EDUMATE_MAIL_DATA)
student.info <- mailData # used as student.info in pastoral_summary
student.info$Cohort <- student.info$Form

students <- unique(all.effort.data[complete.cases(all.effort.data),c("Student.name","Student.code")])
students <- students[order(students$Student.name),]


effort.means <- all.effort.data %>% 
  group_by(Student.code, Student.name, Source, Date) %>% 
  summarise(Effort = mean(Score, na.rm = T))

effort.means.category <- all.effort.data %>% 
  group_by(Student.code, Student.name, Source, Category, Date) %>% 
  summarise(Effort = mean(Score, na.rm = T))

# School Assessment Data
assessment.files <- list.files(PATH_TO_ALL_ACADEMIC_DATA_FOLDER, pattern = "*.csv", full.names = TRUE)
all.assessment.data <- do.call(rbind, lapply(assessment.files, readr::read_csv)) 
all.assessment.data$MARK_PERCENTAGE <- all.assessment.data$RAW_MARK / all.assessment.data$MARK_OUT_OF

# mucking around with assessment data
eff_merge <- effort.means
eff_merge$Type = "Effort"
eff_merge$Score <- eff_merge$Effort
eff_merge$Effort <- NULL
eff_merge$Student.name <- NULL
ach_merge <- select(all.assessment.data, 
                    Student.code = STUDENT_NUMBER,
                    Date = DUE_DATE,
                    Score = Z_SCORE)
ach_merge$Type = "Achievement"
ach_merge$Source = "Teacher"
eff_merge$Score <- (eff_merge$Score - mean(eff_merge$Score, na.rm = T))/sd(eff_merge$Score, na.rm = T)
ach_v_eff <- bind_rows(ach_merge, eff_merge)
ach_v_eff$Date <- lubridate::round_date(ach_v_eff$Date, unit = "3 months")

# mucking around with graph - teacher ach v eff
achVeff <- ach_v_eff[ach_v_eff$Source == "Teacher",] %>% 
  group_by(Student.code, Date, Type) %>% 
  summarise(Score = mean(Score)) %>%
  spread(key = Type, value = Score)
achVeffGraphData <- merge(achVeff[complete.cases(achVeff),],   student.info[,c("Student.code", "Student.name", "Gender")]) 
gtest1 <- ggplot(data = achVeffGraphData, aes(x = Effort, y = Achievement, colour = Gender)) + 
  geom_point(alpha = 0.2) + 
  scale_colour_manual(values = wes_palette("Darjeeling1")) +
  theme_minimal() +
  geom_rug(alpha = 0.1) + 
  geom_smooth(alpha = 0.2, size = 0.4, method = "loess") + 
  ggtitle("Teacher reported effort vs achievement")

# mucking around with graph - student eff v ach
achVeffStudent <- ach_v_eff[ach_v_eff$Type == "Achievement" | ach_v_eff$Source == "Student", ] %>%
  group_by(Student.code, Date, Type) %>%
  summarise(Score = mean(Score)) %>%
  spread(key = Type, value = Score)
achVeffStudent <- merge(achVeffStudent[complete.cases(achVeffStudent),],   student.info[,c("Student.code", "Student.name", "Gender")]) 
gtest2 <- ggplot(data = achVeffStudent, aes(x = Effort, y = Achievement, colour = Gender)) + 
  geom_point(alpha = 0.2) + 
  scale_colour_manual(values = wes_palette("Darjeeling1")) +
  theme_minimal() +
  geom_rug(alpha = 0.1) + 
  geom_smooth(alpha = 0.2, size = 0.4, method = "loess") + 
  ggtitle("Student reported effort vs achievement")

# mucking around with teacher v student
em <- merge(effort.means, student.info[,c("Student.code", "Form", "Gender")])
gtest3 <- ggplot(data = em[em$Form != "2017 Year 12",], aes(Effort, colour = Gender, linetype = Source)) +
  geom_density(bw = 0.2) +
  scale_color_manual(values = wes_palette("Royal1")) +
  scale_x_continuous(limits = c(1,5), labels = c("Unsatisfactory","Fair","Good","Very Good","Outstanding")) +
  theme_minimal() +
  ggtitle("Distribution by Cohort") +
  facet_grid(Form ~ .)
#ggsave("~/Desktop/cohort effort teacher student dist.png", gtest3)


# Getting course marks for each course per year level
# Focusing on 11 to 12 mathematics (data too messy)
course.assessment.data <- all.assessment.data[all.assessment.data$DUE_DATE > "2016-01-01",]
maths.assessment.data <- course.assessment.data[grepl("Math", course.assessment.data$COURSE),]

maths.assessment.data <- maths.assessment.data[
  0 < maths.assessment.data$WEIGHTING & 
    maths.assessment.data$WEIGHTING < 100 & 
    !(is.na(maths.assessment.data$WEIGHTING)),
  ]
maths.assessment.data[grepl("Half|half|mid|Mid", maths.assessment.data$TASK),] <- NULL
maths.assessment.data$WEIGHTED_MARK <- maths.assessment.data$WEIGHTING * maths.assessment.data$MARK_PERCENTAGE
maths.coursemarks <- maths.assessment.data[grepl("12|11",maths.assessment.data$FORM_RUN),] %>%
  group_by(STUDENT_NUMBER, STUDENT_FIRSTNAME, STUDENT_SURNAME, FORM_RUN, COURSE) %>%
  summarise(COURSE_MARK = sum(WEIGHTED_MARK), TOTAL_WEIGHT = sum(WEIGHTING))
# Getting rid of non 100 weights and subsetting
maths.marks <- maths.coursemarks[maths.coursemarks$TOTAL_WEIGHT == 100,]
maths.marks$FORM_RUN <- gsub("201..Year\\s","",maths.marks$FORM_RUN)
maths.marks$COURSE <- gsub("Year .. ","", maths.marks$COURSE)
maths.marks$COURSE <- gsub("General 2", "General", maths.marks$COURSE)
# Spread based on course
maths.marks <- maths.marks %>% spread(key = FORM_RUN, value = COURSE_MARK)
fitm <- lm(`12` ~ `11` ,data = maths.marks)
g_mathsHSCprogress <- ggplot(data = maths.marks, 
                             aes(x = `11`, y = `12`, color = COURSE)) + 
  geom_point() + 
  geom_rug() +
  geom_smooth(inherit.aes = FALSE, aes(x = `11`, y = `12`), 
              color = 'black', 
              size = 0.2,
              method = "lm") +
  scale_x_continuous(limits = c(0,100)) + scale_y_continuous(limits = c(0,100)) +
  scale_colour_manual(values = wes_palette("Rushmore1")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dotted",
                                        size = 0.2, 
                                        colour = "light gray")) +
  ggtitle("HSC Mathematics: Year 11 v Year 12 Marks")

all.engagement <- all.effort.data
all.engagement <- merge(all.engagement, unique(student.info[,c("Student.code","Form")]))
all.engagement <- all.engagement[all.engagement$Category == "Engagement" & all.engagement$Source == "Student",]
# making curriculum areas
all.engagement$Department <- all.engagement$Subject

subject_to_department <- function(x) {
  # Removing extension suffix, numeric suffic, stream suffix
  # This should sort Mathematics and English subjects
  x <- gsub(" 1| 2", "", x)
  x <- gsub(" Extension", "", x)
  x <- gsub(" Advanced", "", x)
  x <- gsub(" Standard", "", x)
  x <- gsub(" General", "", x)
  x <- gsub(" Continuers", "", x)
  # Technology department
  x <- gsub("Graphics |Textiles |Design and |Food |Media ", "", x)
  x <- gsub("Engineering Studies", "Technology", x)
  # Science department
  x <- gsub("Biology|Chemistry|Physics|Science Compaction","Science",x)
  # Humanities
  x <- gsub("History Modern|Geography|Commerce|Big History|Modern History|History Ancienct|Ancient History", "Humanities", x)
  x <- gsub("History|Economics|Legal Studies|Studies of Religion|Business Studies","Humanities", x)
  # Arts
  x <- gsub("Music|Visual Arts|Drama", "Arts", x)
  # Distinctives
  x <- gsub("Cornerstone|SWYM|Global Perspectives|Wide Reading", "Distinctives", x)
  # Other
  x <- gsub("PDHPE|French|French|TVET|Distance Education|EXT", "Other", x)
  return(x)
}

all.engagement$Department <- subject_to_department(all.engagement$Department)
all.engagement.means <- all.engagement[all.engagement$Department != "Other",] %>% group_by(Department, Date, Form) %>% summarise(Engagement = mean(Score, na.rm = T))
g.engagement <- ggplot(data = all.engagement.means[all.engagement.means$Form %in% c("2018 Year 07","2018 Year 08","2018 Year 09","2018 Year 10"),], 
                       aes(x = Date, y = Engagement, color = Department, group = Department)) +
  geom_line() + facet_grid(Form ~ Department) + theme_minimal()
g.engagement + ggtitle("Student Reported Engagement by Form and Department")