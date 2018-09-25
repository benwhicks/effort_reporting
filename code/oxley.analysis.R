# Analysis - Oxley

# importing and setting up data

library(tidyverse)
library(wesanderson)

trim_pref_name <- function(x) {
  x <- gsub( " *\\(.*?\\) *", "", x)
  return(x)
}

PATH_TO_ALL_EFFORT_DATA <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/oxley.all.effort.data.csv"
PATH_TO_ALL_STUDENT_INFO <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/oxley.all.student.info.csv"
PATH_TO_ALL_ACADEMIC_DATA_FOLDER <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/School-Assessments/Edumate Export/"


all.effort.data <- readr::read_csv(PATH_TO_ALL_EFFORT_DATA)
student.info <- readr::read_csv(PATH_TO_ALL_STUDENT_INFO)
all.effort.data$Student.name <- trim_pref_name(
  stringr::str_to_title(all.effort.data$Student.name))
all.effort.data$Source <- factor(all.effort.data$Source, levels = c("Student", "Teacher"))

# removing unwanted fields
all.effort.data$Teacher.code <- NULL
all.effort.data$Teacher.name <- NULL
student.info$Student.name <- trim_pref_name(
  stringr::str_to_title(student.info$Student.name)
)
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
course.assessment.data <- all.assessment.data
course.assessment.data
