datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2017 Term 3/"

# Tidy subject names first

edvalRawData <- read.csv(paste0(datdir, "edval enrollments 2017.08.22.csv"))
subjectsToRemove <- c("Academic Writing", "Assembly", "Community Time", 
                      "Dobell 1", "Dobell 2", "Dobell 3", "Dobell 4",
                      "Durack 1", "Durack 2", "Durack 3", "Durack 4",
                      "Florey 1", "Florey 2", "Florey 3", "Florey 4",
                      "Integrated Unit of Inquiry", 
                      "Mawson 1", "Mawson 2", "Mawson 3", "Mawson 4",
                      "Monash 1", "Monash 2", "Monash 3", "Monash 4",
                      "Oodgeroo 1", "Oodgeroo 2", "Oodgeroo 3", "Oodgeroo 4",
                      "Stepping Stones", "Singing")

edvalCleanData <- edvalRawData[!edvalRawData$Subject %in% subjectsToRemove,]
edvalCleanData <- edvalCleanData[edvalCleanData$Year %in% c(7,8,9,10,11),]
edvalData <- edvalCleanData

write.csv(edvalCleanData, file = paste0(datdir, "effortEdvalData2017t3WORKING.csv"), row.names = FALSE)

# making csv for survey output
edvalEffortForGoogle <- data.frame(student_code = edvalCleanData$Student.code,
                                   student_surname = edvalCleanData$Surname,
                                   student_firstname = edvalCleanData$First.name,
                                   student_year = edvalCleanData$Year,
                                   student_email = edvalCleanData$Email,
                                   class_code = edvalCleanData$Class.code,
                                   class_year = edvalCleanData$Class.year,
                                   teacher_code = edvalCleanData$Teacher.code,
                                   teacher_name = edvalCleanData$Teacher.name,
                                   subject = edvalCleanData$Subject)

edvalEffortForGoogle <- read.csv(paste0(datdir, "effortDataForGoogle2017t3.csv"))
edvalEffortForGoogle$student_firstname <- gsub( " *\\(.*?\\) *", "", edvalEffortForGoogle$student_firstname)
write.csv(edvalEffortForGoogle, file = paste0(datdir, "effortDataForGoogle2017t3.csv"), row.names = F)

############################ Post Survey ###########################
edvalData <- read.csv(paste0(datdir, "effortEdvalData2017t3WORKING.csv"))
edvalDataFull <- edvalRawData
studentDetails <- data.frame(Student.code = edvalDataFull$Student.code, 
                             Student.name = paste0(edvalDataFull$First.name, " ", edvalDataFull$Surname),
                             Email = edvalDataFull$Email)
studentDetails$Student.name <- gsub( " *\\(.*?\\) *", "", studentDetails$Student.name)
studentDetails <- unique(studentDetails)


######### Reading in survey data ##############
#y12raw <- read.csv(paste0(datdir,"Year 12 Effort Survey.2.csv"))
y11raw <- read.csv(paste0(datdir,"Year 11 Effort Survey.csv"))
s5raw <- read.csv(paste0(datdir,"Year 9 and 10 Effort Survey.csv"))
s4raw <- read.csv(paste0(datdir,"Year 7 and 8 Effort Survey.csv"))
teacherRaw <- jsonlite::fromJSON(paste0(datdir,"teacher_responses.json"), flatten = T)

##### dirty class check for teachers ######
teacherCompletionCheck <- function() {
  classCodes <- setdiff(unique(edvalData$Class.code),unique(teacherRaw$class))
  out <- unique(edvalData[,c("Class.code","Teacher.name")])
  out <- out[out$Class.code %in% classCodes,]
  out <- out[with(out, order(Teacher.name)),]
  return(out)
}

y12sn <- unique(edvalData[edvalData$Year == 12,]$Student.code)
y11sn <- unique(edvalData[edvalData$Year == 11,]$Student.code)
s5sn <- unique(edvalData[edvalData$Year == 10 | edvalData$Year == 9,]$Student.code)
s4sn <- unique(edvalData[edvalData$Year == 8 | edvalData$Year == 7,]$Student.code)
y12tc <- unique(edvalData[edvalData$Year == 12, ]$Teacher.code)

getMissingStudents <- function(num1,num2){
  miss.num <- setdiff(num1,num2)
  miss.students <- unique(edvalData[edvalData$Student.code %in% miss.num,1:4])
  return(miss.students)
}

getAllMissingStudents <- function(){
  #miss12 <- getMissingStudents(y12sn, y12raw$Student.code)
  miss11 <- getMissingStudents(y11sn, y11raw$Student.code)
  missS5 <- getMissingStudents(s5sn, s5raw$Student.code)
  missS4 <- getMissingStudents(s4sn, s4raw$Student.code)
  #out1 <- rbind(miss12,miss11)
  out2 <- rbind(missS5, missS4)
  out <- rbind(miss11, out2) # was out1 instead of miss11 
  out <- out[order(out$Surname),]
  return(out)
}

getStudentInfo <- function(ID = NA, name = NA) {
  if (!(is.na(ID))) {
    info <- edvalData[edvalData$Student.code == ID, 
                     names(edvalData) %in% c("Student.code", "Surname", "First.name",
                                             "Year", "Email")]
  }
  if (!(is.na(name))) {
    info <- edvalData[paste0(edvalData$First.name, " ", edvalData$Surname) == name, 
                      names(edvalData) %in% c("Student.code", "Surname", "First.name",
                                              "Year", "Email")]
  }
  return(unique(info))
}

getStudentEnrollments <- function(ID = NA, name = NA) {
  if (!(is.na(ID))) {
    info <- edvalData[edvalData$Student.code == ID, 
                      names(edvalData) %in% c("Student.code", "Surname", "First.name",
                                              "Year", "Class.code", "Subject", "Teacher.code")]
  }
  if (!(is.na(name))) {
    info <- edvalData[paste0(edvalData$First.name, " ", edvalData$Surname) == name, 
                      names(edvalData) %in% c("Student.code", "Surname", "First.name",
                                              "Year", "Class.code", "Subject", "Teacher.code")]
  }
  return(unique(info))
}

cleanStudentGoogleForm <- function(df) {
  # This is for cleaning the survey forms downloaded as csv 
  require(dplyr)
  require(tidyr)
  
  # Removing unwanted columns
  drop <- c("Timestamp", "For.admin.purposes")
  df.out <- df[, !(names(df) %in% drop)]
  
  # Going from wide to long
  df.out <- df.out %>% gather(Subject, Score, 2:length(df.out), na.rm = T)
  df.out$Subject <- gsub("Grade.your.", "", df.out$Subject)
  df.out$Category <- gsub("\\..*$" , "", df.out$Subject)
  df.out$Category <- paste0(toupper(substr(df.out$Category, 1, 1)), tolower(substring(df.out$Category, 2)))
  df.out$Subject <- gsub(".*\\.\\.", "", df.out$Subject)
  df.out$Subject <- gsub("\\.$", "", df.out$Subject)
  df.out$Subject <- gsub("\\.", " ", df.out$Subject)
  df.out$Source <- "Student"
  
  # Getting class code and teacher code from subject and student ID
  edFetch <- unique(edvalData[,c("Student.code","Class.code","Subject","Teacher.code")])
  df.out <- merge(df.out,edFetch, by = c("Student.code","Subject"), all.x = T)
  return(df.out)
}

getStudentID <- function(fullName) {
  ID <- studentDetails[studentDetails$Student.name == fullName,1]
  if (length(ID) == 0) {
    return(NA)
  } else {
    return(ID)
  }
}

getStudentName <- function(ID) {
  name <- studentDetails[studentDetails$Student.code == ID,]$Student.name
  return(name)
}

getClassTeacher <- function(classCode) {
  unique(edvalData[edvalData$Class.code == classCode,]$Teacher.name)
}

cleanTeacherJson <- function(df) {
  require(dplyr)
  require(tidyr)
  # Takes in flattened json object
  # fields must be teacher.code, class.code, student.code, dil, eng, beh
  df.out <- df
  # Currently this data uses student.name, hopefully future versions will be student.code
  names(df.out) <- c("Teacher.code", "Class.code", "Student.name","Diligence","Engagement","Behaviour")
  df.out <- df.out %>% gather(Category, Score, Diligence:Behaviour)
  df.out$Source <- "Teacher"
  nameAndNumber <- subset(studentDetails, select = c("Student.code","Student.name"))
  # Hopefull fixing the capitals issue
  nameAndNumber$Student.name <- tolower(nameAndNumber$Student.name)
  df.out$Student.name <- tolower(df.out$Student.name)
  df.out <- merge(df.out, nameAndNumber, by = "Student.name", all.x = T)
  # Fixing up the Score to be numeric
  df.out$Score <- gsub("Outstanding", 5, df.out$Score)
  df.out$Score <- gsub("Very Good", 4, df.out$Score)
  df.out$Score <- gsub("Good", 3, df.out$Score)
  df.out$Score <- gsub("Fair", 2, df.out$Score)
  df.out$Score <- gsub("Unsatisfactory", 1, df.out$Score)
  df.out$Score <- as.numeric(df.out$Score)
  # Getting the subject name from the class code
  classAndSubject <- unique(edvalData[,c("Class.code","Subject")])
  df.out <- merge(df.out, classAndSubject, by = "Class.code")
  return(df.out)
}

oddballs <- c("Ashley Van beek", "Zachary Gibson-brown", "Henry Barraclough-franks",
              "Lachlan Billington-phillips","D'arcy Deitz","Campbell De montemas",
              "Amelia O'sullivan","Conor O'meagher")

getMissingTeacherData <- function(cleanedTeacherEffortData) {
  totals <- cleanedTeacherEffortData %>% 
    group_by(Class.code, Student.code) %>% 
    summarise(Total = sum(Score))
  totals$Total <- totals$Total/3
  return(totals[is.na(totals$Total),])
}

effLongToWide <- function(d) {
  x <- unique(d)
  x$Category <- paste(x$Source, x$Category, sep = ".")
  x.t <- x[x$Source == "Teacher",!(names(x)) == "Source"]
  x.s <- x[x$Source == "Student",!(names(x)) == "Source"]
  x.t <- x.t %>% spread(Category, Score) 
  x.s <- unique(x.s) %>% spread(Category, Score)
  x.out <- merge(x.t, x.s, by = c("Student.code","Subject","Class.code","Teacher.code"), all = T)
}

getDuplicates <- function(x){
  return(x[duplicated(x[,-which(names(x) == "Score")]),])
}

getMissingData <- function(x) {
  x.out <- effLongToWide(x)
  x.out <- x.out[!(complete.cases(x.out)),]
  x.out$Student.name <- lapply(x.out$Student.code, getStudentName)
  return(x.out)
}

#totals <- tEffortData %>% 
 # group_by(Class.code, Student.code) %>% 
  #summarise(Total = sum(Score))
#totals$Total <- totals$Total/3


# Actually doing stuff
tEffortData <- cleanTeacherJson(teacherRaw)

s5clean <- cleanStudentGoogleForm(s5raw)
s4clean <- cleanStudentGoogleForm(s4raw)
s5add <- read.csv(paste0(datdir,"/Stage6_Y10ACC.csv"))
ts5dat <- tEffortData[tEffortData$Student.code %in% s5sn, ]
ts4dat <- tEffortData[tEffortData$Student.code %in% s4sn, ]

stage4data <- rbind(s4clean,ts4dat[,names(s4clean)])
stage5data <- rbind(s5clean,ts5dat[,names(s5clean)])

viewMissing <- function(x) {
  x <- x[!(is.na(x)),]
  View(x[!(complete.cases(x)),])
}

# Stripping unwanted data
edvalStage4 <- edvalData[edvalData$Student.code %in% s4sn,]
edvalStage5 <- edvalData[edvalData$Student.code %in% s5sn,]
edvalEnrollmentsStage4 <- data.frame(Student.code = edvalStage4$Student.code, 
                               Class.code = edvalStage4$Class.code)

s4tl <- edvalEnrollmentsStage4
s4tl$Source <- "Teacher"
s4sl <- edvalEnrollmentsStage4
s4sl$Source <- "Student"
s4checksum <- rbind(s4tl,s4sl)

effort.data.s4 <- read.csv(paste0(datdir,"Stage 4 Effort Data FINAL.csv"))
effort.data.s5 <- read.csv(paste0(datdir,"Stage 5 Effort Data FINAL.csv"))
# Removing NA from (possibly) students reporting on subjects
# they do not do
effort.data.s4 <- effort.data.s4[!(is.na(effort.data.s4$Class.code)),]
effort.data.s5 <- effort.data.s5[!(is.na(effort.data.s5$Class.code)),]

missingS4 <- getMissingData(effort.data.s4)
missingS5 <- getMissingData(effort.data.s5)

View(missingS5)
View(missingS4)

effort.data <- read.csv(paste0(datdir,"2017 Term 1 Effort Data.csv"))
effort.data <- effort.data[complete.cases(effort.data),]
