library(ggplot2)
library(dplyr)
library(tidyr)

# Miscellaneous functions
toclip <- function(x,row.names=FALSE,col.names=TRUE,...) {
  clip <- pipe("pbcopy", "w")
  write.table(x,file=clip,sep="\t",row.names=row.names,col.names=col.names,...)
  close(clip)
}

trim_pref_name <- function(x) {
  x <- gsub( " *\\(.*?\\) *", "", x)
  return(x)
}

# Import functions #
get.student.details <- function(path.to.clean.edumate.data = edumate.data.path){
  edvalCleanData <- read.csv(path.to.clean.edval.data)
  studentDetails <- data.frame(Student.code = edvalCleanData$Student.code, 
                               Student.name = paste0(edvalCleanData$First.name, " ", edvalCleanData$Surname),
                               Email = edvalCleanData$Email)
  #studentDetails$Student.name <- gsub( " *\\(.*?\\) *", "", studentDetails$Student.name)
  #studentDetails$Student.name <- tolower(studentDetails$Student.name)
  #studentDetails$Student.name <- tools::toTitleCase(studentDetails$Student.name)
  studentDetails <- unique(studentDetails)
  return(studentDetails)
}


cleanStudentGoogleForm <- function(df, edvalPth = edCleanPath) {
  # This is for cleaning the survey forms downloaded as csv 
  require(dplyr)
  require(tidyr)
  
  # Removing unwanted columns
  drop <- c("For.admin.purposes")
  df.out <- df[, !(names(df) %in% drop)]
  
  # Converting data types
  df.out$Timestamp <- as.POSIXct(df.out$Timestamp)
  df.out$Student.code <- as.integer(df.out$Student.code)
  for (i in grep("Grade.your", names(df.out))) {
    df.out[,i] <- as.character(df.out[,i])
    }
  # Filter by Timestamp needed here??
  
  
  # Going from wide to long
  df.out <- df.out %>% 
    gather(Subject, Score, grep("Grade.your",names(df.out)), na.rm = T)
  df.out$Subject <- gsub("Grade.your.", "", df.out$Subject)
  df.out$Category <- gsub("\\..*$" , "", df.out$Subject)
  df.out$Category <- paste0(toupper(substr(df.out$Category, 1, 1)), tolower(substring(df.out$Category, 2)))
  df.out$Subject <- gsub(".*\\.\\.", "", df.out$Subject)
  df.out$Subject <- gsub("\\.$", "", df.out$Subject)
  df.out$Subject <- gsub("\\.", " ", df.out$Subject)
  df.out$Source <- "Student"
  
  # Fixing up the Score to be numeric
  df.out$Score <- gsub("Outstanding", 5, df.out$Score)
  df.out$Score <- gsub("Very Good", 4, df.out$Score)
  df.out$Score <- gsub("Good", 3, df.out$Score)
  df.out$Score <- gsub("Fair", 2, df.out$Score)
  df.out$Score <- gsub("Unsatisfactory", 1, df.out$Score)
  df.out$Score <- gsub("NA", NA, df.out$Score)
  df.out$Score <- as.numeric(df.out$Score)
  
  # Getting class code and teacher code from subject and student ID
  edvalData <- read.csv(edvalPth)
  edFetch <- unique(edvalData[,c("Student.code","Class.code","Subject","Teacher.code")])
  df.out <- merge(df.out, edFetch, by = c("Student.code","Subject"), all.x = T)
  studentDetails <- get.student.details(edCleanPath)
  df.out <- merge(df.out, studentDetails[,c("Student.code","Student.name")], by = c("Student.code"),all.x = T)
  return(df.out)
}

cleanTeacherJsonOld <- function(df, edvalPth = edCleanPath) {
  require(dplyr)
  require(tidyr)
  # Takes in flattened json object
  # fields must be teacher.code, class.code, student.code, dil, eng, beh, time submitted
  df.out <- df
  # Currently this data uses student.name, hopefully future versions will be student.code
  names(df.out) <- c("Teacher.code", "Class.code", "Student.name","Diligence","Engagement","Behaviour", "Timestamp")
  df.out$Timestamp <- as.POSIXct(df.out$Timestamp, format = "%a %b %d %Y %H:%M:%S")
  df.out <- df.out %>% gather(Category, Score, Diligence:Behaviour)
  
  # Fixing up the Score to be numeric
  df.out$Score <- gsub("Outstanding", 5, df.out$Score)
  df.out$Score <- gsub("Very Good", 4, df.out$Score)
  df.out$Score <- gsub("Good", 3, df.out$Score)
  df.out$Score <- gsub("Fair", 2, df.out$Score)
  df.out$Score <- gsub("Unsatisfactory", 1, df.out$Score)
  df.out$Score <- gsub("NA", NA, df.out$Score)
  df.out$Score <- as.numeric(df.out$Score)
  
  df.out$Source <- "Teacher"
  df.out$Student.name <- tools::toTitleCase(tolower(df.out$Student.name))

  studentDetails <- get.student.details(edvalPth)
  df.out <- merge(df.out, studentDetails[,c("Student.name","Student.code")], by = "Student.name",all.x = T)
  
  # Getting the subject name from the class code
  edvalData <- read.csv(edvalPth)
  classAndSubject <- unique(edvalData[,c("Class.code","Subject")])
  df.out <- merge(df.out, classAndSubject, by = "Class.code")
  
  
  return(df.out)
}

cleanStudentJson <- function(df, edvalPth = edCleanPath) {
  require(dplyr)
  require(tidyr)
  # Takes in flattened json object
  # fields must be teacher.code, class.code, student.code, dil, eng, beh, time submitted
  df.out <- df
  # Currently this data uses student.name, hopefully future versions will be student.code
  names(df.out) <- c("Teacher.code", "Class.code", "Student.name","Diligence","Engagement","Behaviour", "Timestamp")
  df.out$Timestamp <- as.POSIXct(df.out$Timestamp, format = "%a %b %d %Y %H:%M:%S")
  df.out <- df.out %>% gather(Category, Score, Diligence:Behaviour)
  
  # Fixing up the Score to be numeric
  df.out$Score <- gsub("Outstanding", 5, df.out$Score)
  df.out$Score <- gsub("Very Good", 4, df.out$Score)
  df.out$Score <- gsub("Good", 3, df.out$Score)
  df.out$Score <- gsub("Fair", 2, df.out$Score)
  df.out$Score <- gsub("Unsatisfactory", 1, df.out$Score)
  df.out$Score <- gsub("NA", NA, df.out$Score)
  df.out$Score <- as.numeric(df.out$Score)
  
  df.out$Source <- "Teacher"
  df.out$Student.name <- tools::toTitleCase(tolower(df.out$Student.name))
  
  studentDetails <- get.student.details(edvalPth)
  df.out <- merge(df.out, studentDetails[,c("Student.name","Student.code")], by = "Student.name",all.x = T)
  
  # Getting the subject name from the class code
  edvalData <- read.csv(edvalPth)
  classAndSubject <- unique(edvalData[,c("Class.code","Subject")])
  df.out <- merge(df.out, classAndSubject, by = "Class.code")
  
  
  return(df.out)
}


getEffortData <- function(path.to.effort.folder = datdir, validate = TRUE) {
  require(tidyr)
  # building empty shell to find missing data
  edvalData <- read.csv(edCleanPath)
  effort.teacher.shell <- data.frame(Student.code = edvalData$Student.code, Subject = edvalData$Subject,
                                     Class.code = edvalData$Class.code, Teacher.code = edvalData$Teacher.code,
                                     Source = "Teacher",
                                     Diligence = NA, Engagement = NA, Behaviour = NA)
  effort.teacher.shell <- effort.teacher.shell %>% gather(Category, Score, Diligence:Behaviour)
  studentDetails <- get.student.details(edCleanPath)[,c("Student.code","Student.name")]
  effort.teacher.shell <- merge(effort.teacher.shell, studentDetails)
  effort.student.shell <- data.frame(Student.code = edvalData$Student.code, Subject = edvalData$Subject,
                                     Class.code = edvalData$Class.code, Teacher.code = edvalData$Teacher.code,
                                     Source = "Student",
                                     Diligence = NA, Engagement = NA, Behaviour = NA)
  effort.student.shell <- effort.student.shell %>% gather(Category, Score, Diligence:Behaviour)
  studentDetails <- get.student.details(edCleanPath)[,c("Student.code","Student.name")]
  effort.student.shell <- merge(effort.student.shell, studentDetails)
  effort.shell <- rbind(effort.teacher.shell,effort.student.shell)
  effort.shell$Score <- NULL # need to obliterate to introduce NA's later
  
  # Extracting data from effort folder
  i <- 1
  effDataList <- list()
  if (file.exists(paste0(datdir, "teacher_responses.json"))) {
    effDataList[[i]] <- cleanTeacherJson(
      jsonlite::fromJSON(paste0(datdir,"teacher_responses.json"), flatten = T))
    i <- i + 1
  }
  if (file.exists(paste0(datdir,"Year 12 Effort Survey.csv"))) {
    effDataList[[i]] <- cleanStudentGoogleForm(
      read.csv(paste0(datdir,"Year 12 Effort Survey.csv")))
    i <- i + 1
  } else {print("No year 12 data")} 
  if (file.exists(paste0(datdir,"Year 11 Effort Survey.csv"))) {
    effDataList[[i]] <- cleanStudentGoogleForm(
      read.csv(paste0(datdir,"Year 11 Effort Survey.csv")))
    i <- i + 1
  } else {print("No year 11 data")}
  if (file.exists(paste0(datdir,"Year 9 and 10 Effort Survey.csv"))) {
    effDataList[[i]] <- cleanStudentGoogleForm(
      read.csv(paste0(datdir,"Year 9 and 10 Effort Survey.csv")))
    i <- i + 1
  } else {print("No year 9 or 10 data")}
  if (file.exists(paste0(datdir,"Year 7 and 8 Effort Survey.csv"))) {
    effDataList[[i]] <- cleanStudentGoogleForm(
      read.csv(paste0(datdir,"Year 7 and 8 Effort Survey.csv")))
    i <- i + 1
  } else {print("No year 7 or 8 data")}
  effort.data <- do.call(rbind, effDataList) 
  
  # Getting rid of missing scores
  effort.data <- effort.data[!is.na(effort.data$Score),]
  
  # Filter by most recent timestamp for duplicates
  effort.data <- aggregate(Timestamp ~ ., data = effort.data, max)
  
  if (validate) {
    effort.data <- merge(effort.shell, effort.data, all.x = T)
  }

  return(effort.data)
}

getMissingTeacherData <- function(df = effort.data, get_ind = FALSE) {
  if (get_ind) {
    df <- df[df$Source == "Teacher" & is.na(df$Score),c("Subject","Class.code","Teacher.code","Student.name")]
  } else {
    df <- df[df$Source == "Teacher" & is.na(df$Score),c("Subject","Class.code","Teacher.code")]
  }
  df <- unique(df)
  df <- df[order(df$Teacher.code),]
  return(df)
}

getMissingStudentData <- function(df = effort.data, tg = NULL) {
  df <- df[df$Source == "Student" & is.na(df$Score),c("Student.code","Subject","Class.code","Student.name","Category")]
  df <- unique(df)
  df <- df[order(df$Student.name),]
  if (!is.null(tg)) {
    tg <- unique(tg[,c("Student.code","Class.code","Teacher.name")])
    names(tg) <- c("Student.code","Tutor Group","Tutor")
    df <- merge(df, tg)
  }
  return(df)
}


effLongToWide <- function(d) {
  require(tidyr)
  x <- unique(d)
  x$Category <- paste(x$Source, x$Category, sep = ".")
  x.t <- x[x$Source == "Teacher",!(names(x)) == "Source"]
  x.s <- x[x$Source == "Student",!(names(x)) == "Source"]
  x.t <- x.t %>% spread(Category, Score) 
  x.s <- unique(x.s) %>% spread(Category, Score)
  x.out <- merge(x.t, x.s, 
                 by = c("Student.code","Subject","Class.code","Teacher.code","Date","Student.name"), 
                 all = T)
}




# Hunting missing data functions #
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

viewMissing <- function(x) {
  x <- x[!(is.na(x)),]
  View(x[!(complete.cases(x)),])
}


# Analysis functions #

# Setting up some default parameters - the main being the factor order
# list for the subjects. This is the order that the subjects will appear on 
# any of the charts. 
subject.order.list <- c("English","English Standard", "English Advanced", "English Extension 1", "English Extension 2",
           "Mathematics", "Mathematics General", "Mathematics Extension 1", "Mathematics Extension 2", "Mathematics General 2",
           "Science", "Science Compaction", "Biology", "Chemistry", "Physics",
           "History", "History Ancient", "History Modern", "History Extension", "Studies of Religion", "Studies of Religion 1", "Studies of Religion 2",
           "Geography", "Economics", "Commerce", "Legal Studies", "Business Studies", "Global Studies", "Global Perspectives",
           "Cornerstone", "SWYM", "Wide Reading",
           "Music", "Music 1 & 2", "Music 1", "Music 2",
           "French", "French Continuers",
           "Design and Technology", "Design & Technology", "Engineering Studies",
           "Media Technology","Food Technology","Graphics Technology", "Technology",
           "Visual Arts",
           "Drama",
           "PDHPE",
           "Distance Education", "TVET")

effortPlot <- function(d, ctitle = "Chart title", slist = subject.order.list) {
  # Recieves a long data frame and returns a ggplot historgram
  # Their must be the following fields:
  #  -- Score : The score (1 to 5, 5 being Outstanding)
  #  -- Subject : The subject (such as 'English')
  #  -- Source : Either Student or Teacher
  #  -- Category : One of Diligence, Behaviour or Effort
  d$Source <- factor(d$Source, levels = c("Student", "Teacher"))
  # checking to see if Subject is in subject.order.list
  for (subject in unique(d$Subject)) 
    {
    if (!(subject %in% slist)) 
      {
      return(paste0("Subject graph cannot be displayed as ", subject, " not in subject.order.list"))
      }
    }
  require(ggplot2)
  # Setting up reording of subjects - more may need to be added as the system is run
  subject.orders <- data.frame(Subject = slist, Order = seq(length(slist)))
  d <- merge(d, subject.orders)
  forOrder <- unique(data.frame(Subject = d$Subject, Order = d$Order))
  forOrder <- forOrder[order(forOrder$Order),]
  facetOrder <- forOrder$Subject
  d$Subject <- factor(d$Subject, levels = facetOrder)
  
  d$Category <- factor(d$Category, levels = c("Diligence", "Engagement", "Behaviour"))
  
  # Creating the actual plot
  g <- ggplot(data = d, 
              aes(x = Category, y = Score, fill = Source)) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    ggtitle(ctitle) +
    guides(fill = guide_legend(title = NULL)) +
    theme(text = element_text(size = 10)) +
    facet_grid(Subject ~ .) +
    coord_cartesian(ylim = c(1,5)) +
    scale_y_discrete(name = "Effort rating", 
                     limits = c("Unsatisfactory", "Fair", "Good", "Very Good", "Outstanding")) +
    scale_fill_manual(values = c("#ECA41A","#1E2132"))
  return(g)
}

overallEffortPlot <- function(d, ctitle = "Overall Effort") {
  require(dplyr)
  d$Source <- factor(d$Source, levels = c("Student", "Teacher"))
  d <- summarise(group_by(d, Source, Category), mean(Score, na.rm = T))
  names(d) <- c("Source", "Category", "Score")
  d$Category <- factor(d$Category, levels = c("Diligence", "Engagement", "Behaviour"))
  # creating the plot
  g <- ggplot(data = d, aes(x = Category, y = Score, fill = Source)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    ggtitle(ctitle) +
    guides(fill = guide_legend(title = NULL)) +
    theme(text = element_text(size = 10)) +
    coord_cartesian(ylim = c(1,5)) +
    scale_y_discrete(name = "Effort rating", 
                     limits = c("Unsatisfactory", "Fair", "Good", "Very Good", "Outstanding")) +
    scale_fill_manual(values = c("#ECA41A","#1E2132"))
  return(g)
}

effortPlotClass <- function(d, title = "By Class") {
  d$Category <- factor(d$Category, levels = c("Diligence", "Engagement", "Behaviour"))
  d2 <- merge(d, get.student.details())
  g <- ggplot(data = d2, 
              aes(x = Category, y = Score, fill = Source)) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    ggtitle(title) +
    guides(fill = guide_legend(title = NULL)) +
    theme(text = element_text(size = 10),
          strip.text.y = element_text(size = 8, colour = "black", angle = 0),
          axis.text.x = element_text(size = 8, angle = 90)) +
    facet_grid(Student.name ~ Class.code, labeller = label_wrap_gen(25)) +
    coord_cartesian(ylim = c(1,5)) +
    scale_y_discrete(name = "Effort rating", 
                     limits = c("Unsatisfactory", "Fair", "Good", "Very Good", "Outstanding")) +
    scale_fill_manual(values = c("#ECA41A","#1E2132"))
  return(g)
}

getCohort <- function(ID) {
  student.info <- read.csv(student.info.path)
  cohort <- student.info[student.info$Student.code == ID,]$Cohort
  cohort.ids <- unique(student.info[student.info$Cohort == cohort,"Student.code"])
  return(cohort.ids)
}

effortIndividualTimeline <- function(d, ID, title = "Effort Timeline", student.info = student.info) {
  require(ggplot2)
  require(dplyr)
  d <- summarise(group_by(d, Student.code, Source, Date), mean(Score, na.rm = T))
  names(d) <- c("ID", "Source", "Date","Score")
  d$Source <- factor(d$Source, levels = c("Student", "Teacher"))
  d.student <- d[d$ID == ID,]
  
  cohort <- student.info[student.info$Student.code == ID,]$Cohort
  cohort.ids <- unique(student.info[student.info$Cohort == cohort,"Student.code"])
  
  d.cohort <- d[d$ID %in% cohort.ids & d$Source == "Teacher",]
  d.cohort$ID <- as.factor(d.cohort$ID)
  alphaLevel <- 4.0/length(cohort.ids)
  
  g <- ggplot() +
    geom_line(data = d.student, 
              aes(x = Date, y = Score, color = Source)) + 
    scale_colour_manual(values =c("#ECA41A","#1E2132")) +
    geom_point(d = d.student, aes(x = Date, y = Score, color = Source)) + 
    geom_line(data = d.cohort, 
              aes(x = Date, y = Score, group = ID), alpha = alphaLevel)+ 
    scale_y_continuous(limits = c(1,5), labels = c("Unsatisfactory","Fair","Good","Very Good","Outstanding"))+
    ggtitle(title) + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype = "dotted",size = 0.2, colour = "light gray"))
  g
}
#effortIndividualTimeline(all.effort.data, 1963)

subjectCompare <- function(sub, df, inc.teacher = FALSE) {
  # sub is the text containing the subject name
  # df is the effort data
  subjects <- unique(df$Subject)
  classes <- subjects[grepl(sub, subjects)]
  df.temp <- df
  df.temp$inSub <- df.temp$Subject %in% classes
  if (!(inc.teacher)) {df.temp <- df.temp[df.temp$Source=="Student",]}
  g_compare <- ggplot(df.temp, 
                      aes(Score, linetype = Source, color = inSub)) +
    geom_density(bw = 0.5) + theme_tufte()
  g_compare + facet_grid(Category ~ Year) + ggtitle(paste0("Whole school and ", sub, " comparison"))
}

teacherCompare <- function(teacher, df, inc.teacher = FALSE) {
  # sub is the text containing the subject name
  # df is the effort data
  classes <- unique(df[df$Teacher.code == teacher,]$Class.code)
  df.temp <- df
  df.temp$inSub <- df.temp$Class.code %in% classes
  if (!(inc.teacher)) {df.temp <- df.temp[df.temp$Source=="Student",]}
  g_compare <- ggplot(df.temp, 
                      aes(Score, linetype = Source, color = inSub)) +
    geom_density(bw = 0.5) + theme_tufte()
  g_compare + facet_grid(Category ~ Class.code) + ggtitle(paste0(teacher, " classes comparison"))
}

class_averages_teacher <- function(df, teacherCode, title = "Class summary") {
  # returns data frame to then send to kable 
  require(dplyr)
  require(tidyr)
  require(knitr)
  df <- df[df$Teacher.code == teacherCode, ]
  df2 <- df[df$Source == "Teacher",]
  df2 <- unique(df2[complete.cases(df2),])
  df2 <- aggregate(Score ~ Student.code + Subject + Category, df2, mean)
  df2 <- tidyr::spread(df2, Category, Score)
  df2$Effort <- (df2$Behaviour + df2$Diligence + df2$Engagement)/3.0
  df2 <- merge(get.student.details(),df2)
  df.out <- data.frame(Class = as.character(df2$Subject), 
                       Student = as.character(df2$Student.name),
                       Effort = df2$Effort, Diligence = df2$Diligence,
                       Engagement = df2$Engagement, Behaviour = df2$Behaviour)
  df.out <- df.out[order(df.out$Class, df.out$Student),]
}










