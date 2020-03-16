library(ggplot2)
library(dplyr)
library(tidyr)

# Miscellaneous functions
toclip <- function(x,row.names=FALSE,col.names=TRUE,...) {
  clip <- pipe("pbcopy", "w")
  write.table(x,file=clip,sep="\t",row.names=row.names,col.names=col.names,...)
  close(clip)
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

viewMissing <- function(x) {
  x <- x[!(is.na(x)),]
  View(x[!(complete.cases(x)),])
}

trim_pref_name <- function(x) {
  x <- gsub( " *\\(.*?\\) *", "", x)
  return(x)
}

yearFromClassCode <- function(x) {
  if (x == "") {return(NA)} 
  else if (substr(x, 1, 2) == "12") {return(12)}
  else if (substr(x, 1, 2) == "11") {return(11)}
  else if (substr(x, 1, 2) == "10") {return(10)}
  else if (substr(x, 1, 1) == "9") {return(9)}
  else if (substr(x, 1, 1) == "8") {return(8)}
  else if (substr(x, 1, 1) == "7") {return(7)}
  else {return(NA)}
}

clean_tracking_data <- function(df) {
  require(stringr)
  require(tidyverse)
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
  return(ed)
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


# Analysis functions #

# Setting up some default parameters - the main being the factor order
# list for the subjects. This is the order that the subjects will appear on 
# any of the charts. 
subject.order.list <- c("English","English Standard", "English Advanced", "English Extension 1", "English Extension 2",
           "Mathematics", "Mathematics Advanced","Mathematics General", "Mathematics Extension 1", "Mathematics Extension 2", "Mathematics General 2", "Mathematics Standard", "Mathematics Standard 2",
           "Science", "Science Compaction", "Biology", "Chemistry", "Physics", "Science Extension",
           "History", "History Ancient", "Ancient History","History Modern", "Modern History","History Extension", "Studies of Religion", "Studies of Religion 1", "Studies of Religion 2",
           "Geography", "Economics", "Commerce", "Legal Studies", "Business Studies", "Global Studies", "Global Perspectives",
           "Cornerstone", "Philosophy", "SWYM", "Wide Reading", "Big History", 
           "Music", "Music 1 & 2", "Music 1", "Music 2", "Music Extension",
           "French", "French Continuers", "Japanese", "Latin",
           "Design and Technology", "Design & Technology", "Engineering Studies", 
           "Media Technology","Food Technology","Graphics Technology", "Technology", "Textiles Technology",
           "Visual Arts",
           "Drama",
           "PDHPE",
           "Distance Education", "TVET", "Personal Interest Project")

effortPlot <- function(d, ctitle = "Chart title", slist = subject.order.list) {
  # Recieves a long data frame and returns a ggplot historgram
  # There must be the following fields:
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
  # You will get strange errors if plyr is loaded after dplyr
 # detach(package:plyr)
  require(dplyr)
  d$Source <- factor(d$Source, levels = c("Student", "Teacher"))
  d <- d[,c("Source","Category", "Score")]
  d <- summarize(group_by(d, Source, Category), Score = mean(Score, na.rm = T))
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
  g <- ggplot(data = d, 
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

effortTimelines <- function(d, title = "Effort Timeline", sce = "Teacher") {
  require(ggplot2)
  require(dplyr)
  d <- summarise(group_by(d, Student.code, Source, Date), mean(Score, na.rm = T))
  names(d) <- c("ID", "Source", "Date","Score")
  d$Source <- factor(d$Source, levels = c("Student", "Teacher"))
  
  d$ID <- as.factor(d$ID)
  alphaLevel <- 0.1
  
  #d <- d[d$Source == sce,]
  
  g <- ggplot() +
    geom_line(data = d, 
              aes(x = Date, y = Score, group = ID), 
              alpha = alphaLevel) + 
    scale_y_continuous(limits = c(1,5), labels = c("Unsatisfactory","Fair","Good","Very Good","Outstanding"))+
    ggtitle(title) + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype = "dotted",size = 0.2, colour = "light gray"))
  g #+ facet_wrap(~ Source)
}

effortIndividualTimeline <- function(d, ID, title = "Effort Timeline", student.info = student.info) {
  require(ggplot2)
  require(dplyr)
  d <- summarise(group_by(d, Student.code, Source, Date), mean(Score, na.rm = T))
  names(d) <- c("ID", "Source", "Date","Score")
  d$Source <- factor(d$Source, levels = c("Student", "Teacher"))
  d.student <- d[d$ID == ID,]
  
  cohort <- unique(student.info[student.info$Student.code == ID,]$Cohort)
  cohort.ids <- unique(student.info[student.info$Cohort == cohort,"Student.code"])
  
  d.cohort <- d[d$ID %in% cohort.ids[[1]] & d$Source == "Teacher",]
  d.cohort$ID <- as.factor(d.cohort$ID)
  alphaLevel <- min(1, 4.0/length(cohort.ids[[1]]))
  
  g <- ggplot() +
    geom_line(data = d.student, 
              aes(x = Date, y = Score, color = Source)) + 
    scale_colour_manual(values =c("#ECA41A","#1E2132")) +
    geom_point(d = d.student, aes(x = Date, y = Score, color = Source)) + 
    geom_line(data = d.cohort, 
              aes(x = Date, y = Score, group = ID), alpha = alphaLevel)+ 
    geom_point(d = d.cohort,
               aes(x = Date, y = Score, group = ID), alpha = alphaLevel/2) +
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

class_averages_teacher <- function(df, teacherCode, title = "Class summary", student.info = student.info) {
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
  df2 <- merge(student.info[,c("Student.code","Student.name")],df2)
  df.out <- data.frame(Class = as.character(df2$Subject), 
                       Student = as.character(df2$Student.name),
                       Effort = df2$Effort, Diligence = df2$Diligence,
                       Engagement = df2$Engagement, Behaviour = df2$Behaviour)
  df.out <- df.out[order(df.out$Class, df.out$Student),]
}

humanities_sub_list <- c("Global Perspectives", "Geography", "History Extension", "History Modern", "History", "Economics", "Business Studies", "History Ancient", "Commerce", "Studies of Religion 1", "Studies of Religion 2", "Big History","Legal Studies")
maths_sub_list <- c("Mathematics", "Mathematics Extension", "Mathematics Extension 1", "Mathematics Extension 2", "Mathematics General", "Mathematics Advanced", "Mathematics Standard")
english_sub_list <- c("English", "English Standard", "English Advanced", "English Extension", "English Extension 1", "English Extension 2")
science_sub_list <- c("Science", "Science Compaction", "Biology", "Chemistry", "Physics")

g_department <- function(df,sub_list) {
  require(ggplot2)
  df.temp <- df[df$Source == "Teacher",]
  df.temp <- df.temp[df.temp$Subject %in% sub_list,]
  g_dep <- ggplot(df.temp, aes(Score)) + geom_histogram(binwidth = 1) + facet_wrap(~ Teacher.code) + ggthemes::theme_tufte()
  return(g_dep)
}







