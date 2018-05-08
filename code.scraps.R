# Codes scraps

# Older code

datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Reporting"
efdatfile <- paste0(datdir, "/2016T4 Y12 effort data.xlsx")
teacher.dat.folder <- paste0(datdir, "/2016T4 teacher data/")
student.dat.file <- paste0(datdir,"/2016T4 Y12 student effort data.csv")
teacher.dat.file <- paste0(datdir,"/2016T4 Y12 teacher effort data.csv")
# the following needs to be merged and then outputed
# teacher.dat <- read.csv(teacher.dat.file)

effortMergeData <- function(studentData, teacherData) {
  require(tidyr)
  sdat <- studentData %>% gather("Category", "Score", 
                                 c(Diligence, Engagement, Behaviour),
                                 na.rm = TRUE)
  sdat <- data.frame(sdat, Source = "Student")
  tdat <- teacherData %>% gather("Category", "Score", 
                                 c(Diligence, Engagement, Behaviour),
                                 na.rm = TRUE)
  tdat <- data.frame(tdat, Source = "Teacher")
  merged.df <- rbind(tdat, sdat)
  merged.df <- subset(merged.df, select = c(Student.code,
                                            Teacher.code,
                                            Subject,
                                            Source,
                                            Category,
                                            Score))
  return(merged.df)
}

xlsMerge <- function(dir = getwd(), sheet.index = 1, colRange = 1:11) {
  ### Merges all xls, and xlsx files in the folder into one data frame
  require(xlsx)
  files <- list.files(path = dir, full.names = T)
  files <- files[grep(".xls*", files)]
  # Exiting if no files
  if (length(files) == 0) {
    return("No xls or xlsx files in directory")
  }
  df <- read.xlsx(files[[1]], sheetIndex = sheet.index, colIndex = colRange)
  for (f in files) {
    df.add <- read.xlsx(f, sheetIndex = sheet.index, colIndex = colRange)
    df <- merge(df, df.add,all = T)
  }
  #df <- aggregate(df, by = list(df$), max, na.rm = T)
  return(df)
}

# Merging multiple xlsx files to create teacher.dat. Slow!!
teacher.dat <- xlsMerge(teacher.dat.folder)
teacher.dat <- teacher.dat[complete.cases(teacher.dat),]
write.csv(teacher.dat, teacher.dat.file, row.names = FALSE)

# Read teacher.dat (and student.dat) instead
teacher.dat <- read.csv(teacher.dat.file)
student.dat <- read.csv(student.dat.file)
effort.data <- effortMergeData(student.dat, teacher.dat)
write.csv(effort.data, "effort_report_data.csv", row.names = FALSE)
effort.data <- read.csv("effort_report_data.csv")
student.info <- data.frame(Student.code = student.dat$Student.code, Email = student.dat$Email,
                           Student.name = paste(student.dat$First.name, student.dat$Surname))
student.info <- unique.data.frame(student.info)
write.csv(student.info, "student.names.csv", row.names = FALSE)