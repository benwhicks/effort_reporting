#######################################
###### Effort Report Script ###########
#######################################

# This is one of four files to generate the effort reports
# The files required are:
#    1. effort.functions.R
#    2. effort_report_script.R
#    3. student_effort_report_markdown.Rmd
#    4. teacher_effort_report_markdown.Rmd 
#    5. school_effort_report_markdown.Rmd
#    6. effort_report_data.csv
#    7. student.names.csv   -- A csv file of student number to name conversions, fields are "Student.code" and "Student.name"
# The files need to be in the same folder. 

library(knitr)
library(rmarkdown)
library(markdown)

# Reading in effort data file called 'effort_report_data.csv"
# Data should have the following fields:
#   Student.code
#   Teacher.code
#   Category 
#   Subject
#   Score
#   Source
effort.data <- read.csv("effort_report_data.csv")
student.numbers <- unique(effort.data$Student.code)
teacher.codes <- unique(effort.data$Teacher.code)

# Creates a directory called 'reports' if it does not already exist
if (!dir.exists("reports")) {dir.create("reports")}

# Creating student reports
for (ID in student.numbers) {
  s.name <- student.info[student.info$Student.code == ID,]$Student.name
  rmarkdown::render('student_effort_report_markdown.Rmd',
                    output_file = paste0("Student_Effort_Report_", s.name , "_", Sys.Date(), ".html" ),
                    output_dir = "reports")
}

# Creating teacher reports
############## commented out until markdown ready #########
for (tcode in teacher.codes) {
  rmarkdown::render('teacher_effort_report_markdown.Rmd',
                    output_file = paste0("Class_Effort_Report_", tcode, "_", Sys.Date(), ".html" ),
                    output_dir = "reports")
}

# Creating school reports

# To be implemented