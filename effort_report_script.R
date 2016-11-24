#######################################
###### Effort Report Script ###########
#######################################

# This is one of four files to generate the effort reports
# The files required are:
#    1. effort.functions.R
#    2. effort_report_script.R
#    3. effort_report_markdown.Rmd
#    4. effort_report_data.csv
# The files need to be in the same folder. 

# The data needs to have the following fields:
#    1. Student.ID
#    2. Teacher.ID
#    3. Subject
#    4. Category (Diligence, Engagement, Behaviour)
#    5. Score (1 to 5, 1 being unsatisfactory, 5 being Outstanding)

library(knitr)
library(rmarkdown)
library(markdown)

# Creates a directory called 'reports' if it does not already exist
if (!dir.exists("reports")) {dir.create("reports")}

# Reading in effort data file called 'effort_report_data.csv"
# Data should have the following fields:
#   Student.ID
#   Category 
#   Subject
#   Score
#   Source
effort.data <- read.csv("effort_report_data.csv")
student.numbers <- unique(effort.data$Student.ID)

for (ID in student.numbers) {
  # main loop
  
}
