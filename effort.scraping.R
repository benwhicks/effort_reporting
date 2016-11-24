datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Reporting"
efdatfile <- paste0(datdir, "/2016T4 Y12 effort data.xlsx")
teacher.dat.folder <- paste0(datdir, "/2016T4 teacher data/")

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

