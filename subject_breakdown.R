# Mucking around with maths data

# Getting data 
datdir <- "/Users/benhicks/Documents/Data Analysis/Data-Oxley/Effort Data/2017 Term 1"
df <- read.csv(paste0(paste0(datdir, "/2017 Term 1 Effort Data.csv")))
student.info <- read.csv(paste0(datdir,"/StudentInfo.csv"))

# Processing data for comparisons
df$Category <- factor(df$Category, levels = c("Diligence", "Engagement","Behaviour"))
df <- unique(merge(df, student.info))

subjects <- unique(df$Subject)
mathsClasses <- subjects[grepl("Math",subjects)]
df.maths <- df[df$Subject %in% mathsClasses,]
englishClasses <- subjects[grepl("Engli",subjects)]

  
g_m <- ggplot(df.maths, aes(Score, linetype = Source)) +
  geom_density(bw = 0.5) + theme_tufte() + theme(text = element_text(size = 6))
g_m + facet_grid(Category ~ Year) + ggtitle("Year, Source, Category comparison for Mathematics classes")

# Mucking around with maths data
g_all <- ggplot(df, aes(Score, linetype = Source)) +
  geom_density(bw = 0.5) + theme_tufte() + theme(text = element_text(size = 6))
g_all + facet_grid(Category ~ Year) + ggtitle("Year, Source, Category comparison for All classes")

df.maths.compare <- df
df.maths.compare$Mathematics <- df.maths.compare$Subject %in% mathsClasses

g_maths_compare <- ggplot(df.maths.compare[df.maths.compare$Source=="Student",], 
                          aes(Score, linetype = Source, color = Mathematics)) +
  geom_density(bw = 0.5) + theme_tufte()
g_maths_compare + facet_grid(Category ~ Year)


df.english.compare <- df
df.english.compare$English <- df.english.compare$Subject %in% englishClasses

g_english_compare <- ggplot(df.english.compare[df.english.compare$Source=="Student",], 
                          aes(Score, linetype = Source, color = English)) +
  geom_density(bw = 0.5) + theme_tufte()
g_english_compare + facet_grid(Category ~ Year)

subjectCompare <- function(sub, df) {
  # sub is the text containing the subject name
  # df is the effort data
  subjects <- unique(df$Subject)
  classes <- subjects[grepl(sub, subjects)]
  df.temp <- df
  df.temp$inSub <- df.temp$Subject %in% classes
  g_compare <- ggplot(df.temp[df.temp$Source=="Student",], 
                              aes(Score, linetype = Source, color = inSub)) +
    geom_density(bw = 0.5) + theme_tufte()
  g_compare + facet_grid(Category ~ Year) + ggtitle(paste0("Whole school and ", sub, " comparison"))
}

classCompare <- function(sub, df, category = "Engagement") {
  # sub is the text containing the subject name
  # df is the effort data
  subjects <- unique(df$Subject)
  classes <- subjects[grepl(sub, subjects)]
  df.temp <- df
  df.temp$inSub <- df.temp$Subject %in% classes
  g_compare <- ggplot(df.temp[df.temp$Source=="Student",], 
                      aes(Score, linetype = Source, color = inSub)) +
    geom_density(bw = 0.5) + theme_tufte()
  g_compare + facet_grid(Category ~ Class.code) + ggtitle(paste0("Whole school and ", sub, " comparison"))
}