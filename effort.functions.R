effortPlot.bysubject <- function(d, ctitle = "Chart title") {
  # Recieves a long data frame and returns a ggplot historgram
  # Their must be the following fields:
  #  -- Score : The score (1 to 5, 5 being Outstanding)
  #  -- Subject : The subject (such as 'English')
  #  -- Source : Either Student or Teacher
  #  -- Category : One of Diligence, Behaviour or Effort
  require(ggplot2)
  
  # Setting up reording of subjects -- this needs to be refined 
  # to include Y12 subjects
  slist <- c("English",
             "Mathematics",
             "Science",
             "History",
             "Geography",
             "Cornerstone",
             "Music",
             "French",
             "Design & Technology",
             "Media Technology",
             "Food Technology",
             "Grahpics Technology",
             "Visual Art",
             "PDHPE")
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
    theme(text = element_text(family = "Helevetica", size = 10)) +
    facet_grid(Subject ~ .) +
    scale_y_discrete(name = "Effort rating", 
                     limits = c("Unsatisfactory", "Fair", "Good", "Very Good", "Outstanding")) +
    scale_fill_brewer(palette = "Paired", type = "div")
  return(g)
}




g <- effortPlot.bysubject(efdat[efdat$Student.ID == 1000,], "Test chart")
ggplotly(g)
