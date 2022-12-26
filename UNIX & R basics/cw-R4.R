humans <- read.csv("Coursework/Data/humans.csv")
View(humans)
humans[10, "Is_Male"] <- "True"
humans[10,"Age"] <- "29"

humans["Age"] <- as.numeric(as.character(humans$Age))
mean(humans$Age)
