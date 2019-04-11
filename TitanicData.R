# Load Raw Data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" varible to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])


# Combnie data sets
data.combined <- rbind(train, test.survived)


# R data types
str(data.combined)

data.combined$survived <- as.factor(data.combined$Survived)
data.combined$pclass <- as.factor(data.combined$Pclass)

# Take a look at gross survival rates
table(data.combined$Survived)

#Distribution across classes
table(data.combined$pclass)

# Load ggplot package for visulization
library(ggplot2)

#Hypothsis Rich survive at higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes( x = Pclass, fill = factor(Survived))) + 
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill = "Survived")

# Examine names in the train data set
head(as.character(train$Name))

# How many unique names are there across Train & Test
length(unique(as.character(data.combined$Name)))

# Find duplicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Look at records in combine data set
data.combined[which(data.combined$Name %in% dup.names),]

# Further look in to titles such as Mr and Miss values
library(stringr)

# Any corralation between Mr and Miss to other variables
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

# Title corrospnde to age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

# Check pattern for men
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# Expand apon relation ships
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  } 
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title <- as.factor(titles) 

library(ggplot2)

ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
  

