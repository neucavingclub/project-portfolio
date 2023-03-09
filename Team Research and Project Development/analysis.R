library(tidyverse)                            #loading the necessary libraries
                           
df <- read.csv("Fitness_Data.csv")            #loading our data intoa  dataframe

steps <- as.numeric(df$Steps)                 #initialising our steps value and converting them to numeric values 

diet.type <- df$Diet.Type                     #initialising the categories

aggregate(steps ~ diet.type, FUN=mean)        #aggregating the data and computing the mean of each category

result <- pairwise.t.test(steps, diet.type, paired=FALSE, p.adj = "holm")     #performing the t test and storing results in the result variable

print(result)                                 #printing the result of the test