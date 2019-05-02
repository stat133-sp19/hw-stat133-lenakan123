#title: make shots data R script file
#description: the script is will help prepare a scv file that contains information about the shots data 
#input(s): R commands, csv files that were uncurled into repository, 
#output: #green-summary.txt #thompson-summary.txt #curry-summary.txt #iguodala-summary.txt #durant-summary.txt
#shots-data-summary.txt

library(dplyr)
setwd("C:/Users/lenak/Desktop/hw-stat133/workout01")
getwd() 
datatypes_GSW <- data_types <- c("character", #team_name
                                 "character", #game_date
                                 "character", #season
                                 "numeric", #period
                                 "numeric", #minutes remaining
                                 "numeric", #seconds remaining
                                 "character", #shot_made_falg
                                 "character", #action type
                                 "character", #shot type
                                 "numeric", #shot distance
                                 "character", #opponent
                                 "numeric", #x
                                 "numeric" #y
                                 
)
curry <- read.csv("./data/stephen-curry.csv", stringsAsFactors = FALSE, colClasses = datatypes_GSW)
thompson <- read.csv("./data/klay-thompson.csv", stringsAsFactors = FALSE, colClasses= datatypes_GSW)
green <- read.csv("./data/draymond-green.csv", stringsAsFactors = FALSE, colClasses = datatypes_GSW)
iguodala <- read.csv("./data/andre-iguodala.csv", stringsAsFactors = FALSE, colClasses = datatypes_GSW)
durant <- read.csv("./data/kevin-durant.csv", stringsAsFactors=FALSE, colClasses = datatypes_GSW)

library(dplyr)
curry <- mutate(curry, name = c("Stephen Curry")) ##curry$name <- c("stephen curry")
thompson <- mutate(thompson, name = c("Klay Thompson"))
green <- mutate(green, name = c("Draymond Green"))
iguodala <- mutate(iguodala, name = c("Andre Iguodala"))
durant <- mutate(durant, name=c("Kevin Durant"))

##replace shot made flag: 
curry$shot_made_flag[curry$shot_made_flag == "n"] <- c("shot_no")
curry$shot_made_flag[curry$shot_made_flag == "y"] <- c("shot_yes")
thompson$shot_made_flag[thompson$shot_made_flag == "n"] <- c("shot_no")
thompson$shot_made_flag[thompson$shot_made_flag == "y"] <- c("shot_yes")
green$shot_made_flag[green$shot_made_flag == "n"] <- c("shot_no")
green$shot_made_flag[green$shot_made_flag == "y"] <- c("shot_yes")
iguodala$shot_made_flag[iguodala$shot_made_flag == "n"] <- c("shot_no")
iguodala$shot_made_flag[iguodala$shot_made_flag == "y"] <- c("shot_yes")
durant$shot_made_flag[durant$shot_made_flag == "n"] <- c("shot_no")
durant$shot_made_flag[durant$shot_made_flag == "y"] <- c("shot_yes")

##minute number where a shot occurred: 
curry <- mutate(curry, minute = period*12-minutes_remaining) 
thompson <- mutate(thompson, minute = period*12-minutes_remaining) 
green <- mutate(green, minute = period*12-minutes_remaining) 
iguodala <- mutate(iguodala, minute = period*12-minutes_remaining) 
durant <- mutate(durant, minute = period*12-minutes_remaining) 

##sink and summary output 
sink(file = './output/curry-summary.txt')
summary(curry)
sink() ## sink curry's file 
sink(file = './output/thompson-summary.txt')
summary(thompson)
sink() ## sink Thompson's file
sink(file = './output/green-summary.txt')
summary(green)
sink() ##sink green's file
sink(file = './output/iguodala-summary.txt')
summary(iguodala)
sink() ## sink iguodala's file 
sink(file = './output/durant-summary.txt')
summary(durant)
sink() ##sink durant's file 

##Use the row binding function rbind() to stack the tables into one single data frame
stacked_tables <- rbind(curry, thompson, green, durant, iguodala)

##export table into CSV file shots-data.csv
write.csv(
  x = stacked_tables, # R object to be exported
  file = './data/shots-data.csv'  # file path
)

##summary of the exported table 
sink(file='./output/shots-data-summary.txt')
summary(stacked_tables)
sink()
