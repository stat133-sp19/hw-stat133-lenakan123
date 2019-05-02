#title: make-shot-charts-script.R
#description: the script will help generate charts of shots made in the games  
#input(s): R commands, ggplot library, 
#output: #kevin-durant-shot-chart.pdf #andre-iguodala-shot-chart.pdf #stephen-curry-shot-chart.pdf 
#draymond-green-shot-chart.pdf #gsw-shot-charts.pdf #klay-thompson-shot-chart.pdf 

setwd("C:/Users/lenak/Desktop/hw-stat133/workout01")
getwd() 

##Play around with Klay Thompson 
library(ggplot2)
klay_scatterplot <- ggplot(data = thompson) + geom_point(aes(x=x, y=y, color=shot_made_flag))
library(jpeg)
library(grid)
court_file <- "./images/nba-court.jpg" ##court image (to be used as backgroudn of plot)
##create raste project
court_image <- rasterGrob(readJPEG(court_file), width = unit(1, "npc"), height = unit(1, "npc"))
##shot chart with court background
klay_shot_chart <- ggplot(data=thompson)+annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x, y=y, color = shot_made_flag))+
  ylim(-50, 420)+
  ggtitle('Shot Chart: Klay Thompson (2016 season)')+
  theme_minimal()

##Shot charts of each player 
green_scatterplot <- ggplot(data = green) + geom_point(aes(x=x, y=y, color=shot_made_flag))
durant_scatterplot <- ggplot(data = durant) + geom_point(aes(x=x, y=y, color=shot_made_flag))
iguodala_scatterplot <- ggplot(data = iguodala) + geom_point(aes(x=x, y=y, color=shot_made_flag))
curry_scatterplot <- ggplot(data = curry) + geom_point(aes(x=x, y=y, color=shot_made_flag))

iguodala_shot_chart <- ggplot(data=iguodala)+annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x, y=y, color = shot_made_flag))+
  ylim(-50, 420)+
  ggtitle('Shot Chart: Andre Iguodala (2016 season)')+
  theme_minimal()

green_shot_chart <- ggplot(data=green)+annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x, y=y, color = shot_made_flag))+
  ylim(-50, 420)+
  ggtitle('Shot Chart: Draymond Green (2016 season)')+
  theme_minimal()

durant_shot_chart <- ggplot(data=durant)+annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x, y=y, color = shot_made_flag))+
  ylim(-50, 420)+
  ggtitle('Shot Chart: Kevin Durant (2016 season)')+
  theme_minimal()

curry_shot_chart <- ggplot(data=curry)+annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x, y=y, color = shot_made_flag))+
  ylim(-50, 420)+
  ggtitle('Shot Chart: Stephen Curry (2016 season)')+
  theme_minimal()

##create pdfs out of each chart 
pdf(file = "./images/andre-iguodala-shot-chart.pdf", width=6.5, height =5)
iguodala_shot_chart
dev.off() ##Andre
pdf(file = "./images/stephen-curry-shot-chart.pdf", width=6.5, height =5)
curry_shot_chart
dev.off() ##curry
pdf(file = "./images/kevin-durant-shot-chart.pdf", width=6.5, height =5)
durant_shot_chart
dev.off() ## durant 
pdf(file = "./images/draymond-green-shot-chart.pdf", width=6.5, height =5)
green_shot_chart
dev.off() ##Green 
pdf(file = "./images/klay-thompson-shot-chart.pdf", width=6.5, height =5)
klay_shot_chart
dev.off() ##Klay

##Facet Shot Chart

facet_shot_chart <- ggplot(data=stacked_tables)+annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x, y=y, color = shot_made_flag))+
  ylim(-50, 420)+
  ggtitle('Shot Charts: GSW (2016 Season)')+
  theme_minimal()+facet_wrap(~name)+scale_color_manual(values=c("#E69F00", "#56B4E9"))

pdf(file = "./images/gsw-shot-charts.pdf", width=8, height = 7)
facet_shot_chart
dev.off()

png(filename = "./images/gsw-shot-charts.png")
facet_shot_chart
dev.off()

##png of scatterplots 
png(filename = "./images/curry-shot-chart.png")
curry_shot_chart
dev.off()

png(filename = "./images/thompson-shot-chart.png")
klay_shot_chart
dev.off()
