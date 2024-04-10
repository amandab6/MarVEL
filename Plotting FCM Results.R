#Make a habit of removing objects in working environment
rm(list=ls())

#Read in libraries (what packages are needed for the project) 
library(ggplot2)
library(readxl)
library(ggsci)

#extract desired file from computer 
FCM.outputs <- read_excel('/Users/amandalaughlin/Desktop/Data /REPEAT_GRAZING_03_2024/FCM/FCM Outputs (Mixo + Pro).xls', sheet = "FCM Calculations")
Combined.FCM.outputs <- read_excel('/Users/amandalaughlin/Desktop/Data /REPEAT_GRAZING_03_2024/FCM/FCM Outputs (Mixo + Pro).xls', sheet = "Sheet1")

#extract certain columns from the FCM.outputs file 
cell.counts <- FCM.outputs[, c(2,3,12,14)]

#make separate data frames for Pro and Mixo counts 
pro.counts <- cell.counts[, c(1,2,4)]
mixo.counts <- cell.counts[, c(1,2,3)]

#graph the data frames
ggplot(pro.counts, aes(Day,Pro_cells_per_mL, group = Treatment, color = Treatment))+geom_line() + 
  ggtitle("Grazing Experiment March 2024 FCM Results: Pro") + scale_color_d3(palette = "category20") + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))


ggplot(mixo.counts, aes(Day,Mixo_cells_per_mL, group = Treatment, color = Treatment))+geom_line() + 
  ggtitle("Grazing Experiment March 2024 FCM Results: Mixotrophs") + scale_color_d3(palette = "category20") + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))

#graph combined data frame but making each treatment type have its own graph 
ggplot(Combined.FCM.outputs, aes(Day,Cells_per_mL, group = Category, color = Category))+geom_line()+facet_wrap(~Treatment) + 
  ggtitle("Grazing Experiment March 2024 FCM Results") + 
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))



