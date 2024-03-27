# Tips and tricks for successful coding:
#   1. Do not put spaces or special characters in file or folder names. Stick to using underscores (_) or dashes (-)
#   2. Errors from specific functions often arise because and object of incorrect class was used. e.g. the function `mean()`        is looking for numerics and not characters (you can't find the mean of letters). If you encounter and error, a good           place to start debugging is to use the `class()` function on the object that you're trying to evaluate.
#   3. Syntax is really important. R (and other coding languages) is case sensitive, reacts poorly to poor punctuation, and         has a weird rule about spaces. Before throwing yourself deep into debugging, make sure you didn't make a simple error         like misspelling the name of an object.   
#   4. Plotting is a skill in itself. Your code could be running beautifully and then you'll hit a wall when you try to plot       something. Pay special attention to what your x and y limits are becaue if they can't fit in the plotting window then         you're in big trouble. Rstudio is notorious for having issues with it's plot window. You can either click the 'Zoom'         button in the plot window to expand your plot and margins. If that does not work, try opening base R and running your         script.
#   


#Make a habit of removing objects in working environment
rm(list=ls())

#Read in libraries
install.packages("readxl")
install.packages("lubridate")
library(readxl)
library(lubridate)

#setwd('/Users/amandalaughlin/Desktop/PhD Year 1/Data') 
#it's a good habit to set your working directory at the beginning of your script. However, you don't always have to do this if you provide path names for all your data files.

folders=list.files('', full.names=TRUE) ##add folder
#well.label.files=list.files('', full.names=TRUE)
well.label<-read_xlsx('')


#for loops can get really complicated. If you get stuck, you can always set the index variable, in this case `i` to 1 and see what value the loop gives you. e.g. i=1, print(folders[i]) to see if the correct file name is printed
for (i in 1:length(folders)){
  
  datanow = read_excel(folders[i], range = 'B53:M60', col_names = FALSE)
  
  data.date = as_datetime(as.character(read_excel(folders[i], range = 'E6', col_names = FALSE)), tz="HST")
  
  ##the above line of code caused us a lot of problems because we gave the `as_datetime()` the wrong object class. The date was being read in as a character instead of an integer. I could have saved myself a lot of headacjes of I just checked the class of the object. 
  
  well.label$RFU = as.vector(t(as.matrix(datanow[,])))
  well.label$date = data.date
  
  if (i == 1) plate.timeseries = well.label
  if (i > 1) plate.timeseries = rbind(plate.timeseries, well.label)
  
}
plate.df<-plate.timeseries #renaming data frame for simplicity

##Adding columns to `plate.df` data frame to creat unique IDs for each well/dilution/virus
plate.df$Virus.Dilution <-paste(plate.df$Virus, plate.df$Dilution)
plate.df$unique.well<-paste(plate.df$Virus, plate.df$Dilution, plate.df$Well)

#'NAs' can cause problems in your code, causing objects to be the wrong class. Here's how you change your NA's to zeroes.
#plate.df[is.na(plate.df)] <-0

#We only need the date, not the time, of your reading. `substr()` allows you to select specific digits or characters in an object. In this case, we're telling `substr()` to use the first ten digits of plate.df$date.
#short.date<-substr(plate.df$date, 1,10)

#let's organize the data frame so that each virus dilution is grouped together. This will help plot similar samples next to each other
plate.df<-plate.df[order(plate.df$Virus.Dilution),]

#create a vector with from the `unique.well` column in `plate.df` to index with.
well.vector<-unique(plate.df$unique.well)

#forcing RFU to be an integer and not a character
plate.df$RFU<-as.integer(plate.df$RFU)


install.packages("ggplot2")
library(ggplot2)
plate.df$Dilution=factor(plate.df$Dilution,levels=c("1.0","0.1","0.01","0.001","1.0E-4","1.0E-5","1.0E-6","1.0E-7","1.0E-8","Blank","No Lysate")) 
ggplot(plate.df, aes(date,RFU, group = unique.well))+geom_line()+facet_wrap(~Dilution) + 
  ggtitle("")



install.packages("dplyr")
library(dplyr)
Lyse.day=subset(plate.df,date=="") #this is the date that most of them lysed by I think
Lysis.counts=Lyse.day%>%group_by(Dilution)%>%summarise(lysed=sum(RFU<5000))
Lysis.counts$fraction=as.numeric(as.character(Lysis.counts$Dilution))
Lysis.counts$wells=6
Lysis.counts=subset(Lysis.counts,!Dilution%in%c("Blank","No Lysate"))

install.packages("MPN")
library(MPN)
initial.volume = 0.02 #amount of virus added
#this will work if you have named the columns in your spreadsheet 'lysed', 'wells', 'fraction'. The 'lysed' column is the number of lysed wells at each dilution. The 'wells' column is the total number of wells at each dilution. The 'fraction' column is the dilution fraction at that dilution. E.g. if you have a series of tenfold dilutions this will be c(1, 0.1, 0.01, ...). 
with(Lysis.counts, mpn(lysed, wells, initial.volume*fraction))
#move adjusted and upper and lower bounds for CI into spreadsheet
