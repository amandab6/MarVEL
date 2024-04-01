##COMBINING MPN TREATMENTS BY DAY##
rm(list=ls())

#Read in libraries
#install.packages("readxl")
#install.packages("lubridate")
library(readxl)
library(lubridate)

folders=list.files('', full.names=TRUE) ##add folder with all MPN excel output files from one day
well.label<-read_xlsx('') ##add location of excel sheet with well labels for MPN 96-well plate

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

##Adding in the name of each plate ran on the same day (example below is for plates ran for 7 days)
plate.df$Plate <- replace(plate.df$Plate, 1:672, "name of replicate/plate")
plate.df$Plate <- replace(plate.df$Plate, 672:1344, "name of replicate/plate")
plate.df$Plate <- replace(plate.df$Plate, 1344:2016, "name of replicate/plate")
plate.df$Plate <- replace(plate.df$Plate, 2016:2688, "name of replicate/plate")
plate.df$Plate <- replace(plate.df$Plate, 2688:3360, "name of replicate/plate")

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

#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggsci")
library(ggsci)
plate.df$Dilution=factor(plate.df$Dilution,levels=c("1.0","0.1","0.01","0.001","1.0E-4","1.0E-5","1.0E-6","1.0E-7","1.0E-8","Blank","No Lysate")) 
ggplot(plate.df, aes(date,RFU, group = unique.well, color = Dilution))+geom_line()+facet_wrap(~Plate) + 
  ggtitle("Day X Plates") + scale_color_d3(palette = "category20")