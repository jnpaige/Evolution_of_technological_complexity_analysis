library(RCurl)
library(here)
library(ggplot2)

#load in the appropriate version of the procedural unit dataset.
#Generated using procedural unit codebook version 1.0

download.file("https://raw.githubusercontent.com/jnpaige/Procedural.unit.dataset/main/Dataset/procedural%20units%20obfus.csv",
              destfile = "procedural_units_v_1.0.csv", method = "curl")

##
##Clean data/select only Single chains
##

setwd(here::here())
data<-read.csv("procedural_units_v_1.0.csv")
data<-subset(data, Single.Chain=="Yes") # Remove rows describing PU present across all chains
data<-subset(data, Arch==1)       #get only archaeological cases
data<-data[,grep("Source|Sitename|KA.young|KA.old|Lat|Long|Species.attribution|Descr.|PU",names(data))]
data$PU<-rowSums(data[,grep("PU",names(data))]) #assign a total number of procedural units to each row
data[, grep("PU", names(data))]<-lapply(data[, grep("PU", names(data))], as.numeric)
data <- subset(data, data$KA.young > 1) # Eliminate data points younger than 1000 years BP
names(data)[2:4]<-c("site","Early.age","Late.age")

#scale and log transform age to help model behave well.
data$age <- scale(log10((data$Early.age+data$Late.age)/2)) # Midpoint between upper and lower bound
data$original_age<-(data$Early.age+data$Late.age)/2
write.csv(data, file="Paige and Perreault Dataset.csv")
d <- read.csv("Paige and Perreault Dataset.csv", header = TRUE)
data<-NULL
