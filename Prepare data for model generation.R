library(ggplot2)
setwd(here::here())# Load and prepare the data
data <- read.csv("Paige and Perreault dataset.csv", header = TRUE)
data<-data[which(data$Arch==1),]
data$Total.PU<-rowSums(data[,grep("PU",names(data))])
data$SiteID<-rownames(data)
data$Early_Age<-as.numeric(data$Early_Age)
data$Late_Age<-as.numeric(data$Late_Age)
data$Mean_Age<-as.numeric(data$Mean_Age)
names(data)
data
d <- data.frame(
  original_age = data$Mean_Age,
  original_early = data$Early_Age,
  original_late = data$Late_Age,
  mean_age = scale(data$Mean_Age),
  early_age = scale(data$Early_Age),
  late_age = scale(data$Late_Age),
  PU = data$Total.PU,
  site = data$Site.name,
  latitude = data$Latitude,
  longitude=data$Longitude)

d



