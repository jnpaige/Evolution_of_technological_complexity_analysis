setwd(here::here())# Load and prepare the data
data <- read.csv("Paige and Perreault Dataset.csv", header = TRUE)

data <- data[order(-data$Mean_Age), ]  # Sort the data by mean age. This step is necessary to generate the cumulative maxima

d <- data.frame(
  original_age = data$Mean_Age,
  original_early = data$Early_Age,
  original_late = data$Late_Age,
  mean_age = scale(data$Mean_Age),
  early_age = scale(data$Early_Age),
  late_age = scale(data$Late_Age),
  PU = data$Total.PU,
  site = data$SiteID,
  latitude = data$Latitude,
  longitude=data$Longitude)

d


