setwd(here::here())# Load and prepare the data
data <- read.csv("Paige and Perreault Dataset.csv", header = TRUE)

data <- data[order(-data$Mean_Age), ]  # Sort the data by mean age. This step is necessary to generate the cumulative maxima
d <- data.frame(
  original_age = data$Mean_Age,
  original_early = data$Early_Age,
  original_late = data$Late_Age,
  mean_age = data$Mean_Age,
  early_age = data$Early_Age,
  late_age = data$Late_Age,
  PU = data$Total.PU,
  site = data$SiteID,
)

# Scale age measurements
d$mean_age <- scale((d$mean_age))
d$early_age <- scale((d$early_age))
d$late_age <- scale((d$late_age))

