library(ggplot2)
library(brms)
library(here)
library(gridExtra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


setwd(here::here())
source("Prepare data script.R")

setwd(paste(here::here(),"/Models",sep="",collapse=""))
m2<-readRDS("m2.Rds")
m2_early<-readRDS("m2_early.Rds")
m2_late<-readRDS("m2_late.Rds")
m2_priors<-readRDS("m2_priors.rds")
d
###
###Figure
###





# Sample from posteriors of mean age, early age, and late age
newd <- data.frame(mean_age = seq(-0.66, 3.3, by = 0.01))
pred <- posterior_predict(m2, newdata = newd, re_formula = NA, ndraw = 4000, ntrys=50)  # Global grand mean, no site-specific deviations. Ntrys =20:  Warning messages of some predicted values invalid are due to the fact that this is a truncated discrete model, see ?posterior_predict

# Calculate prediction interval for mean age
medians <- apply(pred, 2, mean)
quantiles_high <- apply( pred, 2, quantile, probs = 0.9)  # 90th percentile prediction intervals
quantiles_low <- apply(pred, 2, quantile, probs = 0.1)

# Prepare unscaled x-axis custom labels
log_mean <- mean(d$original_age)  # Mean of original scale for age
log_sd <- sd(d$original_age)    # Standard deviation of original scale for age
selected_values <- c(100, 1000, 2000, 3000)
custom_breaks <- (selected_values - log_mean) / log_sd
custom_labels <- c("100 kya", "1 mya", "2 mya", "3 mya")

# Create  plot data
plot_data <- data.frame(
  mean_age = newd$mean_age,
  quantiles_high = quantiles_high,
  quantiles_low = quantiles_low,
  medians = medians)

# Main Figure
main <- ggplot() +
  geom_ribbon(data = plot_data, aes(x = mean_age, ymin = quantiles_low, ymax = quantiles_high), fill = "lightcoral", alpha = 0.35) +  # JON: Other option that looks good is skyblue, alpha 0.4
  geom_point(data = d, aes(x = mean_age, y = PU), shape = 16, fill="black", color = "black", size = 1.5, stroke = 1) +
  geom_line(data = plot_data, aes(x = mean_age, y = medians), color = "black", size = 0.75) +
  geom_hline(yintercept = 6, color = "black", size = 0.5, linetype = "dashed")+
  scale_x_reverse(breaks = custom_breaks, labels = custom_labels) +
  labs(x = "Years before present", y = "Procedural Units") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15, b = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15, l = 10)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

main
# Save figure
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("figure1.pdf", main, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("figure1.tiff", main, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


## Main points only

main <- ggplot() +
  geom_point(data = d, aes(x = mean_age, y = PU), shape = 16, fill="black", color = "black", size = 1.5, stroke = 1) +
  scale_x_reverse(breaks = custom_breaks, labels = custom_labels) +
  labs(x = "Years before present", y = "Procedural Units") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15, b = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15, l = 10)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

main
# Save figure
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("figure1_nolines.pdf", main, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("figure1_nolines.tiff", main, device = "tiff", width = 6, height = 4, units = "in", dpi=600)

#Main horizontal lines only
main <- ggplot() +
  geom_point(data = d, aes(x = mean_age, y = PU), shape = 16, fill="black", color = "black", size = 1.5, stroke = 1) +
  scale_x_reverse(breaks = custom_breaks, labels = custom_labels) +
  labs(x = "Years before present", y = "Procedural Units") +
  theme_classic() +
  geom_hline(yintercept = 6, color = "black", size = 0.7, linetype = "dotted")+
  geom_hline(yintercept = 5, color = "black", size = 0.7, linetype = "dashed")+
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15, b = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15, l = 10)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

main


# Save figure
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("figure1_hlines.pdf", main, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("figure1_hlines.tiff", main, device = "tiff", width = 6, height = 4, units = "in", dpi=600)



# SI figures

data<-d
sites<-data[, grep("^latitude$|^longitude$", names(data))]
names(sites)<-c("latitude","longitude")
sites$grp<-"Assemblages sampled"
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")

text.size<-12
shape.size<-3
map<-ggplot(data = world) +
  geom_sf(fill="gray") +
  geom_sf(data = sites, size = shape.size, shape = 23, color="black", aes(fill = grp)) +
  coord_sf(xlim = c(-170, 180), ylim = c(-50, 75), expand = FALSE) +
  theme_bw()+
  theme(text = element_text(size=text.size),
        legend.position = c(0.7, 0.1),
        legend.title=element_text(color=NA),
        legend.background = element_rect(fill=NA),
        legend.key=element_blank()) 


setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("map.pdf", map, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("map.tiff", map, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


# Simulate data from Poisson Lambda priors
n <- 1000
intercepts <- rnorm(n, mean = 2, sd = 0.5) # Sample intercepts (normal distribution)
lambdas <- exp(intercepts)                 # Generate rates (lambda)
data <- sapply(lambdas, rpois, n = 1)      # Generate Poisson data

# Plot histogram of simulated Poisson data
p<-ggplot(data = data.frame(Value = data), aes(x = Value)) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  labs(title = 'Histogram of Simulated Poisson Data', x = 'PU', y = 'Frequency')
p
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("figure2.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("figure2.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)




# Sample from the priors to simulate regression lines
n_samples <- 100
predictor_range <- seq(from = -2, to = 2, length.out = 100)
intercepts <- rnorm(n_samples, mean = 2, sd = 0.5)  # Sample intercepts
betas <- rnorm(n_samples, mean = -0.2, sd = 0.1)    # Sample beta coefficients

# Prepare data for plotting
plot_data <- expand.grid(mean_age = predictor_range, sample = 1:n_samples)
plot_data$lambda <- exp(intercepts[plot_data$sample] + betas[plot_data$sample] * plot_data$mean_age)

# Plot regression lines
p<-ggplot(plot_data, aes(x = -mean_age, y = lambda, group = sample, color = as.factor(sample))) +
  geom_line(show.legend = FALSE) +
  theme_minimal() +
  labs(title = 'Sampled Regression Lines from Priors', x = 'Age', y = 'PU')
p
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("SIfig3.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig3.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


# Sample from the model's prior and plot one example of the simulated data
newd_prior <- data.frame(mean_age = seq(3.3, -0.65, by = -0.01))
pred_prior <- posterior_predict(m2_priors, newdata = newd_prior, re_formula = NA, ndraw = 1000)
n <- sample(1:1000, size = 1)  # Pick one of the samples

# Plotting sampled data
p<-ggplot(newd_prior, aes(x = -mean_age, y = pred_prior[n,])) +
  geom_point(shape = 21, fill = NA, color = "black") +
  labs(x = "Log age (std)", y = "PU", title = "Sampling from the Priors") +
  theme_minimal()

p
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("SIfig4.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig4.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


## Posterior draws

p<-pp_check(m2)

setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("SIfig5.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig5.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)

#  Probability that a PU threshold has been crossed as a function of time

# Calculate probabilities for each threshold
p6 <- apply(pred, 2, function(column) mean(column > 6)) 
p7 <- apply(pred, 2, function(column) mean(column > 7))
p8 <- apply(pred, 2, function(column) mean(column > 8))

# Prepare data frame for plotting
data <- data.frame(
  age = newd$mean_age,
  p6 = p6,
  p7 = p7,
  p8 = p8)

# Prepare unscaled x-axis custom labels
log_mean <- mean(d$original_age)  # Mean of original scale for age
log_sd <- sd(d$original_age)      # Standard deviation of original scale for age
selected_values <- c(100, 1000, 2000, 3000)
custom_breaks <- (selected_values - log_mean) / log_sd
custom_labels <- c("100 kya", "1 mya", "2 mya", "3 mya")
reversed_breaks <- rev(custom_breaks)
reversed_labels <- rev(custom_labels)

# Define label positions for text annotations
label_positions <- data.frame(
  age = c(0.8, 0.8, 0.8),
  value = c(0.40, 0.27, 0.18),
  line = c("PU>6", "PU=7", "PU>8"))

# Create ggplot without legend
p<-ggplot(data, aes(x = age)) +
  geom_smooth(aes(y = p6), color="darkblue", size = 0.75) +
  geom_smooth(aes(y = p7), color="darkblue", size = 0.75) +
  geom_smooth(aes(y = p8), color="darkblue", size = 0.75) +
  geom_text(data = label_positions, aes(label = line, y = value), size = 3.5, color = "darkblue", hjust = 1, fontface = "bold", vjust = 1, angle = c(21, 17, 17) )+
  geom_hline(yintercept = 0.5, color = "black", size = 0.4)+
  labs(x = "Years before present", y = expression(Prob(PU > x))) +
  scale_x_reverse(breaks = reversed_breaks, labels = reversed_labels) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(color = "#E5E5E5", size = 0.2),
    panel.grid.major.x = element_line(color = "#E5E5E5", size = 0.2),
    axis.title.x = element_text(size = 12, margin = margin(t = 15, b = 10)),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 0.5, margin = margin(r = 20, l = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(.5,1.0,.5,.5, "cm"),
    legend.position = "none")

setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("SIfig6.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig6.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


# Plot posteriors of Early Age and Late Age models

# Sample from posteriors of mean age, early age, and late age

newd_e <- data.frame(early_age = seq(-0.66, 3.3, by = 0.01))
pred_e <- posterior_predict(m2_early, newdata = newd_e, re_formula = NA, ndraw = 4000, ntrys=50)  # Global grand mean, no site-specific deviations. Ntrys =20:  Warning messages of some predicted values invalid are due to the fact that this is a truncated discrete model, see ?posterior_predict

newd_l <- data.frame(late_age = seq(-0.66, 3.3, by = 0.01))
pred_l <- posterior_predict(m2_late, newdata = newd_l, re_formula = NA, ndraw = 4000, ntrys=50)  # Global grand mean, no site-specific deviations. Ntrys =20:  Warning messages of some predicted values invalid are due to the fact that this is a truncated discrete model, see ?posterior_predict

# SI Early and Late Age Figures
# Calculate prediction interval for early age
medians_e <- apply(pred_e, 2, mean)
quantiles_high_e <- apply( pred_e, 2, quantile, probs = 0.9)  # 90th percentile prediction intervals
quantiles_low_e <- apply(pred_e, 2, quantile, probs = 0.1)

# Calculate prediction interval for late age
medians_l <- apply(pred_l, 2, mean)
quantiles_high_l <- apply( pred_l, 2, quantile, probs = 0.9)  # 90th percentile prediction intervals
quantiles_low_l <- apply(pred_l, 2, quantile, probs = 0.1)

# Create  plot data
plot_data <- data.frame(
  mean_age = newd$mean_age,
  quantiles_high = quantiles_high,
  quantiles_low = quantiles_low,
  medians = medians,
  early_age<-newd_e$early_age,
  quantiles_high_e = quantiles_high_e,
  quantiles_low_e = quantiles_low_e,
  medians_e = medians_e,
  late_age<-newd_l$late_age,
  quantiles_high_l = quantiles_high_l,
  quantiles_low_l = quantiles_low_l,
  medians_l = medians_l)


p<-ggplot() +
  geom_point(data = d, aes(x = mean_age, y = PU), shape = 1, color = "black", size = 1.5, stroke = 1) +
  geom_ribbon(data = plot_data, aes(x = mean_age, ymin = quantiles_low, ymax = quantiles_high), fill = "lightgray", alpha = 0.5) +  # JON: Other option that looks good is skyblue, alpha 0.4
  geom_line(data = plot_data, aes(x = mean_age, y = medians), color = "black", size = 0.75) +
  geom_hline(yintercept = 6, color = "black", size = 0.5, linetype = "dashed")+
  geom_ribbon(data = plot_data, aes(x = early_age, ymin = quantiles_low_e, ymax = quantiles_high_e), fill = "lightgray", alpha = 0.5) +  # JON: Other option that looks good is skyblue, alpha 0.4
  geom_ribbon(data = plot_data, aes(x = late_age, ymin = quantiles_low_l, ymax = quantiles_high_l), fill = "lightgray", alpha = 0.5) +  # JON: Other option that looks good is skyblue, alpha 0.4
  geom_line(data = plot_data, aes(x = early_age, y = medians_e), color = "black", size = 0.75) +
  geom_line(data = plot_data, aes(x = late_age, y = medians_l), color = "black", size = 0.75) +
  geom_point(data = d, aes(x = mean_age, y = PU), shape = 1, color = "black", size = 1.5, stroke = 1) +
  scale_x_reverse(breaks = custom_breaks, labels = custom_labels) +
  labs(x = "Years before present", y = "Procedural Units") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15, b = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15, l = 10)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

p
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("SIfig7.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig7.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


