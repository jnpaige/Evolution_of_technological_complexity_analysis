library(ggplot2)
library(brms)
library(here)
library(gridExtra)

setwd(here::here())
source("Prepare data script.R")

setwd(paste(here::here(),"/Models",sep="",collapse=""))
m2<-readRDS("m2.Rds")
m2_early<-readRDS("m2_early.Rds")
m2_late<-readRDS("m2_late.Rds")

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
selected_values <- c(100, 500, 1000, 2000, 3000)
custom_breaks <- (selected_values - log_mean) / log_sd
custom_labels <- c("100 kya", "", "1 mya", "2 mya", "3 mya")

# Create  plot data
plot_data <- data.frame(
  mean_age = newd$mean_age,
  quantiles_high = quantiles_high,
  quantiles_low = quantiles_low,
  medians = medians)

# Main Figure
main <- ggplot() +
  geom_ribbon(data = plot_data, aes(x = mean_age, ymin = quantiles_low, ymax = quantiles_high), fill = "lightcoral", alpha = 0.35) +  # JON: Other option that looks good is skyblue, alpha 0.4
  geom_point(data = d, aes(x = mean_age, y = PU), shape = 1, color = "black", size = 1.5, stroke = 1) +
  geom_line(data = plot_data, aes(x = mean_age, y = medians), color = "black", size = 0.75) +
  geom_hline(yintercept = 6, color = "black", size = 0.5, linetype = "dashed")+
  scale_x_reverse(breaks = custom_breaks, labels = custom_labels) +
  labs(x = "Years before present", y = "Procedural Units") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 12, margin = margin(t = 15, b = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15, l = 10)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

# Save figure
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("figure1.pdf", main, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("figure1.tiff", main, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


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
ggsave("SIfig7.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig7.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


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
ggsave("SIfig8.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig8.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


