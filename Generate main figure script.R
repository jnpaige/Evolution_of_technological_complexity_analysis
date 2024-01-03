library(ggplot2)
library(brms)
library(here)
library(gridExtra)

setwd(here::here())
source("Prepare data script.R")

## Prepare data for figure 1.
d<-d[order(-(d$original_age)),] 
d$original_age<-d$original_age/1000
d$Late.age<-d$Late.age/1000
d$Early.age<-d$Early.age/1000


isol.cum.max<-function(x){
  x<-x[order(-x$original_age),] ###This step is crucial to measuring rates later on.
  script<-cummax(x$PU)
  scriptindex<-c(1,1+which(diff(script)!=0))
  x$top<-FALSE
  x$top[scriptindex]<-TRUE
  
  return(x)
}

d<-isol.cum.max(d)
tops<-subset(d,top==TRUE) # Set up separate df with just the cumulative maxima

colors<- c("grey50", "black")
text.df<-data.frame(
  x=c(-.18,-.18,-.18,-.18,-.18), 
  y=c(6,5,3,2,1),  
  text=c("1","2","3","4","5"))

line.df<-data.frame(
  x=c(-.1,-.1,-.1,-.1,-.1), 
  xend=c(3.5,3.5,3.5,3.5,3.5),
  y=c(1,2,3,5,6),
  yend=c(1,2,3,5,6))

max(d$original_age)
p1<-ggplot(data=d, aes(x=original_age, y=PU,color=top))+
  scale_x_reverse()+
  scale_color_manual(values=c("grey50", "black"))+
  theme_minimal()+
  annotate("rect", xmin=-.1, xmax=3.5, ymin=0, ymax=6, alpha=0.1, fill="blue") +
  geom_point() +
  geom_errorbar(data=tops,inherit.aes = FALSE,aes(xmin=Early.age, xmax=Late.age , y=PU))+
  geom_segment(data=line.df, inherit.aes=FALSE, linetype = "dashed", linewidth=.35, alpha=.5, aes(y = y, x=x, yend=yend, xend=xend))+
  geom_text(data=text.df,inherit.aes=FALSE, aes(x=x, y=y,label=text)) + 
  guides(color="none")+
  labs(y = "Procedural Units", x = "Age (mya)")

p1
setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("Figure.1.pdf",p1, device = "pdf", width =6, height = 4, units = "in", dpi=1200)




###
###Figures 2 and 3
###

setwd(here::here())
source("Prepare data script.R")

setwd(paste(here::here(),"/Models",sep="",collapse=""))
m3.1<-readRDS("m3.1.Rds")


# FIGURE 2
# Sample from posteriors
newd <- data.frame(age = seq(-2, 1.65, by = 0.01))
pred <- posterior_predict(m3.1, newdata = newd, re_formula = NA, ndraw = 4000)  # Global grand mean, no site-specific deviations

# Calculate prediction interval
medians <- apply(pred, 2, mean)
quantiles_high <- apply(pred, 2, quantile, probs = 0.9)  # 90th percentile prediction intervals
quantiles_low <- apply(pred, 2, quantile, probs = 0.1)


# Prepare unlogged, unscaled x-axis custom labels
log_mean <- mean(log10(d$original_age))  # Mean of original scale for age
log_sd <- sd(log10(d$original_age))      # Standard deviation of original scale for age

selected_values <- c(1, 10, 100, 1000, 3000)
custom_breaks <- (log10(selected_values) - log_mean) / log_sd
custom_labels <- c("1 kya", "10 kya", "100 kya","1 mya", "3 mya")

# Create log-scale plot data
plot_data <- data.frame(
  mean_age = newd$age,
  quantiles_high = quantiles_high,
  quantiles_low = quantiles_low,
  medians = medians
)

# Log-scale plot
p1 <- ggplot() +
  geom_point(data = d, aes(x = age, y = PU), shape = 1, color = "black", size = 2, stroke = 1.5) +
  geom_ribbon(data = plot_data, aes(x = mean_age, ymin = quantiles_low, ymax = quantiles_high), fill = "blue", alpha = 0.2) +
  geom_line(data = plot_data, aes(x = mean_age, y = medians), color = "black", size = 1) +
  scale_x_reverse(breaks = custom_breaks, labels = custom_labels) +
  labs(x = "Years before present", y = "PU") +
  theme_classic() +
  theme(panel.grid.major = element_line(color = "light gray",
                                        size = 0.5,
                                        linetype = 1),
        axis.title.x = element_text(size = 12, margin = margin(t = 15, b = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 15, l = 10)),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

p1


# Create two-panel plot and save as PDF

setwd(paste(here::here(),"/Figures",sep="",collapse=""))
fig1 <- grid.arrange(linearplot, logplot, ncol = 2)
ggsave("Figure1.pdf", fig1, device = "pdf", width = 15, height = 6, units = "in", dpi=1200)

# FIGURE 2 - Probability that a PU threshold has been crossed as a function of time

# Define thresholds
th7 <- 7
th8 <- 8
th10 <- 10
th12 <- 12

# Calculate probabilities for each threshold
p7 <- apply(pred, 2, function(column) mean(column >= th7))
p8 <- apply(pred, 2, function(column) mean(column >= th8))
p10 <- apply(pred, 2, function(column) mean(column >= th10))
p12 <- apply(pred, 2, function(column) mean(column >= th12))

# Prepare data frame for plotting
data <- data.frame(
  age = newd$age,
  p7 = p7,
  p8 = p8,
  p10 = p10,
  p12 = p12
)

# Define label positions for text annotations
label_positions <- data.frame(
  age = c(0.43, 0.4, 0.15, -0.05),
  value = c(0.7, 0.57, 0.39, 0.23),
  line = c("PU>6", "PU>7", "PU>9", "PU>12")
)
# Prepare unlogged, unscaled x-axis custom labels
log_mean <- mean(log10(d$original_age))  # Mean of original scale for age
log_sd <- sd(log10(d$original_age))      # Standard deviation of original scale for age

selected_values <- c(1, 10, 100, 1000, 3000)
custom_breaks <- (log10(selected_values) - log_mean) / log_sd
custom_labels <- c("1 kya", "10 kya", "100 kya","1 mya", "3 mya")
reversed_breaks <- rev(custom_breaks)
reversed_labels <- rev(custom_labels)


# Create ggplot without legend
p2 <- ggplot(data, aes(x = age)) +
  geom_smooth(aes(y = p7), se = FALSE) +
  geom_smooth(aes(y = p8), se = FALSE) +
  geom_smooth(aes(y = p10), se = FALSE) +
  geom_smooth(aes(y = p12), se = FALSE) +
  geom_text(data = label_positions, aes(label = line, y = value), hjust = 1, vjust = 1, fontface = "bold") +
  labs(x = "Years before present", y = expression(P(PU > x))) +
  scale_x_reverse(breaks = reversed_breaks, labels = reversed_labels) +
  theme_classic() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major = element_line(color = "light gray",
                                    size = 0.5,
                                    linetype = 1),
    axis.title.x = element_text(size = 12, margin = margin(t = 15, b = 10)),
    axis.title.y = element_text(size = 12, angle = 90, vjust = 0.5, margin = margin(r = 20, l = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(.5,1.0,.5,.5, "cm"),
    legend.position = "none"
  )

fig2
# Save the plot as PDF


setwd(paste(here::here(),"/Figures",sep="",collapse=""))
fig <- grid.arrange(logplot, p2, ncol = 2)
ggsave("Figure.2.pdf", fig, device = "pdf", width = 15, height = 6, units = "in", dpi=1200)
