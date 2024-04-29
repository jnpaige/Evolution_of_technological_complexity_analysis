# Load necessary libraries
library(here)
library(brms)
library(splines)
library(ggplot2)
library(gridExtra)

# Load and prepare dataset
setwd(here::here())
source("Prepare data script.R")

# Simulate data from Poisson Lambda priors
n <- 1000
intercepts <- rnorm(n, mean = 2, sd = 0.5) # Sample intercepts (normal distribution)
lambdas <- exp(intercepts)                 # Generate rates (lambda)
data <- sapply(lambdas, rpois, n = 1)      # Generate Poisson data

trunc<-2
# Plot histogram of simulated Poisson data
ggplot(data = data.frame(Value = data), aes(x = Value)) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  labs(title = 'Histogram of Simulated Poisson Data', x = 'PU', y = 'Frequency')
v<-2

####
#### Model selection. 
####

# Intercept only
m0<- brm(PU|trunc(lb=2) ~ 1+ (1|site),
         family = poisson(),
         data = d,
         iter = 10000, warmup = 5000,
         prior = c(set_prior("normal(2, 0.5)", class = "Intercept")),
         save_pars = save_pars(all = TRUE)) # Save all parameters of the model


# With interaction
m1<- brm(PU |trunc(lb=2) ~ mean_age + (1|site),  
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     set_prior("normal(-0.2, 0.1)", class = "b")),
           control = list(adapt_delta = 0.9),  # Increase adapt_delta to avoid divergences
           save_pars = save_pars(all = TRUE))  # Increase adapt_delta to avoid divergences) 


# With smooth spline interaction
m2<- brm(PU |trunc(lb=2) ~ s(mean_age, k = 14) + (1|site),
         family = poisson(),
         data = d,
         iter = 10000, warmup = 5000,
         prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                   set_prior("normal(-0.2, 0.1)", class = "b", coef = "smean_age_1")),  # Adjust prior for smooth term
         control = list(adapt_delta = 0.99),
         save_pars = save_pars(all = TRUE))

# With smooth spline interaction, using Early_Age as predictor
m2_early<- brm(PU |trunc(lb=2) ~ s(early_age, k = 14) + (1|site),
         family = poisson(),
         data = d,
         iter = 10000, warmup = 5000,
         prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                   set_prior("normal(-0.2, 0.1)", class = "b", coef = "searly_age_1")),  # Adjust prior for smooth term
         control = list(adapt_delta = 0.99),
         save_pars = save_pars(all = TRUE))

# With smooth spline interaction, using Late_Age as predictor
m2_late<- brm(PU |trunc(lb=2) ~ s(late_age, k = 14) + (1|site),
               family = poisson(),
               data = d,
               iter = 10000, warmup = 5000,
               prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                         set_prior("normal(-0.2, 0.1)", class = "b", coef = "slate_age_1")),  # Adjust prior for smooth term
               control = list(adapt_delta = 0.99),
               save_pars = save_pars(all = TRUE))

loo(m0, m1,m2, moment_match=TRUE)

# Sampling from the models' prior
m2_priors<- brm(PU |trunc(lb=2) ~ s(mean_age, k = 14) + (1|site),
                family = poisson(),
                data = d,
                iter = 10000, warmup = 5000,
                prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                          set_prior("normal(-0.2, 0.1)", class = "b", coef = "smean_age_1")),  # Adjust prior for smooth term
                control = list(adapt_delta = 0.99),
                save_pars = save_pars(all = TRUE),
                sample_prior = TRUE)


setwd(paste(here::here(),"/Models",sep="",collapse=""))
# Save an object to a file
saveRDS(m0, file = "m0.rds")
saveRDS(m1, file = "m1.rds")
saveRDS(m2, file = "m2.rds")
saveRDS(m2_early, file = "m2_early.rds")
saveRDS(m2_late, file = "m2_late.rds")
saveRDS(m2_priors, file = "m2_priors.rds")

summary(m2)

setwd(paste(here::here(),"/Models",sep="",collapse=""))
m0<-readRDS(file = "m0.rds")
m1<-readRDS(file = "m1.rds")
m2<-readRDS(file = "m2.rds")
m2_priors<-readRDS(file="m2_priors.rds")

loo(m0, m1,m2, moment_match=TRUE)


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

setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("SIfig4.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig4.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)


# Sample from the model's prior and plot one example of the simulated data
newd_prior <- data.frame(mean_age = seq(3.3, -0.65, by = -0.01))
pred_prior <- posterior_predict(m2_priors, newdata = newd_prior, re_formula = NA, ndraw = 1000)
n <- sample(1:1000, size = 1)  # Pick one of the samples

# Plotting sampled data
p<-ggplot(newd_prior, aes(x = -mean_age, y = pred_prior[n,])) +
  geom_point(shape = 21, fill = NA, color = "black") +
  labs(x = "Log age (std)", y = "PU", title = "Sampling from the Priors") +
  theme_minimal()

setwd(paste(here::here(),"/Figures",sep="",collapse=""))
ggsave("SIfig5.pdf", p, device = "pdf", width = 6, height = 4, units = "in", dpi=1200)
ggsave("SIfig5.tiff", p, device = "tiff", width = 6, height = 4, units = "in", dpi=600)



setwd(here::here())
source("Generate main figure script.R")
