# Load necessary libraries
library(here)
library(brms)
library(splines)
library(ggplot2)
library(gridExtra)

# Load and prepare dataset
setwd(here::here())
source("Prepare data script.R")


# Check variance in outcome variable PU for the last 100,000 years
subset_d <- d[d$original_age < 100, ]
cat("Mean:", mean(subset_d$PU), "\n")
cat("Variance:", var(subset_d$PU), "\n")

# Simulate data from Poisson Lambda priors
n <- 1000
intercepts <- rnorm(n, mean = 2, sd = 0.5) # Sample intercepts (normal distribution)
lambdas <- exp(intercepts)                 # Generate rates (lambda)
data <- sapply(lambdas, rpois, n = 1)      # Generate Poisson data

# Plot histogram of simulated Poisson data
ggplot(data = data.frame(Value = data), aes(x = Value)) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  labs(title = 'Histogram of Simulated Poisson Data', x = 'PU', y = 'Frequency')

####
#### Model generation
####

# Intercept only
m0<- brm(PU ~ 1,
         family = poisson(),
         data = d,
         iter = 10000, warmup = 5000,
         prior = c(set_prior("normal(2, 0.5)", class = "Intercept")),
         save_pars = save_pars(all = TRUE)) # Save all parameters of the model

# A simple linear model
m1<- brm(PU ~ age + (1|site),
         family = poisson(),
         data = d,
         iter = 10000, warmup = 5000,
         prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                   set_prior("normal(-0.2, 0.1)", class = "b")),
         save_pars = save_pars(all = TRUE)) 

# With B-splines, degrees of freedom = 3, degree = 4
m2.1<- brm(PU ~ bs(age, df=3, degree = 3) + (1|site),
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     set_prior("normal(-0.2, 0.1)", class = "b")),
           save_pars = save_pars(all = TRUE))

# With B-splines, degrees of freedom = 4, degree = 4
m2.2<- brm(PU ~ bs(age, df=4, degree = 3) + (1|site), 
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     bspline_prior <- set_prior("normal(-0.2, 0.1)", class = "b", dpar = "mu")),
           save_pars = save_pars(all = TRUE))

# With B-splines, degrees of freedom = 5, degree = 4
m2.3<- brm(PU ~ bs(age, df=5, degree = 3) + (1|site), 
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     bspline_prior <- set_prior("normal(-0.2, 0.1)", class = "b", dpar = "mu")),
           save_pars = save_pars(all = TRUE)) 

# With B-splines, degrees of freedom = 6, degree = 4
m2.4<- brm(PU ~ bs(age, df=6, degree = 3) + (1|site),
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     bspline_prior <- set_prior("normal(-0.2, 0.1)", class = "b", dpar = "mu")),
           save_pars = save_pars(all = TRUE)) 

# With truncation
m3.1<- brm(PU |trunc(lb=2) ~ age + (1|site),  
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     set_prior("normal(-0.2, 0.1)", class = "b")),
           control = list(adapt_delta = 0.9),  # Increase adapt_delta to avoid divergences
           save_pars = save_pars(all = TRUE))  # Increase adapt_delta to avoid divergences) 

# With truncation + B-splines, degrees of freedom =4, degree = 3
m3.2<- brm(PU |trunc(lb=2) ~ bs(age, df=3, degree = 3) + (1|site),
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     bspline_prior <- set_prior("normal(-0.2, 0.1)", class = "b", dpar = "mu")),
           save_pars = save_pars(all = TRUE))

# With truncation + B-splines, degrees of freedom =4, degree = 3
m3.3<- brm(PU |trunc(lb=2) ~ bs(age, df=4, degree = 3) + (1|site),
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     bspline_prior <- set_prior("normal(-0.2, 0.1)", class = "b", dpar = "mu")),
           save_pars = save_pars(all = TRUE))

# With truncation + B-splines, degrees of freedom =5, degree = 3
m3.4<- brm(PU |trunc(lb=2) ~ bs(age, df=5, degree = 3) + (1|site),
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     bspline_prior <- set_prior("normal(-0.2, 0.1)", class = "b", dpar = "mu")),
           save_pars = save_pars(all = TRUE))

# With truncation + B-splines, degrees of freedom =6, degree = 3
m3.5<- brm(PU |trunc(lb=2) ~ bs(age, df=6, degree = 3) + (1|site),
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     bspline_prior <- set_prior("normal(-0.2, 0.1)", class = "b", dpar = "mu")),
           save_pars = save_pars(all = TRUE))

m3.6<- brm(PU |trunc(lb=2) ~ bs(age, df=7, degree = 3) + (1|site),
           family = poisson(),
           data = d,
           iter = 10000, warmup = 5000,
           prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                     bspline_prior <- set_prior("normal(-0.2, 0.1)", class = "b", dpar = "mu")),
           save_pars = save_pars(all = TRUE)) 

## Save models 
setwd(paste(here::here(),"/Models",sep="",collapse=""))
saveRDS(m0,"m0.Rds")
saveRDS(m1,"m1.Rds")
saveRDS(m2.1,"m2.1.Rds")
saveRDS(m2.2,"m2.2.Rds")
saveRDS(m2.3,"m2.3.Rds")
saveRDS(m2.4,"m2.4.Rds")
saveRDS(m3.1,"m3.1.Rds")
saveRDS(m3.2,"m3.2.Rds")
saveRDS(m3.3,"m3.3.Rds")
saveRDS(m3.4,"m3.4.Rds")
saveRDS(m3.5,"m3.5.Rds")
saveRDS(m3.6,"m3.6.Rds")


## Read in models
setwd(paste(here::here(),"/Models",sep="",collapse=""))
m0<-readRDS("m0.Rds")
m1<-readRDS("m1.Rds")
m2.1<-readRDS("m2.1.Rds")
m2.2<-readRDS("m2.2.Rds")
m2.3<-readRDS("m2.3.Rds")
m2.4<-readRDS("m2.4.Rds")
m3.1<-readRDS("m3.1.Rds")
m3.2<-readRDS("m3.2.Rds")
m3.3<-readRDS("m3.3.Rds")
m3.4<-readRDS("m3.4.Rds")
m3.5<-readRDS("m3.5.Rds")
m3.6<-readRDS("m3.6.Rds")

# LOO comparison
loo(m0, m1, m2.1, m2.2, m2.3, m2.4, m3.1, m3.2, m3.3, m3.4, m3.5, m3.6, moment_match = TRUE)


###
### Model 3.1 summary and diagnostics. 
### 


# Summary and diagnostics for a selected model
summary(m3.1)
plot(m3.1)
pp_check(m3.1)


# Sample from the priors to simulate regression lines
n_samples <- 100
predictor_range <- seq(from = -2, to = 2, length.out = 100)
intercepts <- rnorm(n_samples, mean = 2, sd = 0.5)  # Sample intercepts
betas <- rnorm(n_samples, mean = -0.2, sd = 0.1)    # Sample beta coefficients

# Prepare data for plotting
plot_data <- expand.grid(mean_age = predictor_range, sample = 1:n_samples)
plot_data$lambda <- exp(intercepts[plot_data$sample] + betas[plot_data$sample] * plot_data$mean_age)

# Plot regression lines
ggplot(plot_data, aes(x = mean_age, y = lambda, group = sample, color = as.factor(sample))) +
  geom_line(show.legend = FALSE) +
  theme_minimal() +
  labs(title = 'Sampled Regression Lines from Priors', x = 'Age', y = 'PU')

# Sample from the model's prior
m3.1_priors <- brm(PU | trunc(lb = 2) ~ age + (1|site),
                   family = poisson(),
                   data = d,
                   prior = c(set_prior("normal(2, 0.5)", class = "Intercept"),
                             set_prior("normal(-0.2, 0.1)", class = "b")),
                   save_pars = save_pars(all = TRUE),
                   sample_prior = "yes")

#Save this model
setwd(paste(here::here(),"/Models",sep="",collapse=""))
saveRDS(m3.1_priors,"m3.1_priors.Rds")
m3.1_priors <- readRDS("m3.1_priors.Rds")

# Sample from the model's prior and plot one example of the simulated data
newd_prior <- data.frame(age = seq(-2, 1.55, by = 0.01))
pred_prior <- posterior_predict(m3.1_priors, newdata = newd_prior, re_formula = NA, ndraw = 1000)
n <- sample(1:1000, size = 1)  # Pick one of the samples

# Plotting sampled data
ggplot(newd_prior, aes(x = age, y = pred_prior[n,])) +
  geom_point(shape = 21, fill = NA, color = "black") +
  labs(x = "Log age (std)", y = "PU", title = "Sampling from the Priors") +
  theme_minimal()

###
### Generate main figures
###

setwd(here::here())
source("Generate main figure script.R")

setwd(here::here())
source("Make a map.R")


###
### Other code
###

#Code for double checking sites mentioned in text.
sites.in.text<-d[grep("Lomekwi|Bokol|EG12|Kfar|Sujula|GhGk|adze|Kathu",d$site),]
sites.in.text[,-grep("PU|Species.attribution|Descr.", names(sites.in.text))]
sites.in.text[,-grep("PU|Species.attribution|Lat|Long|Top|Age|Total", names(sites.in.text))]
sites.in.text

