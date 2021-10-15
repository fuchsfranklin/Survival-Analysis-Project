library(ggplot2)
library(ggthemes)
library(survival)
library(survminer)

###################################################################
# Normal PDF, CDF, and Survival Fct
###################################################################

# Normal Probability Density Function

normal_pdf <- function(x) {
  dnorm(x, mean=0, sd=1)
}


p9 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = normal_pdf) +
  theme_tufte() +
  ggtitle("Normal PDF") +
  xlab("x") + 
  ylab("density")


p9


# Normal cdf

normal_cdf <- function(x) {
  pnorm(x, mean=1, sd=1)
}

p10 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = normal_cdf) +
  theme_tufte() +
  ggtitle("Normal CDF") +
  xlab("x") + 
  ylab("density")

p10

# Normal Survival Fct
normal_survival <- function(x) {
  1 - pnorm(x, mean=1, sd=1)
}

p11 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = normal_survival ) +
  theme_tufte() +
  ggtitle("Normal Survival Function") +
  xlab("x") + 
  ylab("density")

p11



###################################################################
# Exponential PDF, CDF, and Survival Fct
###################################################################

# Exponential pdf
p12 <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
  stat_function(fun = dexp) +
  theme_tufte() +
  ggtitle("Exponential PDF") +
  xlab("x") + 
  ylab("density")

p12

# Exponential cdf
p13 <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
  stat_function(fun = pexp) +
  theme_tufte() +
  ggtitle("Exponential CDF") +
  xlab("x") + 
  ylab("density")

p13

# Exponential Survival Fct
exp_survival <- function(x) {
  1 - pexp(x)
}

p14 <- ggplot(data.frame(x = c(0, 5)), aes(x = x)) +
  stat_function(fun = exp_survival  ) +
  theme_tufte() +
  ggtitle("Exponential Survival Function") +
  xlab("x") + 
  ylab("density")

p14




###################################################################
# Unif PDF, CDF, and Survival Fct
###################################################################

# Unif pdf
p15 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  stat_function(fun = dunif) +
  theme_tufte() +
  ggtitle("Uniform PDF") +
  xlab("x") + 
  ylab("density")

p15

# Unif cdf
p16 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  stat_function(fun = punif) +
  theme_tufte() +
  ggtitle("Uniform CDF") +
  xlab("x") + 
  ylab("density")

p16

# Unif Survival Fct
unif_survival <- function(x) {
  1 - punif(x) +
    ggtitle("Uniform Survival Function") +
    xlab("x") + 
    ylab("density")
}

p17 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  stat_function(fun = unif_survival ) +
  theme_tufte() +
  ggtitle("Exponential PDF") +
  xlab("x") + 
  ylab("density")

p17


# head(kidney)
# 
# time_in_years <- c(0:24)
# 
# leukemia.surv <- survfit(Surv(time, status) ~ x, data = aml)
# plot(leukemia.surv, lty = 2:3)
# legend(100, .9, c("Maintenance", "No Maintenance"), lty = 2:3)
# title("Kaplan-Meier Curves\nfor AML Maintenance Study")
# lsurv2 <- survfit(Surv(time, status) ~ x, aml)
# ggsurvplot(lsurv2, data = aml)


# Boston University Kaplan-Meier Example

time <- c(0,1,2,3,5,6,9,10,11,12,13,14,17,18,19,21,23,24)
N_t <- c(20,20,19,18,17,16,15,14,13,12,11,10,9,7,6,5,4,3)
D_t <- c(0,1,0,1,1,0,0,0,0,0,0,1,1,0,0,0,1,0)
C_t <- c(0,0,1,0,0,1,1,1,1,1,1,0,1,1,1,1,0,3)
S_t <- c(1,0.95,0.95,0.897,0.844,0.844,0.844,0.844,0.844,0.844,0.844,0.760,0.676,0.676,0.676,0.676,0.507,0.507)

df <- data.frame(time, N_t, D_t, C_t, S_t)

names(df) <- c("Time in Years", "Number at Risk Nt", "Number of Deaths Dt", "Number Censored Ct", "Survival Probility St")


