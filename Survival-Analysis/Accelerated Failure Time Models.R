###############################
#                             #
#      Survival Analysis:     #
#  Accelerated Failure Time   #
#                             #
#        Dr Aric LaBarr       #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("survival")
install.packages("survminer")
install.packages("flexsurv")

library(survival)
library(survminer)
library(flexsurv)

# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
setwd("...")

recid <- read.csv(file = "recid.csv", header = TRUE)

# Accelerated Failure Time Model #
recid.aft.ln <- survreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = 'lognormal')
summary(recid.aft.ln)

# Interpretation of Parameter Estimates #
recid.aft.ln <- survreg(Surv(week, arrest == 1) ~ fin + age +mar + prio, data = recid, dist = 'lognormal')
summary(recid.aft.ln)

(exp(coef(recid.aft.ln))-1)*100

# Exponential vs. Weibull #
recid.aft.w <- survreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = 'weibull')
summary(recid.aft.w)

# Checking Distributions #
recid.aft.w <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "weibull")

plot(recid.aft.w, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Weibull Distribution")

recid.aft.e <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "exp")

plot(recid.aft.e, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Exponential Distribution")

recid.aft.g <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "gamma")

plot(recid.aft.g, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Gamma Distribution")

recid.aft.ll <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "llogis")

plot(recid.aft.ll, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "Cumulative Hazard", main = "Log-Logistic Distribution")

# Goodness-of-Fit Tests #
# The flexsurvreg() function has more distributions available than in SAS so we can perform more comparisons here. #
like.e <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "exp")$loglik
like.w <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "weibull")$loglik
like.ln <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "lnorm")$loglik
like.g <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "gamma")$loglik
like.ll <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "llogis")$loglik
like.f <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro + prio, data = recid, dist = "genf")$loglik

pval.e.g <- 1 - pchisq((-2*(like.e-like.g)), 2)
pval.w.g <- 1 - pchisq((-2*(like.w-like.g)), 1)
pval.ln.g <- 1 - pchisq((-2*(like.ln-like.g)), 1)
pval.g.f <- 1 - pchisq((-2*(like.g-like.f)), 1)
pval.ll.f <- 1 - pchisq((-2*(like.ll-like.f)), 1)

Tests <- c('Exp vs. Gam', 'Wei vs. Gam', 'LogN vs. Gam', 'Gam vs. F', 'LogL vs. F')
P_values <- c(pval.e.g, pval.w.g, pval.ln.g, pval.g.f, pval.ll.f)
cbind(Tests, P_values)

# Predicted Survival Quantiles #
recid.aft.w <- survreg(Surv(week, arrest == 1) ~ fin + age +prio, data = recid, dist = 'weibull')
summary(recid.aft.w)

survprob.75.50.25 <- predict(recid.aft.w, type = "quantile", se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
head(survprob.75.50.25$fit)

# Predicted Mean Event Time #
p.time.mean <- predict(recid.aft.w, type = "response", se.fit = TRUE)
head(p.time.mean$fit, n = 10)

# Predicted Survival Probabilities #
survprob.actual <- 1 - psurvreg(recid$week,
                                mean = predict(recid.aft.w, type = "lp"),
                                scale = recid.aft.w$scale,
                                distribution = recid.aft.w$dist)
head(survprob.actual, n = 10)

survprob.10wk <- 1 - psurvreg(10,
                              mean = predict(recid.aft.w, type = "lp"),
                              scale = recid.aft.w$scale,
                              distribution = recid.aft.w$dist)
head(survprob.10wk)

# Predicted Change in Event Time #
new_time <-  qsurvreg(1 - survprob.actual,
                      mean = predict(recid.aft.w, type = "lp") + coef(recid.aft.w)['fin'],
                      scale = recid.aft.w$scale,
                      distribution = recid.aft.w$dist)

recid$new_time <- new_time
recid$diff <- recid$new_time - recid$week

head(data.frame(recid$week, recid$new_time, recid$diff), n = 10)
