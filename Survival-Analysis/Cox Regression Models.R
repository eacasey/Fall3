###############################
#                             #
#      Survival Analysis:     #
#     Cox Regression Models   #
#                             #
#        Dr Aric LaBarr       #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("survival")
install.packages("survminer")
install.packages("visreg")
install.packages("ggplot2")

library(survival)
library(MASS)
library(survminer)
library(visreg)
library(ggplot2)

# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
setwd("...")

recid <- read.csv(file = "recid.csv", header = TRUE)
recid_long <- read.csv(file = "recid_long.csv", header = TRUE)
recid_lag <- read.csv(file = "recid_lag.csv", header = TRUE)

# Proportional Hazards Model #
recid.ph <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar +paro + prio, data = recid)
summary(recid.ph)

# Parameter Interpretation #
recid.ph2 <- coxph(Surv(week, arrest == 1) ~ fin + age + prio, data = recid)
summary(recid.ph2)

(exp(coef(recid.ph2))-1)*100

# Linearity #
visreg(recid.ph, "age", xlab = "age", ylab = "partial residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red", level = 0.985) + theme_bw()

visreg(recid.ph, "prio", xlab = "#prior convictions", ylab = "partial residuals",
       gg = TRUE, band = FALSE) +
  geom_smooth(col = "red", fill = "red", level = 0.985) + theme_bw()

# Proportional Hazard Test - Schoenfeld Residuals #
recid.ph.zph <- cox.zph(recid.ph, transform = "identity")
recid.ph.zph

recid.ph.zph <- cox.zph(recid.ph, transform = "log")
recid.ph.zph

# Automatic Selection Techniques #
full.model <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar +paro + prio, 
                    data = recid)

empty.model <- coxph(Surv(week, arrest == 1) ~ 1, data = recid)

step.model <- step(empty.model, 
                      scope = list(lower=formula(empty.model), 
                                   upper=formula(full.model)), 
                      direction = "both")
summary(step.model)

full.model <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar +paro + prio, 
                    data = recid)

back.model <- step(full.model, direction = "backward")
summary(back.model)

full.model <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar +paro + prio, 
                    data = recid)

empty.model <- coxph(Surv(week, arrest == 1) ~ 1, data = recid)

for.model <- step(empty.model, 
                  scope = list(lower=formula(empty.model), 
                               upper=formula(full.model)), 
                  direction = "forward")
summary(for.model)

# Estimated Survival Curves #
newdata <- data.frame(fin = c(1, 0), age = 30, race = 0, wexp = c(1, 0),
                      mar = 0, paro = 0, prio = c(0, 4))

ggsurvplot(survfit(recid.ph, newdata), data = newdata, break.y.by = 0.1,
           palette = c("purple", "black"), ylab = "survival probability",
           xlab = "week", legend.labs = c("1", "2"), legend.title = "subject")

# Concordance #
recid.ph <- coxph(Surv(week, arrest == 1) ~ fin + age + prio, data = recid)
concordance(recid.ph)

# Time-Dependent Coefficients #
recid.ph.tdc <- coxph(Surv(week, arrest == 1) ~ fin + race + wexp + mar + paro + age + tt(age), data = recid,
                 tt = function(x, time, ...){x*log(time)})
summary(recid.ph.tdc)

# Time Varying Variables #
recid_long.ph <- coxph(Surv(start, stop, arrested == 1) ~ fin + age + race + wexp + mar + paro + prio + employed, data = recid_long)
summary(recid_long.ph)

recid_lag.ph <- coxph(Surv(start, stop, arrested == 1) ~ fin + age + race + wexp + mar + paro + prio + employed, data = recid_lag)
summary(recid_lag.ph)

