###############################
#                             #
#      Survival Analysis:     #
#     Censoring, Survival,    #
#          & Hazards          #
#                             #
#        Dr Aric LaBarr       #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("survival")
install.packages("survminer")

library(survival)
library(survminer)

# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
setwd("~/Documents/NC State/Fall 3/Survival Analysis/SA Data")

recid <- read.csv(file = "recid.csv", header = TRUE)

customer <- c("A", "B", "C", "D", "E", "F")
tenure <- c( 7, 8, 10, 3, 2, 3)
censored <- c(0, 0, 1, 0, 0, 1)

simple <- data.frame(customer, tenure, censored)

# Survival Function - Simple Data Set #
Surv(time = simple$tenure, event = simple$censored == 0)

simple_km <- survfit(Surv(time = tenure, event = (censored == 0)) ~ 1, data = simple)
summary(simple_km)
plot(simple_km, main = "Survival Function", xlab = "Tenure", ylab = "Survival Probability")

# Survival Function - Recidivism Data Set #
recid_surv <- Surv(time = recid$week, event = recid$arrest == 1)

recid_km <- survfit(recid_surv ~ 1, data = recid)
summary(recid_km)
plot(recid_km, main = "Survival Function", xlab = "Week", ylab = "Survival Probability")

ggsurvplot(recid_km, data = recid, conf.int = TRUE, palette = "purple",
           xlab = "Week", ylab = "Survival Probability", legend = "none",
           break.y.by = 0.1)
table(recid$arrest)
# Stratified Analysis #
survdiff(recid_surv ~ wexp, rho = 0, data = recid)

recid_strat <- survfit(recid_surv ~ wexp, data = recid)
summary(recid_strat)
ggsurvplot(recid_strat, data = recid, conf.int = TRUE, palette = c("purple", "black"),
           xlab = "Week", ylab = "Survival Probability", break.y.by = 0.1,
           legend.title = "work experience", legend.labs = c("no", "yes"))

pairwise_survdiff(recid_surv ~ wexp, rho = 0, data = recid)

# Hazard Function - Simple Data Set #
summary(simple_km)
simple_km$hp <- simple_km$n.event/simple_km$n.risk
print(simple_km$hp)

simple_haz <- merge(data.frame(time = seq(1,10,1)), data.frame(time = simple_km$time, hp = simple_km$hp), by = "time", all = TRUE)
simple_haz[is.na(simple_haz) == TRUE] <- 0
print(simple_haz)

plot(y = simple_haz$hp, x = simple_haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

ggsurvplot(simple_km, data = simple, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "Week", ylab = "Cumulative Hazard", legend = "none")

# Hazard Function - Recidivism Data Set #
recid_km$hp <- recid_km$n.event/recid_km$n.risk
recid_haz <- merge(data.frame(time = seq(1,52,1)), data.frame(time = recid_km$time, hp = recid_km$hp), by = "time", all = TRUE)
recid_haz[is.na(recid_haz) == TRUE] <- 0

plot(y = recid_haz$hp, x = recid_haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

ggsurvplot(recid_km, data = recid, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "Week", ylab = "Cumulative Hazard", legend = "none")
