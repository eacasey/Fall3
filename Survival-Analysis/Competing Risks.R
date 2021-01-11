###############################
#                             #
#      Survival Analysis:     #
#       Competing Risks       #
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
setwd("...")

leaders <- read.csv(file = "leaders.csv", header = TRUE)

leaders$lost <- factor(leaders$lost, 0:3, labels = c("In Office", "Constitutional", "Natural", "Non-Constitutional"))

# Cumulative Incidence Functions #
lcon_data <- finegray(Surv(years, lost) ~ .,
                      data = leaders, etype = "Constitutional")
lnat_data <- finegray(Surv(years, lost) ~ .,
                      data = leaders, etype = "Natural")
lnon_data <- finegray(Surv(years, lost) ~ .,
                      data = leaders, etype = "Non-Constitutional")

lcon <- survfit(Surv(fgstart, fgstop, fgstatus) ~ 1,
                data = lcon_data, weight = fgwt)
lnat <- survfit(Surv(fgstart, fgstop, fgstatus) ~ 1,
                data = lnat_data, weight = fgwt)
lnon <- survfit(Surv(fgstart, fgstop, fgstatus) ~ 1,
                data = lnon_data, weight = fgwt)

leadlist <- list(constitutional = lcon, natural = lnat, nonconstitutional = lnon)

ggsurvplot(leadlist, combine = TRUE, fun = "event", conf.int = FALSE,
           break.x.by = 4, ylim = c(0, 0.5), legend.title = "Status:",
           legend.labs = c("Constitutional", "Natural", "Non-Constitutional"), break.y.by = 0.1, xlab = "Years",
           ylab = "Cumulative Incidence", palette = c("black", "orange", "red"))

# Cox Regression Competing Risks #
cox_const <- coxph(Surv(years, lost == "Constitutional") ~ manner + start + military + age + conflict + loginc + growth + pop + land + literacy + factor(region), 
                   data = leaders)
summary(cox_const)

cox_nat <- coxph(Surv(years, lost == "Natural") ~ manner + start + military + age + conflict + loginc + growth + pop + land + literacy + factor(region), 
                   data = leaders)
summary(cox_nat)

cox_noncon <- coxph(Surv(years, lost == "Non-Constitutional") ~ manner + start + military + age + conflict + loginc + growth + pop + land + literacy + factor(region), 
                   data = leaders)
summary(cox_noncon)

# OPTIONAL: Fine-Gray Model #
lcon_data <- finegray(Surv(years, lost) ~ .,
                      data = leaders, etype = "Constitutional")

fg_con <- coxph(Surv(fgstart, fgstop, fgstatus) ~ manner + start + military + age + conflict + loginc + growth + pop + land + literacy + factor(region),
                data = lcon_data, weight = fgwt)
summary(fg_con)

lnat_data <- finegray(Surv(years, lost) ~ .,
                      data = leaders, etype = "Natural")

fg_nat <- coxph(Surv(fgstart, fgstop, fgstatus) ~ manner + start + military + age + conflict + loginc + growth + pop + land + literacy + factor(region),
                 data = lnat_data, weight = fgwt)
summary(fg_nat)

lnon_data <- finegray(Surv(years, lost) ~ .,
                      data = leaders, etype = "Non-Constitutional")

fg_non <- coxph(Surv(fgstart, fgstop, fgstatus) ~ manner + start + military + age + conflict + loginc + growth + pop + land + literacy + factor(region),
                data = lnon_data, weight = fgwt)
summary(fg_non)
