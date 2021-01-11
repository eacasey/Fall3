###############################
#                             #
#      Survival Analysis:     #
#       Repeated Events       #
#                             #
#        Dr Aric LaBarr       #
#                             #
###############################

# Needed Libraries for Analysis #
install.packages("survival")

library(survival)

# Load Needed Data Sets #
# Replace the ... below with the file location of the data sets #
setwd("...")

bladder <- read.csv(file = "bladder.csv", header = TRUE)

# Independence Model - Time-Dependent Variables #
bladder.td <- coxph(Surv(start, stop, event == 1) ~ rx + number + size + enum, data = bladder)
summary(bladder.td)

# Independence Model - Robust Standard Errors #
bladder.rse <- coxph(Surv(start, stop, event == 1) ~ rx + number + size + enum + cluster(id), data = bladder)
summary(bladder.rse)

# Conditional Model - Assuming Variable Effects Same Across Strata #
bladder.con <- coxph(Surv(start, stop, event == 1) ~ rx + number + size + strata(enum), data = bladder)
summary(bladder.con)

# Conditional Model - Assuming Variable Effects Different Across Strata #
bladder.con2 <- coxph(Surv(start, stop, event == 1) ~ rx:strata(enum) + number:strata(enum) + size:strata(enum), data = bladder)
summary(bladder.con2)

# Gap Time Model - Assuming Variable Effects Same Across Strata #
# Can easily extend the gap time model to assume variable effects different same as above. #
bladder.gap <- coxph(Surv(time = (stop - start), event == 1) ~ rx + number + size + strata(enum), data = bladder)
summary(bladder.gap)
