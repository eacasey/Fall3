##################################################
##############Survival Analysis HW 1
##################################################

#Oct. 30, 2020
#Ellie Code

############################
############## Loading in Data & Libraries
############################
library(dplyr)
library(sas7bdat)
library(haven)
library(broom)
library(survival)
library(survminer)
library(viridis)
library(wesanderson)
library(colorspace)
library(unikn)
library(tidyr)

#Adding fonts
library(extrafont)
library(grDevices)
font_import()
loadfonts(device = "win")

#target variable = hour, survive

setwd("~/Documents/NC State/Fall 3/Survival Analysis")

hurricane <- read.sas7bdat("hurricane.sas7bdat")
table(hurricane$survive)

############################
############## Graphing (A) - Basic
############################
# Survival probability across time for all pumps together – not broken down by failure type.
# Survival Function - Hurricane Data #
Surv(time = hurricane$hour, event = hurricane$survive == 0)
recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0) #plots correctly for some reason if =0, not 1

recid_km <- survfit(recid_surv ~ 1, data = hurricane)
summary(recid_km)
plot(recid_km, main = "Survival Function", xlab = "Hour", ylab = "Survival Probability")

recid_km1 <- tidy(recid_km)
ggsurvplot(recid_km, data = hurricane, conf.int = TRUE, palette = "uniform",
           xlab = "Hour", ylab = "Survival Probability", legend = "none",
           break.y.by = 0.1)

ggsurvplot(recid_km, data = hurricane, conf.int = TRUE, palette = "royalblue",
           xlab = "Hour", ylab = "Survival Probability", legend = "none",
           break.y.by = 0.1) + theme(text = element_text(size=15, family="Sathu"))

write.csv(recid_km1, "plotadata.csv")
############## 

simple_km <- survfit(Surv(time = hour, event = (survive == 0)) ~ 1, data = hurricane)
summary(simple_km)
plot(simple_km, main = "Survival Function", xlab = "Hour", ylab = "Survival Probability")

#recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 1)

############################
############## Graphing (B) - Stratified
############################
# Stratified Analysis #
# Survival probability across time for pumps broken down by failure type overlaid into one graph.
recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)
survdiff(recid_surv ~ survive, rho = 0, data = hurricane)

recid_strat <- survfit(recid_surv ~ reason, data = hurricane)
summary(recid_strat)

recid_strat1<- tidy(recid_strat)

recid_strat1 <- recid_strat1 %>%
  filter(strata!="reason=0") %>%
  mutate('Failure_Reason'= ifelse((strata=="reason=1"), "Flood Failure",
                                  ifelse((strata=="reason=2"), "Motor Failure",
                                         ifelse((strata=="reason=3"), "Surge Failure",
                                                ifelse((strata=="reason=4"), "Jammed Failure", "Banana"))))) %>%
  select(-strata)


recid_strat2 <- recid_strat1 %>%
  spread(Failure_Reason, estimate)

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)

ggsurvplot(recid_strat, data = hurricane, conf.int = TRUE, palette = c("purple", "orange", "yellow", "blue", "red"),
           xlab = "Hour", ylab = "Survival Probability", break.y.by = 0.1,
           legend.title = "Failure Reason", legend.labs = c(0,1,2,3,4))

seecol("unikn_all", n=5)

ggplot(recid_strat1, aes(x=time, y=estimate, fill=Failure_Reason)) + geom_line(aes(colour=Failure_Reason), size=1.1) + 
  labs(x="Hour", y = "Survival Probability", colour="Failure Reason") + theme_classic() + scale_fill_manual(values  = usecol(pal_unikn_pref, n=5)) + 
  scale_color_manual(values = usecol(pal_unikn_pref, n=5)) + theme(text = element_text(size=15, family="Sathu"))

pairwise_survdiff(recid_surv ~ survive, rho = 0, data = hurricane)

write.csv(recid_strat1,"plotbdata.csv")

############################
############## Graphing (C) - Conditional Hazard
############################
#look into making more tickmarks 
# Conditional failure probabilities across time for all pumps together – not broken down by failure type.
recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0) #plots correctly for some reason if =0, not 1
recid_km <- survfit(recid_surv ~ 1, data = hurricane)
#Hazard Function
recid_km$hp <- recid_km$n.event/recid_km$n.risk
recid_haz <- merge(data.frame(time = seq(1,48,1)), data.frame(time = recid_km$time, hp = recid_km$hp), by = "time", all = TRUE)
recid_haz[is.na(recid_haz) == TRUE] <- 0

plot(y = recid_haz$hp, x = recid_haz$time, main = "Hurricane Hazard Probability Function", xlab = "Time", ylab = "Hazard Probability",
     type = 'l') #There's a big spikes at time=25ish & 45ish, wonder what's going on here


ggplot(recid_haz, aes(x=time, y=hp)) + geom_line(color="skyblue4",size=1.1) + 
  labs(x="Hour", y = "Hazard Probability") + theme_classic() + scale_fill_manual(values  = usecol(pal_unikn_pref, n=5)) + 
  scale_color_manual(values = usecol(pal_unikn_pref, n=5)) + theme(text = element_text(size=15, family="Sathu"))


write.csv(recid_haz, "plotcdata.csv")

############################
############## Graphing (D) - Conditional Hazard Stratified
############################

#Don't know if stratified correctly
# Conditional failure probabilities across time for pumps broken down by failure type overlaid into one graph.
recid_surv <- Surv(time = hurricane$hour, event = hurricane$survive == 0)
survdiff(recid_surv ~ survive, rho = 0, data = hurricane)
recid_strat <- survfit(recid_surv ~ reason, data = hurricane)

#Hazard Function
recid_strat<- tidy(recid_strat)
recid_strat$hp <- recid_strat$n.event/recid_strat$n.risk
recid_haz <- merge(data.frame(time = seq(1,52,1)), data.frame(time = recid_strat$time, hp = recid_strat$hp, reason=recid_strat$n.censor), by = "time", all = TRUE)
recid_strat[is.na(recid_strat) == TRUE] <- 0

plot(y = recid_haz$hp, x = recid_haz$time, by = recid_strat$strata, main = "Hurricane Hazard Probability Function", xlab = "Time", ylab = "Hazard Probability",
     type = 'l')

ggplot(data=recid_strat, aes(x=time, y=hp, group=strata, color=strata)) + geom_line() + 
  labs(x="Hour", y= " Hazard Probability", main="Hurricane Hazard Probability Function") + 
  theme_light()

recid_strat1.1 <- recid_strat %>%
  filter(strata!="reason=0") %>%
  mutate('Failure_Reason'= ifelse((strata=="reason=1"), "Flood Failure",
                                  ifelse((strata=="reason=2"), "Motor Failure",
                                         ifelse((strata=="reason=3"), "Surge Failure",
                                                ifelse((strata=="reason=4"), "Jammed Failure", "Banana"))))) %>%
  select(-strata)

ggplot(data=recid_strat1.1, aes(x=time, y=hp, group=Failure_Reason, color=Failure_Reason)) + geom_line(size=1.1) + 
  labs(x="Hour", y= " Hazard Probability", main="Hurricane Hazard Probability Function", colour = "Failure Reason") + 
  theme_classic() + scale_fill_manual(values  = usecol(pal_unikn_pref, n=5)) + 
  scale_color_manual(values = usecol(pal_unikn_pref, n=5)) + theme(text = element_text(size=15, family="Tahoma"))

write.csv(recid_strat1.1, "plotddata.csv")

####################################################################################################
#Instructions:
#Provide the following graphs as well as any meaningful insights from the graphs that you see:
  # Survival probability across time for all pumps together – not broken down by failure type.
  # Survival probability across time for pumps broken down by failure type overlaid into one graph.
  # Conditional failure probabilities across time for all pumps together – not broken down by failure type.
  # Conditional failure probabilities across time for pumps broken down by failure type overlaid into one graph.
  # (HINT: Some of these can be put into the appendix. Only include the ones in the main report you feel provide the meaningful insights.)


####################################################################################################
#Extra Code



ggplot(recid_strat1, aes(x=time, y=estimate, fill=Failure_Reason)) + geom_line(aes(colour=Failure_Reason)) + 
  labs(x="Hour", y = "Survival Probability", colour="Failure Reason") + theme_classic() + scale_fill_viridis(discrete=TRUE) + 
  scale_color_viridis(discrete=T, option="D")

pal <- wes_palette("Royal2", 5, type="discrete")
ggplot(recid_strat1, aes(x=time, y=estimate, fill=Failure_Reason)) + geom_line(aes(colour=Failure_Reason)) + 
  labs(x="Hour", y = "Survival Probability", colour="Failure Reason") + theme_classic() + scale_fill_manual(values = pal) + scale_color_manual(values = pal)




####################################################################################################
#Fonts

/System/Library/Fonts/Supplemental/Arial Unicode.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Unicode
/System/Library/Fonts/Apple Braille Outline 6 Dot.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Apple Braille Outline 6 Dot
/System/Library/Fonts/Apple Braille Outline 8 Dot.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Apple Braille Outline 8 Dot
/System/Library/Fonts/Apple Braille Pinpoint 6 Dot.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Apple Braille Pinpoint 6 Dot
/System/Library/Fonts/Apple Braille Pinpoint 8 Dot.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Apple Braille Pinpoint 8 Dot
/System/Library/Fonts/Apple Braille.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Apple Braille
/System/Library/Fonts/Apple Symbols.ttf : No FontName. Skipping.
/System/Library/Fonts/Keyboard.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Keyboard
/System/Library/Fonts/NewYork.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NewYork
/System/Library/Fonts/NewYorkItalic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NewYorkItalic
/System/Library/Fonts/SFCompactDisplay.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFCompactDisplay
/System/Library/Fonts/SFCompactRounded.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFCompactRounded
/System/Library/Fonts/SFCompactText.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFCompactText
/System/Library/Fonts/SFCompactTextItalic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFCompactTextItalic
/System/Library/Fonts/SFNS.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFNS
/System/Library/Fonts/SFNSItalic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFNSItalic
/System/Library/Fonts/SFNSMono.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFNSMono
/System/Library/Fonts/SFNSMonoItalic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFNSMonoItalic
/System/Library/Fonts/SFNSRounded.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/SFNSRounded
/System/Library/Fonts/Supplemental/Andale Mono.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Andale Mono
/System/Library/Fonts/Supplemental/Apple Chancery.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/AppleGothic.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/AppleMyungjo.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/AppleMyungjo
/System/Library/Fonts/Supplemental/Arial Black.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Black
/System/Library/Fonts/Supplemental/Arial Bold Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Bold Italic
/System/Library/Fonts/Supplemental/Arial Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Bold
/System/Library/Fonts/Supplemental/Arial Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Italic
/System/Library/Fonts/Supplemental/Arial Narrow Bold Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Narrow Bold Italic
/System/Library/Fonts/Supplemental/Arial Narrow Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Narrow Bold
/System/Library/Fonts/Supplemental/Arial Narrow Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Narrow Italic
/System/Library/Fonts/Supplemental/Arial Narrow.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Narrow
/System/Library/Fonts/Supplemental/Arial Rounded Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Rounded Bold
/System/Library/Fonts/Supplemental/Arial Unicode.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial Unicode
/System/Library/Fonts/Supplemental/Arial.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Arial
/System/Library/Fonts/Supplemental/Ayuthaya.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/BigCaslon.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Bodoni 72 Smallcaps Book.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Bodoni 72 Smallcaps Book
/System/Library/Fonts/Supplemental/Bodoni Ornaments.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Bodoni Ornaments
/System/Library/Fonts/Supplemental/Bradley Hand Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Bradley Hand Bold
/System/Library/Fonts/Supplemental/Brush Script.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Brush Script
/System/Library/Fonts/Supplemental/Chalkduster.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Comic Sans MS Bold.ttfsh: line 1: 30011 Abort trap: 6           '/Library/Frameworks/R.framework/Versions/4.0/Resources/library/Rttf2pt1/exec//ttf2pt1' -a -GfAe '/System/Library/Fonts/Supplemental/Chalkduster.ttf' '/var/folders/pm/vvqvdfy151ngq5zqzzj5kpg00000gn/T//RtmpjQOLSy/fonts/Chalkduster' 2>&1
=> /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Comic Sans MS Bold
/System/Library/Fonts/Supplemental/Comic Sans MS.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Comic Sans MS
/System/Library/Fonts/Supplemental/Courier New Bold Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Courier New Bold Italic
/System/Library/Fonts/Supplemental/Courier New Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Courier New Bold
/System/Library/Fonts/Supplemental/Courier New Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Courier New Italic
/System/Library/Fonts/Supplemental/Courier New.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Courier New
/System/Library/Fonts/Supplemental/DIN Alternate Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/DIN Alternate Bold
/System/Library/Fonts/Supplemental/DIN Condensed Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/DIN Condensed Bold
/System/Library/Fonts/Supplemental/Diwan Thuluth.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Farisi.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Georgia Bold Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Georgia Bold Italic
/System/Library/Fonts/Supplemental/Georgia Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Georgia Bold
/System/Library/Fonts/Supplemental/Georgia Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Georgia Italic
/System/Library/Fonts/Supplemental/Georgia.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Georgia
/System/Library/Fonts/Supplemental/Gurmukhi.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Herculanum.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Hoefler Text Ornaments.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Impact.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Impact
/System/Library/Fonts/Supplemental/Khmer Sangam MN.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Khmer Sangam MN
/System/Library/Fonts/Supplemental/Kokonor.ttfsh: line 1: 30052 Abort trap: 6           '/Library/Frameworks/R.framework/Versions/4.0/Resources/library/Rttf2pt1/exec//ttf2pt1' -a -GfAe '/System/Library/Fonts/Supplemental/Kokonor.ttf' '/var/folders/pm/vvqvdfy151ngq5zqzzj5kpg00000gn/T//RtmpjQOLSy/fonts/Kokonor' 2>&1
: No FontName. Skipping.
/System/Library/Fonts/Supplemental/Krungthep.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Lao Sangam MN.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Lao Sangam MN
/System/Library/Fonts/Supplemental/Luminari.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Luminari
/System/Library/Fonts/Supplemental/Microsoft Sans Serif.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Microsoft Sans Serif
/System/Library/Fonts/Supplemental/Mishafi Gold.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Mishafi.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/NISC18030.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/NotoSansAvestan-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansAvestan-Regular
/System/Library/Fonts/Supplemental/NotoSansBamum-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansBamum-Regular
/System/Library/Fonts/Supplemental/NotoSansBatak-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansBatak-Regular
/System/Library/Fonts/Supplemental/NotoSansBrahmi-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansBrahmi-Regular
/System/Library/Fonts/Supplemental/NotoSansBuginese-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansBuginese-Regular
/System/Library/Fonts/Supplemental/NotoSansBuhid-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansBuhid-Regular
/System/Library/Fonts/Supplemental/NotoSansCarian-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansCarian-Regular
/System/Library/Fonts/Supplemental/NotoSansChakma-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansChakma-Regular
/System/Library/Fonts/Supplemental/NotoSansCham-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansCham-Regular
/System/Library/Fonts/Supplemental/NotoSansCoptic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansCoptic-Regular
/System/Library/Fonts/Supplemental/NotoSansCuneiform-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansCuneiform-Regular
/System/Library/Fonts/Supplemental/NotoSansCypriot-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansCypriot-Regular
/System/Library/Fonts/Supplemental/NotoSansEgyptianHieroglyphs-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansEgyptianHieroglyphs-Regular
/System/Library/Fonts/Supplemental/NotoSansGlagolitic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansGlagolitic-Regular
/System/Library/Fonts/Supplemental/NotoSansGothic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansGothic-Regular
/System/Library/Fonts/Supplemental/NotoSansHanunoo-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansHanunoo-Regular
/System/Library/Fonts/Supplemental/NotoSansImperialAramaic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansImperialAramaic-Regular
/System/Library/Fonts/Supplemental/NotoSansInscriptionalPahlavi-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansInscriptionalPahlavi-Regular
/System/Library/Fonts/Supplemental/NotoSansInscriptionalParthian-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansInscriptionalParthian-Regular
/System/Library/Fonts/Supplemental/NotoSansKaithi-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansKaithi-Regular
/System/Library/Fonts/Supplemental/NotoSansKayahLi-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansKayahLi-Regular
/System/Library/Fonts/Supplemental/NotoSansKharoshthi-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansKharoshthi-Regular
/System/Library/Fonts/Supplemental/NotoSansLepcha-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansLepcha-Regular
/System/Library/Fonts/Supplemental/NotoSansLimbu-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansLimbu-Regular
/System/Library/Fonts/Supplemental/NotoSansLinearB-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansLinearB-Regular
/System/Library/Fonts/Supplemental/NotoSansLisu-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansLisu-Regular
/System/Library/Fonts/Supplemental/NotoSansLycian-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansLycian-Regular
/System/Library/Fonts/Supplemental/NotoSansLydian-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansLydian-Regular
/System/Library/Fonts/Supplemental/NotoSansMandaic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansMandaic-Regular
/System/Library/Fonts/Supplemental/NotoSansMeeteiMayek-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansMeeteiMayek-Regular
/System/Library/Fonts/Supplemental/NotoSansMongolian-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansMongolian-Regular
/System/Library/Fonts/Supplemental/NotoSansNewTaiLue-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansNewTaiLue-Regular
/System/Library/Fonts/Supplemental/NotoSansNKo-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansNKo-Regular
/System/Library/Fonts/Supplemental/NotoSansOgham-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansOgham-Regular
/System/Library/Fonts/Supplemental/NotoSansOlChiki-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansOlChiki-Regular
/System/Library/Fonts/Supplemental/NotoSansOldItalic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansOldItalic-Regular
/System/Library/Fonts/Supplemental/NotoSansOldPersian-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansOldPersian-Regular
/System/Library/Fonts/Supplemental/NotoSansOldSouthArabian-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansOldSouthArabian-Regular
/System/Library/Fonts/Supplemental/NotoSansOldTurkic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansOldTurkic-Regular
/System/Library/Fonts/Supplemental/NotoSansOsmanya-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansOsmanya-Regular
/System/Library/Fonts/Supplemental/NotoSansPhagsPa-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansPhagsPa-Regular
/System/Library/Fonts/Supplemental/NotoSansPhoenician-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansPhoenician-Regular
/System/Library/Fonts/Supplemental/NotoSansRejang-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansRejang-Regular
/System/Library/Fonts/Supplemental/NotoSansRunic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansRunic-Regular
/System/Library/Fonts/Supplemental/NotoSansSamaritan-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansSamaritan-Regular
/System/Library/Fonts/Supplemental/NotoSansSaurashtra-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansSaurashtra-Regular
/System/Library/Fonts/Supplemental/NotoSansShavian-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansShavian-Regular
/System/Library/Fonts/Supplemental/NotoSansSundanese-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansSundanese-Regular
/System/Library/Fonts/Supplemental/NotoSansSylotiNagri-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansSylotiNagri-Regular
/System/Library/Fonts/Supplemental/NotoSansSyriac-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansSyriac-Regular
/System/Library/Fonts/Supplemental/NotoSansTagalog-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansTagalog-Regular
/System/Library/Fonts/Supplemental/NotoSansTagbanwa-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansTagbanwa-Regular
/System/Library/Fonts/Supplemental/NotoSansTaiLe-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansTaiLe-Regular
/System/Library/Fonts/Supplemental/NotoSansTaiTham-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansTaiTham-Regular
/System/Library/Fonts/Supplemental/NotoSansTaiViet-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansTaiViet-Regular
/System/Library/Fonts/Supplemental/NotoSansThaana-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansThaana-Regular
/System/Library/Fonts/Supplemental/NotoSansTifinagh-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansTifinagh-Regular
/System/Library/Fonts/Supplemental/NotoSansUgaritic-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansUgaritic-Regular
/System/Library/Fonts/Supplemental/NotoSansVai-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansVai-Regular
/System/Library/Fonts/Supplemental/NotoSansYi-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSansYi-Regular
/System/Library/Fonts/Supplemental/NotoSerifBalinese-Regular.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/NotoSerifBalinese-Regular
/System/Library/Fonts/Supplemental/PlantagenetCherokee.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Sathu.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Silom.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Skia.ttf : No FontName. Skipping.
/System/Library/Fonts/Supplemental/Tahoma Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Tahoma Bold
/System/Library/Fonts/Supplemental/Tahoma.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Tahoma
/System/Library/Fonts/Supplemental/Times New Roman Bold Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Times New Roman Bold Italic
/System/Library/Fonts/Supplemental/Times New Roman Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Times New Roman Bold
/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Times New Roman Italic
/System/Library/Fonts/Supplemental/Times New Roman.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Times New Roman
/System/Library/Fonts/Supplemental/Trattatello.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Trattatello
/System/Library/Fonts/Supplemental/Trebuchet MS Bold Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Trebuchet MS Bold Italic
/System/Library/Fonts/Supplemental/Trebuchet MS Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Trebuchet MS Bold
/System/Library/Fonts/Supplemental/Trebuchet MS Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Trebuchet MS Italic
/System/Library/Fonts/Supplemental/Trebuchet MS.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Trebuchet MS
/System/Library/Fonts/Supplemental/Verdana Bold Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Verdana Bold Italic
/System/Library/Fonts/Supplemental/Verdana Bold.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Verdana Bold
/System/Library/Fonts/Supplemental/Verdana Italic.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Verdana Italic
/System/Library/Fonts/Supplemental/Verdana.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Verdana
/System/Library/Fonts/Supplemental/Webdings.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Webdings
/System/Library/Fonts/Supplemental/Wingdings 2.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Wingdings 2
/System/Library/Fonts/Supplemental/Wingdings 3.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Wingdings 3
/System/Library/Fonts/Supplemental/Wingdings.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/Wingdings
/System/Library/Fonts/Supplemental/Zapfino.ttf : No FontName. Skipping.
/System/Library/Fonts/Symbol.ttf : No FontName. Skipping.
/System/Library/Fonts/ZapfDingbats.ttf : No FontName. Skipping.
/Users/ellie/Library/Fonts/econsans-condensed-primary-subset-bd-it.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/econsans-condensed-primary-subset-bd-it
/Users/ellie/Library/Fonts/econsans-condensed-primary-subset-bd.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/econsans-condensed-primary-subset-bd
/Users/ellie/Library/Fonts/econsans-condensed-primary-subset-it.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/econsans-condensed-primary-subset-it
/Users/ellie/Library/Fonts/econsans-condensed-primary-subset-li-it.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/econsans-condensed-primary-subset-li-it
/Users/ellie/Library/Fonts/econsans-condensed-primary-subset-li.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/econsans-condensed-primary-subset-li
/Users/ellie/Library/Fonts/econsans-condensed-primary-subset-md-it.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/econsans-condensed-primary-subset-md-it
/Users/ellie/Library/Fonts/econsans-condensed-primary-subset-md.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/econsans-condensed-primary-subset-md
/Users/ellie/Library/Fonts/econsans-condensed-primary-subset-rg.ttf => /Library/Frameworks/R.framework/Versions/4.0/Resources/library/extrafontdb/metrics/econsans-condensed-primary-subset-rg