/* Making library for hurricane data */
libname storm "/opt/sas/home/mdreicha/sasuser.viya/Survival_Analytics/Homework";

/* Making macro for main effect variables */
%let main_effects = age backup bridgecrane elevation gear servo slope trashrack;

/* General proportional hazards model before assessing assumptions */
/* From this model we can determine that age, servo, and slope are significant */
/* Next step is to check assumptions */
proc phreg data=storm.hurricane;
	model hour*reason(0,1,3,4) = &main_effects/ ties=efron risklimits=pl;
run;

/* Asessing the proportional hazards assumption for linearity by using all the variables */
/* All variables meet the proportional hazards assumption for linearity except trashrack */
/* This method is doing it by using martingale residuals to estimate through simulation what */
/* the model should looks like in terms of residuals and what it actually looks like */
proc phreg data=storm.hurricane;
	model hour*reason(0,1,3,4) = &main_effects/ ties=efron;
	assess ph / resample;
run;

/* This is testing the assumptions for Proportional Hazards test by using schoenfeld residuals */
/* For this section, the residuals are plotted against the linear function of time */
proc phreg data=storm.hurricane zph(global transform=identity fit=loess);
	model hour*reason(0,1,3,4) = &main_effects/ ties=efron;
	ods output zphTest = ident_ph;
run;

/* This is testing the assumptions for Proportional Hazards test by using schoenfeld residuals */
/* For this section, the residuals are plotted against the logistic function of time */
proc phreg data=storm.hurricane zph(global transform=log fit=loess);
	model hour*reason(0,1,3,4) = &main_effects/ ties=efron;
	ods output zphTest = log_ph;
run;

/* Based upon both Proportional Hazard tests all the variables meet the proportional hazard tests */

/* This is used for doing automatic selection techniques using proportional hazards with a confidence */
/* value of 0.03 with backward selection */
proc phreg data=storm.hurricane;
	model hour*reason(0,1,3,4) = &main_effects/ ties=efron alpha=0.03 risklimits=pl selection=backward;
run;

/* Making a macro for the significant variables as detemined by significance value */
%let sig_effects = age servo slope;

/* Reassessing the Proportional Hazard assumptions for the significant variables only */
/* I don't know if this is necessary to be reran with only the significant values */
/* After running it, age sometimes meets the assumption and slope does not meet the assumption */
proc phreg data=storm.hurricane;
	model hour*reason(0,1,3,4) = &main_effects/ ties=efron;
	assess var=(&sig_effects) / resample;
run;

/* For this section, the residuals are plotted against the linear function of time for significant variables */
proc phreg data=storm.hurricane zph(global transform=identity fit=loess);
	model hour*reason(0,1,3,4) = &sig_effects/ ties=efron;
	ods output zphTest = ident_sig;
run;

/* For this section, the residuals are plotted against the logistic function of time for significant variables */
proc phreg data=storm.hurricane zph(global transform=log fit=loess);
	model hour*reason(0,1,3,4) = &sig_effects/ ties=efron;
	ods output zphTest = log_sig;
run;

/* Running highest correlation numbers for significant variables that don't meet assumptions */
/* Trying to see if theres any time-depenent coefficients */
proc phreg data=storm.hurricane;
	model hour*reason(0,1,3,4) = &sig_effects ageloghour slopehour/ ties=efron;
	ageloghour = age*log(hour);
	slopehour = slope * hour;
run;

/* Checking for time-dependent variable */
/* Please see note about issue with time-dependent variable */
proc phreg data=storm.hurricane;
	model hour*reason(0,1,3,4) = &main_effects running;
	array H(*) H1-H48;
	running = H[hour];
run;

/* Checking concordance to see how our model is running */
proc phreg data=storm.hurricane concordance;
	model hour*reason(0,1,3,4) = &sig_effects/ ties=efron risklimits=pl;
run;

/* Running concordance on main effects to see if improvement for adding additional variables */
/* When running the all main effects we received a great concordance value */
proc phreg data=storm.hurricane concordance;
	model hour*reason(0,1,3,4) = &main_effects/ ties=efron risklimits=pl;
run;