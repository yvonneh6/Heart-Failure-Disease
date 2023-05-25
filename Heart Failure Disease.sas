*\ Final project STAT448 Spr 23 *\;
*\ Data set was obtained from the UCI Machine Learning Repository:
https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records;
data heartf;
	infile '/home/user/sasuser.v94/Project/heart_failure_clinical_records_dataset.csv' dsd missover firstobs=2;
	input age anaemia creatinine_p diab eject_f hbp plat serum_c serum_s sex smk time Death_E;
run;

proc sort data=heartf;
by Death_E;
run;

%let NumVar=age creatinine_p eject_f plat serum_c serum_s time;
%let CatVar=anaemia hbp diab sex smk;

/*************************************************************************
      *                                                                  *
      *  Preliminary Analysis       									 *
      *                                                                  *
      ********************************************************************/

/* Descriptive Statistics */
proc means data=heartf NMISS RANGE MIN Q1 MEDIAN MEAN Q3 MAX  STD SKEW  MAXDEC=4; 
var  Death_E;
run;


proc means data=heartf NMISS RANGE MIN Q1 MEAN Q3 MAX  STD SKEW  MAXDEC=4; 
var &NumVar;
run;


proc means data=heartf NMISS RANGE MIN Q1 MEDIAN Q3 MAX  STD SKEW  MAXDEC=4; 
var &CatVar;
run;

/* Response Variable */
proc sgplot data=heartf;
   vbar  Death_E /group=Death_E datalabel;
run;

proc univariate data=heartf normal;
var Death_E;
qqplot Death_E /Normal;
hist Death_E;
run;



/* Correlation Matrix */
PROC SGSCATTER data = heartf; 
MATRIX &NumVar/ ellipse diagonal = (histogram normal) group = Death_E ;
RUN ;

/* Pearson Correlation */
proc corr data=heartf;
var &NumVar;
run;

/* Continuous Variables vs Death_E */
proc sgpanel data=heartf;
   panelby Death_E;
%macro create_box_plots;
   %local i var;
   %do i = 1 %to %sysfunc(countw(&NumVar));
      %let var = %scan(&NumVar, &i);
      proc sgplot data=heartf;
         vbox &var / group=Death_E ;
         title "&var vs. Death_E";
      run;
   %end;
%mend create_box_plots;

%create_box_plots;
run;



/*Categorical Varaibles Barplot */

proc sgpanel data=heartf;
%macro create_box_plots;
   %local i var;
   %do i = 1 %to %sysfunc(countw(&NumVar));
      %let var = %scan(&CatVar, &i);
      proc sgplot data=heartf;
         vbar &var / group=Death_E datalabel;
         title "&var vs. Death_E";
      run;
   %end;
%mend create_box_plots;

%create_box_plots;
run;

/* Categorical Variables vs Death_E */

%macro generate_freq_tables;
   %local i var;
   
   %do i = 1 %to %sysfunc(countw(&CatVar));
      %let var = %scan(&CatVar, &i);
      
      proc freq data=heartf;
         tables &var * Death_E / nopercent norow nocol expected chisq exact relrisk riskdiff;
      run;
   %end;
%mend;

%generate_freq_tables;

* Normality Tests for Continuous Varaibles;
%macro normality_test;
   %local i var;
   
   %do i = 1 %to %sysfunc(countw(&NumVar));
      %let var = %scan(&NumVar, &i);
      
      proc univariate data=heartf normal;
         class Death_E;
         var &var;
         ods select BasicMeasures TestsForNormality;
         ods graphics on;
      run;
   %end;
%mend;

%normality_test;

*Wilcoxon Rank Sum Test;

%macro generate_npar1way_analysis;
   %local i var;
   
   %do i = 1 %to %sysfunc(countw(&NumVar));
      %let var = %scan(&NumVar, &i);
      
      proc npar1way data=heartf wilcoxon;
         class Death_E;
         var &var;
         ods exclude KruskalWallisTest;
      run;
   %end;
%mend;

%generate_npar1way_analysis;

/*************************************************************************
      *                                                                  *
      *  Logistics Regression       									 *
      *                                                                  *
      ********************************************************************/
/* Initial Full Model */
proc logistic data=heartf plots(label)=(effect roc);
	class &CatVar;
    model Death_E = age--time;
	output out=heartf2 cbar=cbar;
    ods exclude EffectPlot;
run;

/* Remove Influential Point */
data heartf3;
  set heartf2;
  where cbar <= 1;
run;

/* Refit Model then Stepwise Selection */
proc logistic data=heartf3 plots=(effect roc);
	class &CatVar;
    model Death_E = age--time / selection=stepwise sle=.05 sls=.05;
    output predprobs=individual out=heartf_out;	
    ods exclude EffectPlot;
run;

/* Effect Plots */
%let SelectedVar0 = age eject_f serum_c time;

%let numVars0 = %sysfunc(countw(&SelectedVar0));

%macro generate_effect_plots;
   %do i = 1 %to &numVars0;
      %let var0 = %scan(&SelectedVar0, &i);
      
      ods region;
      ods select EffectPlot;
      proc logistic data=heartf3 plots=effect;
         model Death_E (event='1') = &var0;
         ods output EffectPlot=EffectPlot_&var0;
         title "&var0";
      run;
      
      ods region close;
   %end;
%mend;

ods layout gridded columns=1 rows=%sysfunc(countw(&SelectedVar0));
%generate_effect_plots;
ods layout end;

/* New Logistic Model excludes Time then Stepwise Selection */
proc logistic data=heartf plots=(roc);
	class &CatVar;
    model Death_E = age -- smk / sle=.05 sls=.05;
	
run;
proc logistic data=heartf plots=( roc);
	class &CatVar;
    model Death_E = age -- smk / selection=stepwise sle=.05 sls=.05;
	output predprobs=individual out=heartf_out0;	
run;


/* Effect Plots */

%let SelectedVar1 = age eject_f serum_c;

%let numVars1 = %sysfunc(countw(&SelectedVar1));

%macro generate_effect_plots;
   %do i = 1 %to &numVars1;
      %let var1 = %scan(&SelectedVar1, &i);
      
      ods region;
      ods select EffectPlot;
      proc logistic data=heartf3 plots=effect;
         model Death_E (event='1') = &var1;
         ods output EffectPlot=EffectPlot_&var1;
         title "&var1";
      run;
      
      ods region close;
   %end;
%mend;

ods layout gridded columns=1 rows=%sysfunc(countw(&SelectedVar1));
%generate_effect_plots;
ods layout end;


/* Optimal Logistic Model Confusion Matrix */
proc freq data=heartf_out0;
    tables Death_E*_into_/nopercent norow nocol;
run;

/*************************************************************************
      *                                                                  *
      *  Decision Tree       											 *
      *                                                                  *
      ********************************************************************/
/* Full Model */
proc hpsplit data=heartf seed=15531;
class Death_E;
model Death_E = age -- time;
grow entropy;
prune costcomplexity;
run;

/* Model excludes Time */
proc hpsplit data=heartf seed=15531;
class Death_E;
model Death_E = age -- smk;
grow entropy;
prune costcomplexity;
run;
