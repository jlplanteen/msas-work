**************************************************;
*** Effect of Self Control on Mathematical Ability for Hispanic Students in Early Childhood;

* Longitudinal analysis of mathematical ability from kindergarten to 5th grade;
* Investigation into dropout / missing data for each data collection wave, including using logistic regression
to model probability of missingness.  MAR - missing at random most likely mechanism;
* Covariance structure selected using maximal model structure (unstructured covariance selected);
* Reduction of model terms then completed, using likelihood ratio test to verify adequacy of reduced model.
* Final model fit using REML and adequacy assessed using contrasts.
* Analysis of Cholesky residuals completed to assess model validity;

***  Jacey Planteen; Last modified: 5/2/16;
**************************************************;


*Create reference library;
libname edat '\\Client\C$\...';

options nofmterr;

proc format;
	value Sex
		1 = 'Male'
		2 = 'Female'
		;
	value Race
		1 = 'White, Non-Hispanic'
		2 = 'Black, Non-Hispanic'
		3 = 'Hispanic, Race Specified'
		4 = 'Hispanic, Race Not Specified'
		5 = 'Asian'
		6 = 'Hawaiian or Pacific Islander'
		7 = 'American Indian or Alaska Native'
		8 = 'Multiple Races, Non-Hispanic'
		;
	value Miss
		1 = 'Not Missing'
		2 = 'Drop Out'
		3 = 'Missing Wave'
		4 = 'Not Ascertained'
		5 = 'Not Applicable'
		;
	value RaceCondense
		1 = 'White'
		2 = 'Black'
		3 = 'Hispanic'
		4 = 'Other'
		;
run;


/* Keep only the selected variables */
data edat.child;
   set edat.ECLSK_98_99_K8_CHILD_v1_0(keep=
      CS_TYPE2
      GENDER
      RACE
      R1_KAGE
      R2_KAGE
      R3AGE
      R4AGE
      R5AGE
      R6AGE
      R7AGE
      C1R4MSCL
      C2R4MSCL
      C4R4MSCL
      C5R4MSCL
      C6R4MSCL
	  C7R4MSCL
      T1CONTRO
      T2CONTRO
      T4CONTRO
      T5CONTRO
      T6CONTRO
      P1AGEENT
   );
run;

*Remove all previously associated formats;
proc datasets lib =edat memtype=data;
	modify child;
	attrib _all_ format =;
run;

*Create dummy variables for missing data patterns;
data edat.child2;
	set edat.child;

	*Determine what waves of data are missing for math scores 
	and how they are missing. Recode scores which indicate missing 
	data (i.e. -1 and -9);
	If C6R4MSCL = . then do;
		if C7R4MSCL = . then
			Math6=2;
		else
			Math6=3;
		end;
	Else if C6R4MSCL = -9 then do;
		Math6 = 4;
		C6R4MSCL = .;
		end;
	Else if C6R4MSCL = -1 then do;
		Math6 = 5;
		C6R4MSCL = .;
		end;
	else Math6 = 1;
	
	If C5R4MSCL = . then do;
		if Math6 = 2 then
			Math5 = 2;
		else
			Math5=3;
		end;
	Else if C5R4MSCL = -9 then do;
		Math5 = 4;
		C5R4MSCL = .;
		end;
	Else if C5R4MSCL = -1 then do;
		Math5 = 5;
		C5R4MSCL = .;
		end;
	else Math5 = 1;

	If C4R4MSCL = . then do;
		if Math5 = 2 then
			Math4 = 2;
		else
			Math4=3;
		end;
	Else if C4R4MSCL = -9 then do;
		Math4 = 4;
		C4R4MSCL = .;
		end;
	Else if C4R4MSCL = -1 then do;
		Math4 = 5;
		C4R4MSCL = .;
		end;
	else Math4 = 1;

	If C2R4MSCL = . then do;
		if Math4 = 2 then
			Math2 = 2;
		else
			Math2=3;
		end;
	Else if C2R4MSCL = -9 then do;
		Math2 = 4;
		C2R4MSCL = .;
		end;
	Else if C2R4MSCL = -1 then do;
		Math2 = 5;
		C2R4MSCL = .;
		end;
	else Math2 = 1;

	If C1R4MSCL = . then do;
		if Math2 = 2 then
			Math1 = 2;
		else
			Math1=3;
		end;
	Else if C1R4MSCL = -9 then do;
		Math1 = 4;
		C1R4MSCL = .;
		end;
	Else if C1R4MSCL = -1 then do;
		Math1 = 5;
		C1R4MSCL = .;
		end;
	else Math1 = 1;

	TotalMissing = (Math1 ne 1) + (Math2 ne 1) + (Math4 ne 1) + (Math5 ne 1)
					+ (Math6 ne 1);

	Format Math1 Miss. Math2 Miss. Math4 Miss. Math5 Miss. Math6 Miss.;
	
	*Condense races to fewer categories;
	If Race in (3, 4) then Race2 = 3;
	Else If Race > 4 then Race2 = 4;
	else Race2 = Race;
	Format Race2 RaceCondense.;
	*Only keep subjects with known gender and race;
	where Gender ~= -9 and Race ~=-9;
run;

*Drop subjects who are missing response at all 5 time points;
*Reduce to only Hispanic children;
data edat.Child3;
	set edat.Child2;
	where TotalMissing ~=5 and Race2=3;
run;

*Examine missing patterns;
proc freq data = edat.Child3;
	tables Math1 Math2 Math4 Math5 Math6 TotalMissing;
run;

*Look at missing data pattern for 2nd wave;
proc sort data = edat.Child3;
	by Math2;
proc means data = edat.Child3;
	vars C1R4MSCL;
	by Math2;
run;

*Look at students who dropped out in 2nd wave;
data drop2;
	set edat.Child3;
	where Math2 in (1,2);
run;
*Logistic regression modeling probability of dropout on explanatory variables
and previous math score;
proc genmod data = drop2 desc;
	class Math2 Gender / param=ref;
	model Math2 = C1R4MSCL T1CONTRO P1AGEENT Gender / dist = bin 
			link = logit aggregate noscale type3 lrci;
run;

*Look at missing data pattern for 4th wave;
data drop4;
	set edat.Child4;
	where Math4 in (1,2);
proc genmod data = drop4;
	class Math4 Gender / param=ref;
	model Math4 = C1R4MSCL T1CONTRO C2R4MSCL T2CONTRO Gender P1AGEENT/ dist = bin 
			link = logit aggregate noscale type3 lrci;
run;
*Remove C1R4MSCL - possible multi-collinearity with C2R4MSCL;
proc genmod data = drop4;
	class Math4 Gender / param=ref;
	model Math4 = T1CONTRO C2R4MSCL T2CONTRO Gender P1AGEENT/ dist = bin 
			link = logit aggregate noscale type3 lrci;
run;

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
*Convert to long format;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
data edat.childLong;
	set edat.child3;
	array conts{5} T1CONTRO T2CONTRO T4CONTRO T5CONTRO T6CONTRO;
	array mathi{5} C1R4MSCL C2R4MSCL C4R4MSCL C5R4MSCL C6R4MSCL;
	array round{5} _temporary_ (1 2 4 5 6);
	array grades{5} _temporary_ (0 0.5 1.5 3.5 5.5);
	do i = 1 to 5;
		control = conts{i};
		MathIRT = mathi{i};
		Wave = round{i};
		Time = grades{i};
		output;
	end;
	keep CHILDID GENDER P1AGEENT RACE Race2 Control MathIRT
		Time Height1 TotalMissing;
run;

*Format long data set;
data edat.childlong;
	set edat.childlong;
	format Gender Sex.;
run;


****************************************************;
*Create exploratory graphics;
****************************************************;

*Create dummy variables for missing data;
data edat.childlong3;
	set edat.childlong;
	if control = . then MissControl = 1;
		else MissControl = 0;
	if MathIRT = . then MissMath = 1;
		else MissMath=0;
run;

*Look at missing data pattern;
proc freq data = edat.childlong3;
	tables Time*MissMath*MissControl;
run;


*Individual level graphics;
*Math scores for select individuals;
proc sgpanel data=edat.childlong3;
	panelby CHILDID /columns=4 rows=4;
	reg x = Time y=MathIRT / group=Gender;
	label MathIRT="Math IRT Score" time="Grade" CHILDID="ID";
	Title "Individual Graphs of Math IRT Scores for Select Students";
	where CHILDID in ('0098024C', '0687014C',
			'0705001C', '1042016C', '1134009C',
			'1142016C', '1189007C', '3038013C',
			'0285023C', '0528010C',
			'0856020C', '0940001C', '1126010C',
			'1256008C', '1149013C', '2122002C');
run;

*Self control scores for select individuals;
proc sgpanel data=edat.childlong3;
	panelby CHILDID /columns=4 rows=4;
	reg x = Time y=Control / group=Gender;
	label Control="Self Control Score" time="Grade" CHILDID="ID";
	Title "Individual Graphs of Self Control Scores for Select Students";
	where CHILDID in ('0285023C', '0528010C',
			'0098024C', '0687014C',
			'0705001C', '1042016C', '1134009C',
			'1142016C', '1189007C', '3038013C',
			'0856020C', '0940001C', '1126010C',
			'1256008C', '1149013C', '2122002C');
run;

*Look at distribution of kindergarten entry ages;
proc univariate data = edat.childlong3;
	var P1AGEENT;
	histogram;
	where Time = 0;
	Title "Histogram of Kindergarten Entry Ages";
run;

*Create mean level graphics;
proc sort data=edat.childlong3;
	by Time Gender;
proc means data = edat.childlong3;
	vars MathIRT;
	by Time Gender;
	Output out=MeanMath2 Mean=Avg;
proc means data = edat.childlong3;
	vars Control;
	by Time Gender;
	Output out=MeanControl2 Mean=Avg;
proc sgplot data = MeanMath2;
	series x=Time y=Avg / group=Gender;
	title 'Mean Math Scores by Gender';
	xaxis label='Grade';
	yaxis label='Math IRT Score';
proc sgplot data = MeanControl2;
	series x=Time y=Avg / group=Gender;
	title 'Mean Self Control by Gender';
	xaxis label='Grade';
	yaxis label='Self-Control Score';
run;

*Output key statistics for age and self control;
proc means data = edat.childlong3 
		min Q1 median Q3 max mean var;
	var P1AGEENT ;
	by Time;
	output out = AgeStats mean=Amean;
proc means data = edat.childlong3 
		min Q1 median Q3 max mean var;
	var Control;
	by Time;
	output out = ControlStats mean=Cmean;
run;

*Merge age and self control summary statistics together;
data ControlStats;
	merge ControlStats AgeStats;
	by Time;
run;

*Create a dataset for future predictions;
*Three groups for predictions:
*Group 1: Gender effect: mean age / mean self control, by gender;
*Group 2: K entry age effect: select ages / mean self control, by gender;
*Group 3: Self control effect: select control levels / mean k entry age, by gender;
data prediction;
	set ControlStats;
	CHILDID="PREDICT";
	array Ages{10} _temporary_ (0 54 60 66 72 78 0 0 0 0);
	array SCS{10} _temporary_ (0 0 0 0 0 0 1 2 3 4);
	array Sexes{2} _temporary_ (1 2);
	array Groups{10} _temporary_ (1 2 2 2 2 2 3 3 3 3);
	do i = 1 to 10;
		do j = 1 to 2;
			P1AGEENT = Ages{i};
			Gender = Sexes{j};
			Control = SCS{i};
			Group = Groups{i};
			output;
		end;
	end;
	keep CHILDID Time P1AGEENT Gender Control Group AMean CMean;
data prediction;
	set prediction;
	if P1AGEENT = 0 Then P1AGEENT = AMean;
	if Control = 0 then Control = CMean;
	Drop AMean CMean;
run;


****************************************************;
*** Model for Math IRT Score;
****************************************************;

**Select covariance structure;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

*Fit maximal model, unstructured covariance;
proc mixed data = edat.childlong3;
	class Gender CHILDID Time Race2;
	model MathIRT = Gender|Time|Control|P1AGEENT @3/s;
	repeated Time / type = un sub=CHILDID;
run;

*Fit maximal model, compound symmetry heterogeneous covariance;
*LRT indicates not adequate;
proc mixed data = edat.childlong3;
	class Gender CHILDID Time Race2;
	model MathIRT = Gender|Time|Control|P1AGEENT @3/s;
	repeated Time / type = csh sub=CHILDID;
run;

*Fit maximal model, compound symmetry covariance;
*LRT indicates really not adequate;
proc mixed data = edat.childlong3;
	class Gender CHILDID Time Race2;
	model MathIRT = Gender|Time|Control|P1AGEENT @3/s;
	repeated Time / type = cs sub=CHILDID;
run;


*******MODEL REDUCTION;
*Begin reducing!;
*Fit maximal model, unstructured covariance;
proc mixed data = edat.childlong3;
	class Gender CHILDID Time;
	model MathIRT = Gender|Time|Control|P1AGEENT @3/s;
	repeated Time / type = un sub=CHILDID;
run;

*Remove all 3 way interactions except control*gender*time;
proc mixed data = edat.childlong3;
	class Gender CHILDID Time;
	model MathIRT = Gender|Time|Control|P1AGEENT @2 Control*Gender*Time/s;
	repeated Time / type = un sub=CHILDID;
run;

*Remove all 3 way interactions;
proc mixed data = edat.childlong3;
	class Gender CHILDID Time;
	model MathIRT = Gender|Time|Control|P1AGEENT @2/s;
	repeated Time / type = un sub=CHILDID;
run;


*Remove most 2-way interactions;
*All effects now significant;
proc mixed data = edat.childlong3;
	class Gender CHILDID Time;
	model MathIRT = Gender Time Control P1AGEENT Gender*Time
			Control*Time P1AGEENT*Time/s;
	repeated Time / type = un sub=CHILDID;
run;

*Look at continuous ways to treat time;
*Create quadratic effect for time;
*Spline with knot at 1.5;
data edat.childlong3;
	set edat.childlong3;
	timecopy = time;
	time2 = time*time;
	timek = max(0, time-1.5);
run;

*Linear effect for time;
*Does not appear to be adequate;
proc mixed data = edat.childlong3;
	class Gender CHILDID Timecopy;
	model MathIRT = Gender Time Control P1AGEENT Gender*Time
			Control*Time P1AGEENT*Time/s;
	repeated Timecopy / type = un sub=CHILDID;
run;


*Quadratic effect for time;
*Does not appear to be adequate;
proc mixed data = edat.childlong3;
	class Gender CHILDID Timecopy;
	model MathIRT = Gender Time Time2 Control P1AGEENT Gender*Time
			Control*Time P1AGEENT*Time
			Gender*Time2 Control*Time2 P1AGEENT*Time2/s;
	repeated Timecopy / type = un sub=CHILDID;
run;

*Linear spline for time;
*Does not appear to be adequate;
proc mixed data = edat.childlong3;
	class Gender CHILDID Timecopy;
	model MathIRT = Gender Time Timek Control P1AGEENT Gender*Time
			Control*Time P1AGEENT*Time
			Gender*Timek Control*Timek P1AGEENT*Timek/s;
	repeated Timecopy / type = un sub=CHILDID;
run;


*Test adequacy of final reduced model, time as factor unstructured covariance;
*Use contrasts!;
proc mixed data = edat.childlong3 method=REML;
	class CHILDID Time;
	model MathIRT = Gender|Time|Control|P1AGEENT @3/s;
	repeated Time / type = un sub=CHILDID;
	contrast "Reduced Model Adequate?"
			Control*P1AGEENT*Time 1 0 0 0 0 Control*P1AGEENT 1,
			Control*P1AGEENT*Time 0 1 0 0 0 Control*P1AGEENT 1,
			Control*P1AGEENT*Time 0 0 1 0 0 Control*P1AGEENT 1,
			Control*P1AGEENT*Time 0 0 0 1 0 Control*P1AGEENT 1,
			Control*P1AGEENT*Time 0 0 0 0 1 Control*P1AGEENT 1,
			Control*Gender*Time 1 0 0 0 0 Control*Gender 1,
			Control*Gender*Time 0 1 0 0 0 Control*Gender 1,
			Control*Gender*Time 0 0 1 0 0 Control*Gender 1,
			Control*Gender*Time 0 0 0 1 0 Control*Gender 1,
			Control*Gender*Time 0 0 0 0 1 Control*Gender 1,
			P1AGEENT*Gender*Time 1 0 0 0 0 P1AGEENT*Gender 1,
			P1AGEENT*Gender*Time 0 1 0 0 0 P1AGEENT*Gender 1,
			P1AGEENT*Gender*Time 0 0 1 0 0 P1AGEENT*Gender 1,
			P1AGEENT*Gender*Time 0 0 0 1 0 P1AGEENT*Gender 1,
			P1AGEENT*Gender*Time 0 0 0 0 1 P1AGEENT*Gender 1,
			Control*P1AGEENT*Gender 1 /e;
run;

*Residuals analysis;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;

*Final Model - create and output residuals;
proc mixed data = edat.childlong3 plots(maxpoints=11000) = vcirypanel
		plots(maxpoints=11000)=influencestatpanel;
	class Gender CHILDID Time;
	model MathIRT = Gender Time Control P1AGEENT Gender*Time
			Control*Time P1AGEENT*Time/s vciry influence outpm=margres
			outp=condres;
	repeated Time / type = un sub=CHILDID r=3 rcorr=3;
	lsmeans Gender*Time / slice=Time;
run;

*Create additional residual graphs versus explantory variables;
proc gplot data=margres;
	plot scaledresid*Time;
	plot scaledresid*Gender;
	plot scaledresid*P1AGEENT;
	plot scaledresid*Control;
run;

*Create normal q-q plots of residuals for each time point;
proc univariate data = margres;
	var scaledresid;
	qqplot scaledresid / normal(mu=est sigma=est);
	title "Cholesky Residuals at Time = 0 (Kindergarten Fall)";
	where Time = 0;
proc univariate data = margres;
	var scaledresid;
	qqplot scaledresid / normal(mu=est sigma=est);
	title "Cholesky Residuals at Time = 0.5 (Kindergarten Spring)";
	where Time = 0.5;
proc univariate data = margres;
	var scaledresid;
	qqplot scaledresid / normal(mu=est sigma=est);
	title "Cholesky Residuals at Time = 1.5 (1st Grade Spring)";
	where Time = 1.5;
proc univariate data = margres;
	var scaledresid;
	qqplot scaledresid / normal(mu=est sigma=est);
	title "Cholesky Residuals at Time = 3.5 (3rd Grade Spring)";
	where Time = 3.5;
proc univariate data = margres;
	var scaledresid;
	qqplot scaledresid / normal(mu=est sigma=est);
	title "Cholesky Residuals at Time = 5.5 (5th Grade Fall)";
	where Time = 5.5;
run;

*Outlier analysis;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
proc sort data = margres;
	by descending scaledresid;
run;
data outlierstudent;
	set margres;
	where CHIlDID='0528010C';
proc print data = outlierstudent;
run;

*Create data set with outlying observation removed;
data childlongoutrem;
	set edat.childlong3;
	if (CHILDID='0528010C' & Time=0) Then
		MathIRT=.;
run;

*Fit model without outlier;
proc mixed data = childlongoutrem;
	class Gender CHILDID Time;
	model MathIRT = Gender Time Control P1AGEENT Gender*Time
			Control*Time P1AGEENT*Time/s;
	repeated Time / type = un sub=CHILDID;
run;


*Predictions with final model for final graphics;
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;
*Final Model - predictioins;
*Merge predication data set with raw data;
data childlong4;
	set edat.childlong3 prediction;
run;

*Fit model using data with new observations for prediction;
proc mixed data = childlong4;
	class Gender CHILDID Time;
	model MathIRT = Gender Time Control P1AGEENT Gender*Time
			Control*Time P1AGEENT*Time/s outp=PREDICTED;
	repeated Time / type = un sub=CHILDID;
run;

*Keep only the desired summary predictions;
data predicted;
	set predicted;
	where CHILDID="PREDICT";
run;


*Mean Predicted Gender plots - controling for entry age & self-control;
proc sgplot data = predicted;
	where group=1;
	series x = Time y = pred / Group = Gender;
	yaxis label="Predicted Math IRT Score" min=20 max=120;
	xaxis label = "Grade";
	title "Predicted Math Performance by Sex";
  
*Mean Male Predicted graphs by Kindergarten entry age;
proc sgplot data = predicted;
	where group=2 and Gender=1;
	series x = Time y = pred / Group = P1AGEENT;
	yaxis label="Predicted Math IRT Score" min=20 max=120;
	xaxis label = "Grade";
	title "Predicted Math Performance for Males by Kindergarten Entry Age";
  
*Mean Female Predicted graphs by kindergarten entry age;
proc sgplot data = predicted;
	where group=2 and Gender=2;
	series x = Time y = pred / Group = P1AGEENT;
	yaxis label="Predicted Math IRT Score" min=20 max=120;
	xaxis label = "Grade";
	title "Predicted Math Performance for Females by Kindergarten Entry Age";
  
*Mean Male Predicted graphs by self control score;
proc sgplot data = predicted;
	where group=3 and Gender=1;
	series x = Time y = pred / Group = Control;
	yaxis label="Predicted Math IRT Score" min=20 max=120;
	xaxis label = "Grade";
	keylegend / title="Self Control Score";
	title "Predicted Math Performance for Males by Self Control Score";
  
*Mean Female Predicted graphs by self control score;
proc sgplot data = predicted;
	where group=3 and Gender=2;
	series x = Time y = pred / Group = Control;
		yaxis label="Predicted Math IRT Score" min=20 max=120;
	xaxis label = "Grade";
	keylegend / title="Self Control Score";
	title "Predicted Math Performance for Females by Self Control Score";
run;

