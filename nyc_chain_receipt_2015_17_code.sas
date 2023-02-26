/*Code for results in "Changes in consumer purchasing patterns at New York City chain restaurants following adoption of the sodium warning icon rule, 2015-2017"*/
/*Dataset posted on Github*/
/*Please review accompanying README file and codebook posted on Github*/


/*Call in dataset*/
/*insert libname and filepath here*/

/*Format Statements*/
proc format;
value EDUCATION_FMT
	1='Less than High School'
	2='High School Graduate or GED'
	3='Some College or Technical School (1 to 3 years)'
	4='College Graduate'
	. = "Refused";
value AGE_FMT
	1="18-24"
	2="25-34"
	3="35-44"
	4="45-64"
	5="65+"
	.="Refused";
value GENDER_FMT
	1="Male"
	2="Female"
	. = "Missing";
value RACE_FMT
	1="Asian/PI, non-Latino"
	2="Black, non-Latino"
	3="Latino"
	4="White, non-Latino"
	5="Other, non-Latino"
	.="Don't know/refused";	
value LOCATION_FMT
	1="NYC"
	2="Yonkers";
value YN_FMT
	0 = "No"
	1 = "Yes";
value EXCLUSIONS_FMT
	0 = "Include"
	1 = "Exclude, no match"
	2 = "Exclude, only purchased alcohol"
	3 = "Exclude, no nutrition info for entire purchase"
	4 = "Exclude, outlier day"
	5 = "Exclude, only purchased beverage"
	6 = "Exclude, sodium outlier";
run;

data eval;
/*set step - insert datafile name here*/
format gender GENDER_FMT. race RACE_FMT. education EDUCATION_FMT. agegroup AGE_FMT. location LOCATION_FMT. warningpurchase YN_FMT. twowarns YN_FMT. 
excl_detail EXCLUSIONS_FMT.;
run;
/*4844 observations and 24 variables*/

/*********************************************************************
Tables  1 & 2*********************************************************
*********************************************************************/

/*Table 1 - FSR Descriptives*/
proc freq data=eval;
where location=1 and type="fsr" and include=1; *FSR, NYC;
*where location=2 and type="fsr" and include=1; *FSR, Yonkers;
tables wave*(restaurant gender agegroup race education residence)/list missing chisq nocol nocum nopercent ;
run;

proc freq data=eval;
where wave=1 and type="fsr" and include=1; *just for NYC vs Yonkers 2015 p values;
*where wave=2 and type="fsr" and include=1;*just for NYC vs Yonkers 2017 p values;
tables location*(restaurant gender agegroup race education residence)/list missing chisq nocol nocum norow nopercent ;
run;


/*Table 2 - QSR descriptives*/
proc freq data=eval;
where location=1 and type="qsr" and include=1;
*where location=2 and type="qsr" and include=1;
tables wave*(restaurant gender agegroup race education residence)/list missing chisq nocol nocum nopercent ;
run;

proc freq data=eval;
where wave=1 and type2="qsr" and include=1;*just for NYC vs Yonkers 2015 p values;
*where wave=2 and type2="qsr" and include=1;*just for NYC vs Yonkers 2017 p values;
tables location*(restaurant gender agegroup race education residence)/list missing chisq nocol nocum norow nopercent ;
run;

/**************************************************************
Table 3*****************************************************
***************************************************************/


/**********Mean # high-sodium items purchased********************/
proc glimmix data=eval;
where type="fsr" and include=1; /*full-service*/
*where type="qsr" and include=1; /*quick-service*/
class address wave (ref=first) location (ref=last) outlet gender race education agegroup;
model totalwarnings=wave|location outlet gender race education agegroup/dist=poisson solution; 
random _residual_/ subject=address type=ar(1);
lsmeans wave|location/   diff cl ilink;
run;


/******at least 1 high-sodium item purchased*********************/
proc glimmix data=eval;
where type="fsr" and include=1; /*full-service*/
*where type="qsr" and include=1; /*quick-service*/
class address wave (ref=first) location (ref=last) outlet gender race education agegroup;
model warningpurchase (event='Yes')= wave|location outlet gender race education agegroup/dist=binary link=logit solution; 
random _residual_/ subject=address type=ar(1);
lsmeans wave|location/   diff cl ilink  ;
run;

/******at least 2 high-sodium items purchased*********************/
*note - FSR only, QSR did not have 2+ warning item purchases;
proc glimmix data=eval;
where type="fsr" and include=1; /*full-service*/
class address wave (ref=first) location (ref=last) outlet gender race education agegroup;
model twowarns (event='Yes')= wave|location outlet gender race education agegroup/dist=binary link=logit solution; 
random _residual_/ subject=address type=ar(1);
lsmeans wave|location/   diff cl ilink  ;
run;

/********************************************************************
*Figures 3 & 4, Supplemental Table 5 ********************************
*********************************************************************/

/**********square-root transformed mean sodium and calories*************/
*note, p-values used in figures 3 & 4;
proc glimmix data=eval;
where type="fsr" and include=1; /*full-service*/
*where type="qsr" and include=1; /*quick-service*/
class  address wave (ref=first) location (ref=last) outlet gender race education agegroup;
model sq_sod=wave|location outlet gender race education agegroup/solution; 
*model sq_cal=wave|location outlet gender race education agegroup/solution;
random _residual_/subject=address type=ar(1);
lsmeans wave|location/  diff cl;
run;

/**********non-transformed mean sodium and calories*********************/
*note, mean values used in figures 3 & 4;
proc glimmix data=eval;
where type="fsr" and include=1; /*full-service*/
*where type="qsr" and include=1; /*quick-service*/
class  address wave (ref=first) location (ref=last) outlet gender race education agegroup;
model purchased_sod=wave|location outlet gender race education agegroup/solution; 
*model purchased_cal=wave|location outlet gender race education agegroup/solution;
random _residual_/subject=address type=ar(1);
lsmeans wave|address/  diff cl;
run;

/**************************************************************************
Supplemental Tables 6 & 7**************************************************
**************************************************************************/

/**********Mean # high-sodium items purchased at individual FSR chains********************/
proc glimmix data=eval;
where restaurant="IHOP" and include=1; /*IHOP*/
*where restaurant="TGI Friday's" and include=1; /*TGIF*/
class address wave (ref=first) location (ref=last) gender race education agegroup;
model totalwarnings=wave|location gender race education agegroup/dist=poisson solution; 
random _residual_/ subject=address type=ar(1);
lsmeans wave|location/   diff cl ilink;
run;


/**********Mean # high-sodium items purchased at individual QSR chains********************/
proc glimmix data=eval;
where restaurant="Popeyes" and include=1; /*Popeyes*/
*where restaurant="Subway" and include=1; /*Subway*/
class address wave (ref=first) location (ref=last) gender race education agegroup;
model totalwarnings=wave|location gender race education agegroup/dist=poisson solution; 
random _residual_/ subject=address;
lsmeans wave|location/   diff cl ilink;
run;


/******at least 1 high-sodium item purchased at individual FSR chains*********************/
proc glimmix data=eval;
where restaurant="IHOP" and include=1; /*IHOP*/
*where restaurant="TGI Friday's" and include=1; /*TGI Friday's*/
class address wave (ref=first) location (ref=last) gender race education agegroup;
model warningpurchase (event='Yes')= wave|location gender race education agegroup/dist=binary link=logit solution; 
random _residual_/ subject=address type=ar(1);
lsmeans wave|location/   diff cl ilink  ;
run;


/******at least 1 high-sodium item purchased at individual QSR chains*********************/
proc glimmix data=eval;
where restaurant="Popeyes" and include=1; /*Popeyes*/
*where restaurant="Subway" and include=1; /*Subway*/
class address wave (ref=first) location (ref=last) gender race education agegroup;
model warningpurchase (event='Yes')= wave|location gender race education agegroup/dist=binary link=logit solution; 
random _residual_/ subject=address;
lsmeans wave|location/   diff cl ilink  ;
run;


/******at least 2 high-sodium items purchased at individual FSR chains*********************/

proc glimmix data=eval;
where restaurant="IHOP" and include=1; /*IHOP*/
*where restaurant="TGI Friday's" and include=1; /*TGI Friday's*/
class address wave (ref=first) location (ref=last) gender race education agegroup;
model twowarns (event='Yes')= wave|location gender race education agegroup/dist=binary link=logit solution; 
random _residual_/ subject=address type=ar(1);
lsmeans wave|location/   diff cl ilink  ;
run;


/**********square-root transformed mean sodium at individual restaurant chains*************/
proc glimmix data=eval;
where restaurant="IHOP" and include=1; /*IHOP*/
*where restaurant="TGI Friday's" and include=1; /*TGI Friday's*/
*where restaurant="Popeyes" and include=1; /*Popeyes*/
*where restaurant="Subway" and include=1; /*Subway*/
class address wave (ref=first) location (ref=last) gender race education agegroup;
model sq_sod=wave|location gender race education agegroup/solution; 
random _residual_/subject=address type=ar(1);
lsmeans wave|location/  diff cl;
run;

/**********non-transformed mean sodium at individual restaurant chains*********************/
proc glimmix data=eval;
where restaurant="IHOP" and include=1; /*IHOP*/
*where restaurant="TGI Friday's" and include=1; /*TGI Friday's*/
*where restaurant="Popeyes" and include=1; /*Popeyes*/
*where restaurant="Subway" and include=1; /*Subway*/
class address wave (ref=first) location (ref=last) gender race education agegroup;
model purchased_sod=wave|location gender race education agegroup/solution; 
random _residual_/subject=address type=ar(1);
lsmeans wave|location/  diff cl;
run;

/**************END***************/


