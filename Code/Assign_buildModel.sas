%macro FreqReport(dsn);
proc datasets nolist lib=work;
delete FreqReport;
run;

%global obs vars;
%ObsAndVars(&dsn);
%varlist(&dsn);

%local i j;
 %do j=1 %to &nvars;
proc freq data=&dsn noprint order=freq;
tables %qscan(&varlist,&j)/out=freqout missing missprint;
run;

data missing nonmissing;
set  freqout;
if %qscan(&varlist,&j) =  '' then output missing;
if %qscan(&varlist,&j) ne '' then output nonmissing;
run;

proc summary data=nonmissing;
var count;
output out=nomissfigs n=catnomiss max=countmax;
run;

data top3;
set  nonmissing;
if _n_ <=3;
run;

proc summary data=top3;
var count;
output out=top3count sum=Top3Sum;
run;

data record;
format varname $50.
       pctmiss pctmax pct3 percent6.;
label countmiss="Missing Count"
      CatNoMiss="# Non Missing Categories"
      CountMax= "# In Largest Non-Missing Category"
	  Top3Sum=  "# In Three Largest Categories"
      pctmiss="Missing Percent";;
varname="%qscan(&varlist,&j)";
merge missing    (keep=count rename=(count=countmiss))
      nomissfigs (keep=CatNoMiss CountMax)
      top3count  (keep=Top3Sum)
      ;
pctmiss=countmiss/&nobs;
pctMax =CountMax/&nobs;
pct3   =Top3Sum/&nobs;
run;


proc append data=record base=FreqReport;
run;
%end;

data temp;
set  FreqReport;
len=length(varname);
run;

proc summary data=temp;
var len;
output out=maxlen max=;
run;

data _null_;
set  maxlen;
call symput('len',len);
run;

data FreqReport2;
format varname $&len..;
set  FreqReport;
run;

proc contents data=&dsn varnum noprint out=contents;
run;

proc sort data=contents (rename=(name=varname));
by varname;
run;

proc sort data=freqreport2;
by varname;
run;

data  FreqReportWithLabels;
merge FreqReport2 (in=infreq)
      contents (in=incontents keep=varname label varnum type)
	  ;
by    varname;
if    infreq and incontents;
run;

proc sort data=FreqReportWithLabels;
by varnum;
run;

proc format;
value type 1='Numb' 2='Char';
run;

proc print data=FreqReportWithLabels;
var varname type pctmiss CatNoMiss PctMax pct3 countmiss CountMax Top3Sum;
format type type.;
title "Freq Report for File &dsn";
run;
title;

options nomprint;
%mend FreqReport;


%Macro DissGraphMakerLogOdds(dsn,groups,indep,dep);
proc summary data=&dsn;
var &indep;
output out=Missing&indep nmiss=;
run;

data Missing&indep;
set  Missing&indep;
PctMiss=100*(&indep/_freq_);
rename &indep=NMiss;
run;

data _null_;
set  Missing&indep;
call symput ('Nmiss',Compress(Put(Nmiss,6.)));
call symput ('PctMiss',compress(put(PctMiss,4.)));
run;

proc rank data=&dsn groups=&groups out=RankedFile;
var &indep;
ranks Ranks&indep;
run;

proc summary data=RankedFile nway missing;
class Ranks&indep;
var &dep &indep;
output out=GraphFile mean=;
run;

data graphfile;
set  graphfile;
logodds=log(&dep/(1-&dep));
run;

data graphfile setaside;
set  graphfile;
if &indep=. then output setaside;
else             output graphfile;
run;

data _null_;
set  setaside;
call symput('LogOdds',compress(put(LogOdds,4.2)));
run;

proc plot data=graphfile;
plot LogOdds*&indep=' ' $_FREQ_ /vpos=20;
title "&dep by &groups Groups of &indep NMiss=&Nmiss PctMiss=&PctMiss%  LogOdds in Miss=&LogOdds"
;
run;
title;
quit;
%Mend DissGraphMakerLogOdds;


%macro ObsAndVars(dsn);
%global nobs nvars;
%let dsid=%sysfunc(open(&dsn));   
%let nobs=%sysfunc(attrn(&dsid,nobs));     
%let nvars=%sysfunc(attrn(&dsid,nvars));   
%let rc=%sysfunc(close(&dsid));            
%put nobs=&nobs nvars=&nvars;   
%mend ObsAndVars;


%macro varlist(dsn);
options nosymbolgen;
 %global varlist cnt;
 %let varlist=;

/* open the dataset */
 %let dsid=%sysfunc(open(&dsn));

/* count the number of variables in the dataset */
 %let cnt=%sysfunc(attrn(&dsid,nvars));

 %do i=1 %to &cnt;
 %let varlist=&varlist %sysfunc(varname(&dsid,&i));
 %end;

/* close the dataset */
 %let rc=%sysfunc(close(&dsid));
*%put &varlist;
%mend varlist;

%macro CatToBinWithDrop(filename,id,varname);
data &filename;
set  &filename;
%unquote(&varname._)= &varname; if &varname =' ' then %unquote(&varname._)='x';
run;
proc transreg data=&filename DESIGN;
model class (%unquote(&varname._)/ ZERO='x');
output out = %unquote(&varname._)(drop = Intercept _NAME_ _TYPE_);
id &ID;
run;
proc sort data=%unquote(&varname._);by &ID;
data &filename (drop=&varname %unquote(&varname._));
merge &filename %unquote(&varname._);
by &ID;
run;
proc datasets nolist;
delete %unquote(&varname._);
run;
quit;
%mend CatToBinWithDrop;
PROC IMPORT OUT= WORK.S26                                                                                                                   
            DATAFILE= "C:\S26.csv"                                                                                                          
            DBMS=CSV REPLACE;                                                                                                               
     GETNAMES=YES;                                                                                                                          
     DATAROW=2;                                                                                                                             
RUN;                                                                                                                                        
                                                                                                                                            
proc contents data=work.s26;                                                                                                                
run;                                                                                                                                        
                                                                                                                                            
proc freq data=s26;                                                                                                                         
run;                                                                                                                                        
data s26_1(keep=seqnum--AMOTSC resp respholdout)
;
set  s26 ;
rand=ranuni(092765);
     if rand <=.7 then RespHoldout=.;
else if rand  >.7 then do;
   RespHoldout=Resp;
   Resp=.
   ;
end;
run;                                                                                                                                
                                                                                                                                                                                                                                                                                     
data s26_2;                                                                                                                                 
set  s26_1;                                                                                                                                   
array orig[10](0, 1, 2,  3,  4,  5,   6,   7,    8,    9);                                                                                  
array new[10] (0,25,75,150,350,750,3000,7500,15000,30000);                                                                                  
retain orig1-orig10 new1-new10;                                                                                                             
do i=1 to dim(orig);                                                                                                                        
 if PWAPAR=orig[i] then PWAPAR2=new[i];                                                                                                     
 if PPERSA=orig[i] then PPERSA2=new[i];                                                                                                     
 if PAANHA =orig[i] then PAANHA2=new[i];                                                                                                    
 if AWAPAR=orig[i] then AWAPAR2=new[i];                                                                                                     
 if APERSA=orig[i] then APERSA2=new[i];                                                                                                     
 if AMOTSC=orig[i] then AMOTSC2=new[i];                                                                                                     
end;                                                                                                                                        
drop orig1--orig10 new1--new10 i;                                                                                                           
run;                                                                                                                                                                                                                                                                                   
                                                                                                                                            
proc freq data=s26_2;                                                                                                                       
tables      PWAPAR*PWAPAR2                                                                                                                  
            PPERSA*PPERSA2                                                                                                                  
            PAANHA*PAANHA2                                                                                                                  
            AWAPAR*AWAPAR2                                                                                                                  
            APERSA*APERSA2                                                                                                                  
            AMOTSC*AMOTSC2/list;                                                                                                            
run;                                                                                                                                        
                                                                                                                                            
data s26_2;                                                                                                                                 
set  s26_2;                                                                                                                                 
drop pwapar paanha ppersa APERSA AWAPAR AMOTSC;                                                                                             
run;                                                                                                                                        
                                                                                                                                                                                                                                                                                      
data s26_3;                                                                                                                                 
set  s26_2;                                                                                                                                 
array orig[10](0,  1, 2, 3, 4, 5, 6, 7, 8,  9);                                                                                             
array new[10] (0,5.5,17,30,43,56,69,82,94,100);                                                                                             
retain orig1-orig10 new1-new10;                                                                                                             
do i=1 to dim(orig);                                                                                                                        
if MSKA =orig[i] then MSKA2 =new[i];                                                                                                        
if MHHUUR =orig[i] then MHHUUR2 =new[i];                                                                                                    
if MFALLE =orig[i] then MFALLE2 =new[i];                                                                                                    
if MSKB2  =orig[i] then MSKB22 =new[i];                                                                                                     
if MRELGE =orig[i] then MRELGE2 =new[i];                                                                                                    
if MSKC =orig[i] then MSKC2 =new[i];                                                                                                        
if MAUT2 =orig[i] then MAUT22 =new[i];                                                                                                      
if MAUT0  =orig[i] then MAUT02 =new[i];                                                                                                     
if MFWEKI =orig[i] then MFWEKI2 =new[i];                                                                                                    
if MAUT1  =orig[i] then MAUT12 =new[i];                                                                                                     
if MINKGE =orig[i] then MINKGE2 =new[i];                                                                                                    
if MOPLHO =orig[i] then MOPLHO2 =new[i];                                                                                                    
if MGODRK =orig[i] then MGODRK2 =new[i];                                                                                                    
if MSKB1 =orig[i] then MSKB12 =new[i];                                                                                                      
if MGODPR =orig[i] then MGODPR2 =new[i];                                                                                                    
end;                                                                                                                                        
drop orig1--orig10 new1--new10 i;                                                                                                           
run;                                                                                                                                        
                                                                                                                                            
                                                                                                                                            
proc freq data=s26_3;                                                                                                                       
tables                                                                                                                                      
MSKA*MSKA2                                                                                                                                  
MHHUUR*MHHUUR2                                                                                                                              
MFALLE*MFALLE2                                                                                                                              
MSKB2*MSKB22                                                                                                                                
MRELGE*MRELGE2                                                                                                                              
MSKC*MSKC2                                                                                                                                  
MAUT2*MAUT22                                                                                                                                
MAUT0*MAUT02                                                                                                                                
MFWEKI*MFWEKI2                                                                                                                              
MAUT1*MAUT12                                                                                                                                
MINKGE*MINKGE2                                                                                                                              
MOPLHO*MOPLHO2                                                                                                                              
MGODRK*MGODRK2                                                                                                                              
MSKB1*MSKB12                                                                                                                                
MGODPR*MGODPR2                                                                                                                              
/list;                                                                                                                                      
run;                                                                                                                                        
                                                                                                                                            
data s26_4;                                                                                                                                 
set  s26_3;                                                                                                                                 
drop                                                                                                                                        
MSKA                                                                                                                                        
MHHUUR                                                                                                                                      
MFALLE                                                                                                                                      
MSKB2                                                                                                                                       
MRELGE                                                                                                                                      
MSKC                                                                                                                                        
MAUT2                                                                                                                                       
MAUT0                                                                                                                                       
MFWEKI                                                                                                                                      
MAUT1                                                                                                                                       
MINKGE                                                                                                                                      
MOPLHO                                                                                                                                      
MGODRK                                                                                                                                      
MSKB1                                                                                                                                       
MGODPR                                                                                                                                      
;                                                                                                                                           
run;                                                                                                                                        
                                                                                                                                            
data indep;                                                                                                                                 
set  s26_4 (drop=resp seqnum moshoo mostyp);                                                                                                
run;                                                                                                                                        
                                                                                                                                            
%ObsAndVars(indep);                                                                                                                         
%varlist(indep);                                                                                                                            
                                                                                                                                            
                                                                                                                                            
%macro GraphLoop;                                                                                                                           
options mprint;                                                                                                                             
 %do i=1 %to &nvars;                                                                                                                        
   %let variable=%scan(&varlist,&i);                                                                                                        
%DissGraphMakerLogOdds(s26_4,10,&variable,resp);                                                                                            
 %end;                                                                                                                                      
options nomprint;                                                                                                                           
%mend GraphLoop;                                                                                                                            
%GraphLoop;                                                                                                                                 
                                                                                                                                            
%CatToBinWithDrop(s26_4,seqnum,mostyp);                                                                                                     
%CatToBinWithDrop(s26_4,seqnum,MOSHOO);                                                                                                     

data s26_5;
set s26_4;
run;

proc logistic data=s26_5 descending;
model resp=
mostyp_1 mostyp_2 mostyp_3 mostyp_4 mostyp_5 mostyp_6 mostyp_7 mostyp_8 mostyp_9 mostyp_10 mostyp_11 mostyp_12 mostyp_13 mostyp_15 mostyp_16 mostyp_17 mostyp_18 mostyp_19 mostyp_20 mostyp_21 mostyp_22 mostyp_23 mostyp_24 mostyp_25 mostyp_26 mostyp_28 mostyp_29 mostyp_30 mostyp_31 mostyp_32 mostyp_33 mostyp_34 mostyp_35 mostyp_36 mostyp_37 mostyp_38 mostyp_39 mostyp_40 mostyp_41
MSKA2
MOSHOO_1 MOSHOO_2 MOSHOO_3 MOSHOO_4 MOSHOO_5 MOSHOO_6 MOSHOO_7 MOSHOO_8 MOSHOO_9 MOSHOO_10
MHHUUR2
MFALLE2
MSKB22
MGEMLE
MRELGE2
MSKC2
MAUT22
MAUT02
MFWEKI2
MAUT12
MINKGE2
MOPLHO2
MGODRK2
MSKB12
MGEMOM
MAANTH
MGODPR2
PWAPAR2
PPERSA2
PAANHA2
AWAPAR2
APERSA2
AMOTSC2
/selection=stepwise;
output out=scored p=pred;
run;

data holdout;
set  scored;
if   respholdout>.;
run;

proc sort data=holdout;
by descending pred;
run;

data RespAnal (keep=reccount respholdout cumcount cumresp);
set  holdout;
reccount=1;
cumcount+reccount;
cumresp+respholdout;
run;

proc freq data=RespAnal;
tables respholdout;
run;

data RespAnal;
set  RespAnal;
RespPct=cumresp/103;
CountPct=cumcount/1665;
run;


data cutpoint;
set  RespAnal;
lagCountPct=lag(CountPct);
if   CountPct ge .75 and lagCountPct lt .75 then output;
run;

proc print data=cutpoint;
run;

proc reg data=s26_5;
model resp=mostyp_21 MRELGE2 MFWEKI2 MSKB12 MGODPR2 AMOTSC2;
run;
quit;
