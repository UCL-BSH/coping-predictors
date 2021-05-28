***********************************************************
********** Coping predictors during lockdown ********
***********************************************************
cd  "/Users/megan.fluharty/Desktop/Covid-19 study/Coping predictors/Data and analyses"
use  "Covid19_Data_20Jul2020.dta", clear

********** Recoding and cleaning **************************

*generate cross-secional complete case sample for outcome (coping)
gen cc_cope=1 if COP_dis!=. & COP_act!=. & COP_den!=. & COP_sub & COP_emo!=. & COP_ins!=. ///
& COP_beh!=. & COP_ven!=. & COP_pos!=. & COP_pla!=. & COP_hum!=. & COP_acp!=. & COP_rel!=. & COP_sbm!=. 
drop if  cc_cope==.

count // 29,882

*merge mplus dervied coping styles
cap drop _merge
merge 1:m record_id using "/Users/megan.fluharty/Desktop/Covid-19 study/Coping trajectories/Data and analyses/mplus_cop.dta"
drop if _merge==2
drop _merge

count //29,882

*keep variables of interest
keep  cc_cope record_id-income smoker- alcohol support housechildren rooms closefriends- socfreq covid smokechange alcoholchange adverse___1-adverse___9 stressorsminor___1-stressorsmajor___17 cop1-cop28 your_experiences_dur_v_0-w1wgt cop_p-cop_s

*Demographic predictors
*ethnicity
recode ethnicity (5=1 White)(1/4 6/9=2 Other), gen(ethnicity_)

*educcational attainment
recode education (1/2=1)(3/4=2)(5=3)(6=4)
label define education 1 "Highest qual/GCSE lower" 2 "A levels or vocaional training" 3 "Undergraduate degree" 4 "Postgraduate degree"
label values education education

*income
sum lowincome
tab lowincome

*home ownership
recode ownership (1/3=1) (4/5=2),
label define ownership 1 "Owned" 2 "Rented/other"
label values ownership ownership

*employment
recode employment (3/5=1) (1/2=2) (6 7 9=3) (8=4)
label define employment 1 "Employed" 2 "School" 3 "Inactive"  4 "Unemployed"
label values employment employment

*Psychosocial predictors
*area
recode area (1/3=1) (4/6=2)
label define area 1 "Urban" 2 "Rural"
label values area area

*Living status (iving alone v living with others not overcrowded v living with others overcrowded)
gen ovcrowd = 1 if alone==1
replace ovcrowd=0 if alone==0 & overcrowd==0
replace ovcrowd=2 if alone==0 & overcrowd==1
label define ovcrowd 1 "Alone" 0 "Others not overecrowded" 2 "Others overecrowded"
label values ovcrowd ovcrowd

*Keyworkers v none
tab keyworker

*Freq socialise
recode  socfreq (1/3=1) (4/5=2)
label define socfreq 1 "Weekly" 2 "< Weekly"
label values socfreq socfreq

*Adversity predictors 
*had/suspected covid
recode covid (1/3=1) (4=0)
label define covid 0 "no" 1 "yes"
label values covid covid

*Type of adversity experianced- financial/ basic needs / covid related
*missing as 0s
foreach x of varlist adverse___1-adverse___9 {
recode `x' (.=0)
}

gen adverse_finance = max(adverse___1, adverse___2, adverse___3, adverse___10)
gen adverse_basic = max(adverse___4, adverse___5, adverse___6)
gen adverse_virus = max(adverse___5, adverse___5)


*Type of worry adversity experiances- financial/ basic needs / covid related
*finance
gen worry_finance = max(stressorsminor___6, stressorsminor___7, stressorsminor___8, ///
stressorsmajor___6, stressorsmajor___7, stressorsmajor___8)
gen worry_basic = max(stressorsminor___9, stressorsminor___10, stressorsminor___11, ///
stressorsmajor___9, stressorsmajor___10, stressorsmajor___11)
gen worry_virus = max(stressorsminor___15, stressorsminor___16, stressorsmajor___15, ///
stressorsmajor___16)

*check missingness
mdesc

*recode sex to missing if other/prefer not to say
recode sex 3=.
**drop covariates if missing or nonresponse 
foreach var of varlist sex lowincome ownership status BFI_n covid lonely nonwhite w1wgt{
drop if `var'==.
}
*restrict to complete case outcome
gen cc=1 if cop_p!=. & cop_e!=. &  cop_a!=. & cop_s!=.
drop if cc==.
count // 26,016 

*keep final dataset
keep age lonely  adverse_finance-adverse_virus worry_finance-worry_virus record_id-employment area living-garden___1 rooms- covid agegrp3 female nonwhite lowincome keyworker ltc_p ltc_m alone withchild status overcrowd carer employed edu BFI_n-BFI_c cop_p- cop_s support lonely w1wgt-worry_virus

save "covid_coping_BMC.dta", replace

********** Analysis **************************

*reorder vars
order record_id redcap_event_name wave date datepx  sex agegrp3 nonwhite education employment ownership lowincome area ovcrowd keyworker ltc_p ltc_m socfreq BFI_n-BFI_c support lonely covid ///
	  adverse_finance adverse_basic adverse_virus worry_finance worry_basic worry_virus

*set sample weight
svyset record_id [pweight= w1wgt]	  
	  

*unweighted & weighted predictors descriptive table
foreach var of varlist sex-lowincome area-lonely covid-worry_virus {
svy: tab `var' 
}
foreach var of varlist BFI_n-BFI_c lonely support {
svy: mean `var' 
}
*unweighted
foreach var of varlist sex-area {
tab `var' 
}

*outcome descriptives
sum cop_* [aw = w1wgt], de

*set covariates
global m1 i.sex i.agegrp3 i.nonwhite i.education i.employment i.ownership i.lowincome
global m2 $m1 i.area i.ovcrowd i.keyworker i.ltc_p i.ltc_m  BFI_n-BFI_c lonely support
global m3 $m2 i.ovcrowd i.keyworker i.ltc_p i.ltc_m  BFI_n-BFI_c lonely support i.covid i.adverse_finance i.adverse_basic i.adverse_virus i.worry_finance i.worry_basic i.worry_virus

*test fit statistiscs
global stat fitstat 
foreach cop of varlist cop_p-cop_s {
regress `cop' $m1
$stat
}
global stat fitstat 
foreach cop of varlist cop_p-cop_s {
regress `cop' $m2
$stat
}
global stat fitstat 
foreach cop of varlist cop_p-cop_s {
regress `cop' $m3
$stat
}

*main weighted analyses
**MODEL ONE SOCIODEMOGRAPHIC
foreach cope of varlist cop_p-cop_s   {
eststo raw: svy: regress `cope' $m1 

eststo: svy: regress `cope'   $m1
est store `cope'
esttab . using "coping_m1_svy.txt", ///
cells("b (fmt(2)) ci_l ci_u p (fmt(3))") stats() modelwidth(20)   /// 
plain nolabel  varwidth (15) nolines nogaps  append   nolabel  ///
mlabels("confounder_adj") collabels("") nocons keep(`exposure')
}

** MODEL TWO PSYCHOSOCIAL
foreach cope of varlist cop_p-cop_s   {
eststo raw: svy: regress `cope'  $m2

eststo: svy: regress `cope'  $m2
est store `cope'
esttab . using "coping_m2_svy.txt", ///
cells("b (fmt(2)) ci_l ci_u p (fmt(3))") stats() modelwidth(20)   /// 
plain nolabel  varwidth (15) nolines nogaps  append   nolabel  ///
mlabels("confounder_adj") collabels("") nocons keep(`exposure')
}

**MODEL THREE ADVERSE
foreach cope of varlist cop_p-cop_s  {
eststo raw: svy: regress `cope' $m3

eststo: svy: regress `cope' $m3
est store `cope'
esttab . using "coping_m3_svy.txt", ///
cells("b (fmt(2)) ci_l ci_u p (fmt(3))") stats() modelwidth(20)   /// 
plain nolabel  varwidth (15) nolines nogaps  append   nolabel  ///
mlabels("confounder_adj") collabels("") nocons keep(`exposure')
}

*unweighted analyses for supplement
**MODEL ONE SOCIODEMOGRAPHIC
foreach cope of varlist cop_p-cop_s   {
eststo raw: regress `cope' $m1 

eststo: regress `cope' `sep'  $m1
est store `cope'
esttab . using "coping_m1_unwt.txt", ///
cells("b (fmt(2)) ci_l ci_u p (fmt(3))") stats() modelwidth(20)   /// 
plain nolabel  varwidth (15) nolines nogaps  append   nolabel  ///
mlabels("confounder_adj") collabels("") nocons keep(`exposure')
}

** MODEL TWO PSYCHOSOCIAL
foreach cope of varlist cop_p-cop_s   {
eststo raw: regress `cope'  $m2

eststo: regress `cope'  $m2
est store `cope'
esttab . using "coping_m2_unwt.txt", ///
cells("b (fmt(2)) ci_l ci_u p (fmt(3))") stats() modelwidth(20)   /// 
plain nolabel  varwidth (15) nolines nogaps  append   nolabel  ///
mlabels("confounder_adj") collabels("") nocons keep(`exposure')
}

**MODEL THREE ADVERSE
foreach cope of varlist cop_p-cop_s  {
eststo raw: regress `cope' $m3

eststo: regress `cope' $m3
est store `cope'
esttab . using "coping_m3_unwt.txt", ///
cells("b (fmt(2)) ci_l ci_u p (fmt(3))") stats() modelwidth(20)   /// 
plain nolabel  varwidth (15) nolines nogaps  append   nolabel  ///
mlabels("confounder_adj") collabels("") nocons keep(`exposure')
}







