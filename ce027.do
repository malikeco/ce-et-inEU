*===============================================================================
****-  Circular economy and EU’s energy transition: The moderating and transitioning effects of financial structure and circular carbon technology innovation: Evidence from C-Lasso and PSTR approaches.
Khadim Hussain 1*, Zhong Jian, and Anwar Khan, 
****
*===============================================================================
*===============================================================================
****-  Step 1 : Install packges required to perform the estimation  -****
*===============================================================================
// ssc install reghdfe
// ssc install ftools
// ssc install classifylasso

*===============================================================================
****-  Step 2 : load data and set variables  -****
*===============================================================================


*Data path
global data "F:\Research 2023-24\CE ET Social justice oct 2024\data"
global results "F:\Research 2023-24\CE ET Social justice oct 2024\results2"

*Table 2 Descriptive statistics and A4_Covariance Matrix  
log using "${results2}\Table2-.smcl", replace
use "${data}\data.dta", clear
xtset id year
*Missing data
mdesc
ipolate et year, gen(ET) epolate by (id)
ipolate fins year,gen(Fins) epolate by (id)
gen LnCE12=ln(ce_rank)
gen LnET=ln(ET)
replace LnET=0 if missing(LnET)
gen LnFins=ln(Fins)
gen LnGrFin=ln(greenfinance)
replace LnGrFin = 0 if missing(LnGrFin)
gen LnGDP=ln(gdp)
gen LnURP=ln(urp)
gen LnREIT = ln(reit)
replace LnREIT=0 if missing(LnREIT)
gen LnCCUS=ln(ccus)
replace LnCCUS=0 if missing(LnCCUS)

sum LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS
**# Table A4 #1
pwcorr LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS, star(1) 
log close
**************************************Table 3 (Approximately  23min17s are required.)**********************************
log using "${results2}\Table3_Heterogenous_effects_of_CE_onET.smcl", replace

reghdfe LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id year) 
est store a1 
classifylasso LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, group(1/5) rho(0.2) dynamic optmaxiter(300)
estimates save ce12
estimates replay,outreg2("LnCE.xls")
classogroup, export("fig3.1.eps")
classocoef LnCE12, export("fig3.2.eps")
esttab a?, b(%6.4f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab a? using Table3_Pools.rtf, replace b(%6.4f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap
log close

**************************************Table 4 (Approximately  1.5 hours are required.)**********************************
log using "${results2}\Table4_Moderating effects_of_Fins&CCUS.smcl", replace
egen ave_LnCE12=mean(LnCE12)
gen dc_LnCE12=LnCE12-ave_LnCE12
egen ave_CCUS=mean(LnCCUS)
gen dc_CCUS=LnCCUS-ave_CCUS
gen CE12_CCUS=dc_LnCE12*dc_CCUS
egen ave_LnFins=mean(LnFins)
gen dc_LnFins=LnFins-ave_LnFins
gen CE12_Fins=dc_LnCE12*dc_LnFins

reghdfe LnET LnCE12 CE12_Fins LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id)
est store p1
classifylasso LnET LnCE12 CE12_Fins LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, group(1/5) rho(0.2) dynamic optmaxiter(300)
estimates save CE_Fins
estimates replay,outreg2("finsLnET.xls")
classogroup, export("fig4.1.eps")
classocoef CE12_Fins, export("fig4.2.pdf")
reghdfe LnET LnCE12 CE12_CCUS LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id)
est store p2
classifylasso LnET LnCE12 CE12_CCUS LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, group(1/5) rho(0.2) dynamic optmaxiter(300)
estimates save CE_CCUS
estimates replay,outreg2("ccusLnET.xls")
classogroup, export("fig4.3.eps")
classocoef CE12_CCUS, export("fig4.4.eps")
esttab p?, b(%6.4f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab p? using moderatingRoles_Pools.rtf, replace b(%6.4f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap
log close

**************************************Table 5 Analysis of Robustness (Approximately  5min are required.)**********************************
log using "${results2}\Table5_robustness.different_estimators.smcl", replace
qreg LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, quantile(.30) vce(iid, kernel(parzen) chamberlain)
est store a1
qreg LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, quantile(.75) vce(iid, kernel(parzen) chamberlain)
est store a2
sqreg LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, quantile(.20 ) reps(100)
est store a3
esttab a1 a2 a3  using Table-Robust_QREG.rtf, replace b(%6.3f) t(%6.2f) star(* 0.10 ** 0.05 *** 0.01) nogap
**************************************Table 7 and Figure 6-Effect of Policy change (Approximately  50min are required.)**********************************
gen COVID = (year >= 2019) // this will genrate a new data with 0 and 1 for beofre and after COVID-19
reghdfe LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1 if COVID==0 , a(id year) 
est store d1 
reghdfe LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1 if COVID==1 , a(id year) 
est store d2
classifylasso LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1 if COVID==0, group(1/5) rho(0.2) dynamic optmaxiter(300)
estimates save beCOVID
estimates replay,outreg2("beCOVID.xls")
classogroup, export("Fig6_1.png") // can save file in pnd, eps, or pdf
classocoef LnCE12, export("Fig6_2.png")
classifylasso LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1 if COVID==1, group(1/5) rho(0.2) dynamic optmaxiter(300)
estimates save afterCOVID
estimates replay,outreg2("afterCOVID.xls")
classogroup, export("Fig6_3.eps")
classocoef LnCE12, export("Fig6_4.eps")
esttab d?, b(%6.4f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab d? using Table7.rtf, replace b(%6.4f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap


log close

************************************************************************************************************************************************************










