*===============================================================================
****-  Energy transition in Europen Uion, the impact of circular economy under the moderating role of financial structure and circular carbon technology innovation. New evidence from latent structure model
Khadim Hussain 1*, Zhong Jian 1**, 
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

log close

************************************************************************************************************************************************************

clear all
cd "F:\Research 2023-24\CE ET Social justice oct 2024\data"
use data.dta
xtset id year
*Visualizing Panel Data
panelview et ccus reit greenfinance fins urp, i(id) t(year) type(missing)
// clear all
// cd "F:\CE ET Social justice oct 2024\data"
// use data.dta
// xtset id year
*Missing data
mdesc
ipolate et year, gen(ET) epolate by (id)
ipolate fins year,gen(Fins) epolate by (id)

// gen et2= (gereation-ET)/installed
// ipolate et2 year,gen(ET2) epolate by (id)

**# Bookmark #1
*To eliminate potential heteroskedasticity and data fluctuations, we apply natural logarithm 
// The proposed score and ranks are based "oprobit frontier CIE GRS PC SRM Wm" (Model_1) and arthematic mean is used against the proposed GM because data has negative value after ipolataion
**According to Elif 2024, Using ,"proposedCE_rank" (ce_rank) is a better option to rank alternatives than "proposedCE_frontier_rank" (ce_frontier_rank) . Because it accounts for the existence of non-domination between alternatives. (an alternative dominates another alternative in one dimension but is dominated in another dimension by it). "proposedCE_rank" ranks the alternatives regarding their scores "proposedET_score". Here ce_rank2 and ce_rank are the proposed rank
// the proposed score are based "ologit frontier CIE ln_GRS ln_PC SRM Wm" (Model_2) and arthematic mean is used against the proposed GM because data has negative value after ipolataion, Only taken ln for the postive diemsnsions
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
// gen LnCO2=ln(co2)
// replace LnCO2 = 0 if missing(LnCO2)
// gen LnCO22=ln(co22)
// replace LnCO22 = 0 if missing(LnCO22)

**# Unitroot test #1
**Levin-Lin-Chu test


    Levin-Lin-Chu test

        xtunitroot llc varname [if] [in] [, LLC_options]


    Harris-Tzavalis test

        xtunitroot ht varname [if] [in] [, HT_options]


    Breitung test

        xtunitroot breitung varname [if] [in] [, Breitung_options]


    Im-Pesaran-Shin test

        xtunitroot ips varname [if] [in] [, IPS_options]


    Fisher-type tests (combining p-values)

        xtunitroot fisher varname [if] [in], {dfuller | pperron} lags(#) [Fisher_options]


    Hadri Lagrange multiplier stationarity test

        xtunitroot hadri varname [if] [in] [, Hadri_options]



xtunitroot llc LnCE12
xtunitroot llc LnURP
xtunitroot llc LnGDP
xtunitroot llc LnGrFin
xtunitroot llc LnFins
xtunitroot llc LnREIT
xtunitroot llc LnCCUS

xtunitroot llc d.LnCE12
xtunitroot llc d.LnURP
xtunitroot llc d.LnGDP
xtunitroot llc d.LnGrFin
xtunitroot llc d.LnFins
xtunitroot llc d.LnREIT
xtunitroot llc d.LnCCUS

*Harris-Tzavalis test
xtunitroot ht LnCE12
xtunitroot ht LnURP
xtunitroot ht LnGDP
xtunitroot ht LnGrFin
xtunitroot ht LnFins
xtunitroot ht LnREIT
xtunitroot ht LnCCUS

xtunitroot ht d.LnCE12
xtunitroot ht d.LnURP
xtunitroot ht d.LnGDP
xtunitroot ht d.LnGrFin
xtunitroot ht d.LnFins
xtunitroot ht d.LnREIT
xtunitroot ht d.LnCCUS

*    Breitung test
xtunitroot breitung LnCE12
xtunitroot breitung LnURP
xtunitroot breitung LnGDP
xtunitroot breitung LnGrFin
xtunitroot breitung LnFins
xtunitroot breitung LnREIT
xtunitroot breitung LnCCUS

xtunitroot breitung d.LnCE12
xtunitroot breitung d.LnURP
xtunitroot breitung d.LnGDP
xtunitroot breitung d.LnGrFin
xtunitroot breitung d.LnFins
xtunitroot breitung d.LnREIT
xtunitroot breitung d.LnCCUS

*Im-Pesaran-Shin test
xtunitroot ips LnCE12
xtunitroot ips LnURP
xtunitroot ips LnGDP
xtunitroot ips LnGrFin
xtunitroot ips LnFins
xtunitroot ips LnREIT
xtunitroot ips LnCCUS

xtunitroot ips d.LnCE12
xtunitroot ips d.LnURP
xtunitroot ips d.LnGDP
xtunitroot ips d.LnGrFin
xtunitroot ips d.LnFins
xtunitroot ips d.LnREIT
xtunitroot ips d.LnCCUS
* Fisher-type tests (combining p-values)
xtunitroot fisher LnCE12, dfuller lags(1)
xtunitroot fisher LnURP, dfuller lags(1)
xtunitroot fisher LnGDP, dfuller lags(1)
xtunitroot fisher LnGrFin, dfuller lags(1)
xtunitroot fisher LnFins, dfuller lags(1)
xtunitroot fisher LnREIT, dfuller lags(1)
xtunitroot fisher LnCCUS, dfuller lags(1)

xtunitroot fisher d.LnCE12, dfuller lags(1)
xtunitroot fisher d.LnURP, dfuller lags(1)
xtunitroot fisher d.LnGDP, dfuller lags(1)
xtunitroot fisher d.LnGrFin, dfuller lags(1)
xtunitroot fisher d.LnFins, dfuller lags(1)
xtunitroot fisher d.LnREIT, dfuller lags(1)
xtunitroot fisher d.LnCCUS, dfuller lags(1)

*Hadri Lagrange multiplier stationarity test
xtunitroot hadri LnCE12
xtunitroot hadri LnURP
xtunitroot hadri LnGDP
xtunitroot hadri LnGrFin
xtunitroot hadri LnFins
xtunitroot hadri LnREIT
xtunitroot hadri LnCCUS

xtunitroot hadri d.LnCE12
xtunitroot hadri d.LnURP
xtunitroot hadri d.LnGDP
xtunitroot hadri d.LnGrFin
xtunitroot hadri d.LnFins
xtunitroot hadri d.LnREIT
xtunitroot hadri d.LnCCUS

***********************
cd "F:\Research 2023-24\CE ET Social justice oct 2024\data"
putexcel A1 = matrix(panel_results), names_sheet("UnitRootTests") modify file("F:\Research 2023-24\CE ET Social justice oct 2024\data\UnitRootTests.xlsx")
putdocx begin
putdocx table tbl1 = matrix(panel_results)
putdocx save "C:\path\to\your\desired\directory\UnitRootTests.docx", replace

cd "F:\Research 2023-24\CE ET Social justice oct 2024\data" // Set the working directory

program define panel_unitroot_tests, rclass
    // Create an empty matrix to store results
    mata: panel_results = J(0, 16, .)
    
    // Define variables for the tests
    local varlist LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS
    
    foreach var of varlist {
        // Perform LLC test and collect the result
        xtunitroot llc `var'
        local llc_stat = r(z)
        local llc_pval = r(p)
        
        // Perform HT test
        xtunitroot ht `var'
        local ht_stat = r(z)
        local ht_pval = r(p)
        
        // Perform Breitung test
        xtunitroot breitung `var'
        local breitung_stat = r(z)
        local breitung_pval = r(p)
        
        // Perform IPS test
        xtunitroot ips `var'
        local ips_stat = r(z)
        local ips_pval = r(p)
        
        // Perform Fisher test
        xtunitroot fisher `var', dfuller lags(1)
        local fisher_stat = r(p)
        local fisher_pval = r(p)
        
        // Perform Hadri test
        xtunitroot hadri `var'
        local hadri_stat = r(z)
        local hadri_pval = r(p)
        
        // Store results in mata matrix (16 columns: variable, test stats, p-values)
        mata: panel_results = panel_results \ (`"`var'"', `llc_stat', `llc_pval', `ht_stat', `ht_pval', `breitung_stat', `breitung_pval', `ips_stat', `ips_pval', `fisher_stat', `fisher_pval', `hadri_stat', `hadri_pval')
    }

    // Save matrix to Excel with explicit file path
    putexcel A1 = matrix(panel_results), names sheet("UnitRootTests") modify file(
	"F:\Research 2023-24\CE ET Social justice oct 2024\data\UnitRootTests.xlsx")

    // Or Save matrix to Word with explicit file path
    // putdocx begin
    // putdocx table tbl1 = matrix(panel_results)
    // putdocx save "C:\path\to\your\desired\directory\UnitRootTests.docx", replace

end




*Model Specification tests
xtreg LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1,  fe
estimates store fixed
xtreg LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS,  re
*Test the appropriateness of the random-effects estimator (xtreg, re)
hausman fixed ., sigmamore

xtreg LnET LnCE22 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS,  fe
estimates store fixed2
xtreg LnET LnCE22 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS,  re
*Test the appropriateness of the random-effects estimator (xtreg, re)
hausman fixed2 ., sigmamore


*===============================================================================
****-Step 2 : TableA# Descriptive statistical results    -****
*===============================================================================

*--//Table A2 - Descriptive statistical//--*
sum LnET LnCE22 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS

logout,save(Table2-Summary) word dec(3) replace: tabstat LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS, stat(count mean sd min max) col(stat) format(%10.2f)


*--//Table A3 - Correlation matrix among variables//--*
*if the correlation coefficients are less than 0.8, indicating that there is no serious collinearity

* Calculate the correlation matrix
pwcorr LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS, star(1) 

********** Shapiro-Wilk and Shapiro-Francia tests for normality
swilk LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS

**Shapiro-Francia normality test

sfrancia LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnCO2 LnCO22

**Skewness and kurtosis test for normality
sktest  LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnCO2 LnCO22

 histogram LnCE, frequency kdensity kdenopts(lcolor(blue) lwidth(vthin) lpattern(solid) gaussian) scheme(economist)
 
//Table 3: 
*WE implements the classifier-lasso method (Su, Shi, and Phillips, 2016, Econometrica
//84: 2215–2264) to simultaneously identify and estimate unobserved parameter
//heterogeneity in panel-data models using penalized techniques

** Table 


log using CE, replace
reghdfe LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id year) 
est store a1 
classifylasso LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, group(1/5) rho(0.2) dynamic optmaxiter(300)
estimates save ce12
log close


// Figure
estimates use ce12.ster
set scheme sj
classogroup, export("fig1.pdf")
classocoef LnCE12, export("fig2.pdf")


**# Table. : Robustness analysis

***Step1. With different estiamtors
log using robustness, replace
qreg LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, quantile(.30) vce(iid, kernel(parzen) chamberlain)
est store a1
qreg LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, quantile(.75) vce(iid, kernel(parzen) chamberlain)
est store a2
sqreg LnET LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, quantile(.20 ) reps(100)
est store a3
esttab a1 a2 a3  using Table-Robust_QREG.rtf, replace b(%6.3f) t(%6.2f) star(* 0.10 ** 0.05 *** 0.01) nogap
log close

qreg LnET LnWaste LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, quantile(.30) vce(iid, kernel(parzen) chamberlain)



***   With casuality test Dumitrescu and Hurlin 2012 casuality test
**xtgcause y x, lag(1)
xtgcause LnET LnCE12, lag(1)
xtgcause LnET LnURP, lag(1)
xtgcause LnET LnGDP, lag(1)
xtgcause LnET LnGrFin,lag(1)
xtgcause LnET LnFins, lag(1)
xtgcause LnET LnREIT 
xtgcause LnET LnCCUS

xtgcause LnET LnWaste, lag(1)

**BC casuality test 
bcgcausality


// bcgcausality lnco2 lnnrec lncons lnmfg lngdp, varlag(3) condtype(geweke)
// bcgcausality lnco2 lncons lnnrec  lnmfg lngdp, varlag(3) condtype(geweke)
// bcgcausality lnco2 lnmfg lncons lnnrec   lngdp, varlag(3) condtype(geweke)
// bcgcausality lnco2 lngdp lnmfg lncons lnnrec   , varlag(3) condtype(geweke)

*--//Table  - Moderating role of financial instrastrure and circular carbon technology
log using moderating_roles, replace
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
reghdfe LnET LnCE12 CE12_CCUS LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id)
est store p2
classifylasso LnET LnCE12 CE12_CCUS LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, group(1/5) rho(0.2) dynamic optmaxiter(300)
estimates save CE_CCUS
estimates replay,outreg2("ccusLnET.xls")

esttab p?, b(%6.4f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab p? using moderatingRoles_Pools.rtf, replace b(%6.4f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap
log close
***With common Equation
reghdfe LnET LnCE12 CE12_Fins CE12_CCUS LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id)
classifylasso LnET LnCE12 CE12_CCUS CE12_Fins LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, group(1/5) rho(0.2) dynamic optmaxiter(300)
estimates save CE_Finscommon
estimates replay,outreg2("ccusLnET_common.xls")

// Figure
estimates use Fins.ster
set scheme sj
classogroup, export("fins1.pdf")
classocoef CE12_Fins, export("fins2.pdf")

estimates use CCUS.ster
set scheme sj
classogroup, export("ccus1.pdf")
classocoef CE12_CCUS, export("ccus2.pdf")


classogroup, export("selection2.eps")
classogroup, export("selection2.pdf")
classocoef Circulareconomy, export("coefce.eps")
estimates use ssp2016_2
classoselect, group(3) postselection


estimates use CE121.rtf
set scheme sj
classogroup, export("classifylasso3.pdf")
classocoef CE12, export("classifylasso4.pdf")


esttab b?, b(%6.3f) t(%6.2f) r2 ar2 star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
esttab b? using Table4-Pooled.rtf, replace b(%6.3f) t(%6.2f) r2 ar2 star(* 0.1 ** 0.05 *** 0.01) nogap





// est store h1
// set scheme sj
// classogroup, export("selection.eps")
// classogroup, export("selection.pdf")
// classocoef , export("coefce.eps")
// classocoef CE12, export("coefce.pdf")
// classocoef, export("coefce.xls")
// estimates save CE122, replace

estimates replay
estimates use CE.ster
set scheme sj
estimates replay

******************************************************************************************************            Heterogeneity analysis: different regions

log using Regional_heterogenity_analysis, replace

// *For Eastern EU
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) dynamic optmaxiter(300) if regionid==1
// estimates save Eastern1
// *For Northern Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) dynamic optmaxiter(300) if regionid==2
// estimates save Northern1
// *For Southern Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) dynamic optmaxiter(300) if regionid==3
// estimates save Southern1
// *For Western Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) dynamic optmaxiter(300) if regionid==4
// estimates save Western1
//
// *******************with year and country fixed effect
// // a(country year) cluster(country)
// // a(id year) cluster(id)
//
// *For Eastern EU
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(id year) cluster(id)dynamic optmaxiter(300) if regionid==1
// estimates save Eastern2
// *For Northern Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(id year) cluster(id)dynamic optmaxiter(300) if regionid==2
// estimates save Northern2
// *For Southern Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(id year) cluster(id)dynamic optmaxiter(300) if regionid==3
// estimates save Southern2
// *For Western Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(id year) cluster(id)dynamic optmaxiter(300) if regionid==4
// estimates save Western2
// log close
// **************************************************************************************************
// ******with country fixed effect
// log using Regional_heterogenity_analysis, append
// *For Eastern EU
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(id) dynamic optmaxiter(300) if regionid==1
// estimates save Eastern3
// *For Northern Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(id ) dynamic optmaxiter(300) if regionid==2
// estimates save Northern3
// *For Southern Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(id) dynamic optmaxiter(300) if regionid==3
// estimates save Southern3
// *For Western Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(id) 
// estimates save Western3
//
//
// ******with year fixed effect
//
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(year) dynamic optmaxiter(300) if regionid==1
// estimates save Eastern4
// *For Northern Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(year) dynamic optmaxiter(300) if regionid==2
// estimates save Northern4
// *For Southern Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(year) dynamic optmaxiter(300) if regionid==3
// estimates save Southern4
// *For Western Europe
// classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) a(year) cluster(id)dynamic optmaxiter(300) if regionid==4
// estimates save Western4
**# Bookmark #4

**************using demo code version
// classifylasso savings lagsavings cpi interest gdp, group(1/5) lambda(1.5485) tol(1e-4) dynamic 

classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) roup(1/5) lambda(1.5485) tol(1e-4) dynamic  if regionid==1
estimates save Eastern5
*For Northern Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , roup(1/5) lambda(1.5485) tol(1e-4) dynamic  if regionid==2
estimates save Northern5
*For Southern Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , roup(1/5) lambda(1.5485) tol(1e-4) dynamic  if regionid==3
estimates save Southern5
*For Western Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , roup(1/5) lambda(1.5485) tol(1e-4) dynamic  if regionid==4
estimates save Western5
log close

log using Regional_heterogenity_analysis, append
**# Bookmark #3
****WITH BOTH YEAR AND COUNTRY FIXED EFFECT WITH DEMO CODE
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , roup(1/5) lambda(1.5485) tol(1e-4) a(id) dynamic if regionid==2
estimates save Eastern7
*For Northern Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , roup(1/5) lambda(1.5485) tol(1e-4) a(id) dynamic if regionid==2
estimates save Northern7
*For Southern Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , roup(1/5) lambda(1.5485) tol(1e-4) a(id)  dynamic  if regionid==3
estimates save Southern7
*For Western Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , roup(1/5) lambda(1.5485) tol(1e-4) a(id)  dynamic  if regionid==4
estimates save Western7

***************Heterogeneity analysis: country characteristics (Energy import 2014)

*For Eastern EU

classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) dynamic optmaxiter(300) if regionid==1
estimates save Eastern1
*For Northern Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) dynamic optmaxiter(300) if regionid==2
estimates save Northern1
*For Southern Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) dynamic optmaxiter(300) if regionid==3
estimates save Southern1
*For Western Europe
classifylasso log_ET2 LnCE12 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , group(1/5) rho(0.2) dynamic optmaxiter(300) if regionid==4
estimates save Western1
log close


****************************************************************************************************************************************************************************************************************************************
*Dimensions index

clear all
cd "F:\Research 2023-24\CE ET Social justice oct 2024\data"
use dimensions.dta
xtset c_id year


sum cie011_2 cie012_2 cie020_2
correlate cie011_2 cie012_2 cie020_2

factor cie011_2 cie012_2 cie020_2,pcf
estat kmo
paran cie011_2 cie012_2 cie020_2
factor cie011_2 cie012_2 cie020_2,pcf factor(1)
rotate,normalize
predict d1

sum grs010_2 grs011_2 grs030_2
correlate grs010_2 grs011_2 grs030_2

factor grs010_2 grs011_2 grs030_2,pcf
estat kmo
paran grs010_2 grs011_2 grs030_2
factor grs010_2 grs011_2 grs030_2,pcf factor(2)
rotate,normalize
predict d2


sum pc020_2 pc030_2 pc031_2 pc040_2 pc050_2
correlate pc020_2 pc030_2 pc031_2 pc040_2 pc050_2

factor pc020_2 pc030_2 pc031_2 pc040_2 pc050_2,pcf
estat kmo
paran pc020_2 pc030_2 pc031_2 pc040_2 pc050_2
factor pc020_2 pc030_2 pc031_2 pc040_2 pc050_2,pcf factor(2)
rotate,normalize
predict d3

sum srm030_2 srm020_2
correlate srm030_2 srm020_2
factor srm030_2 srm020_2,pcf
estat kmo
paran srm030_2 srm020_2
factor srm030_2 srm020_2,pcf factor(1)
rotate,normalize
predict d4

sum wm011_2 wm020_2 wm060_2
correlate wm011_2 wm020_2 wm060_2
factor wm011_2 wm020_2 wm060_2,pcf
estat kmo
paran wm011_2 wm020_2 wm060_2
factor wm011_2 wm020_2 wm060_2,pcf factor(1)
rotate,normalize
predict d5



sum d1 d2 d3 d4 d5 
correlate d1 d2 d3 d4 d5 

factor d1 d2 d3 d4 d5 ,pcf
estat kmo
paran d1 d2 d3 d4 d5 
factor d1 d2 d3 d4 d5 ,pcf factor(1)
rotate,normalize
predict pca_ce
**# Bookmark #1
reghdfe LnET pca_ce LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id year) 

reghdfe LnET d1 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id year) 
est store a1 
reghdfe LnET d2 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id year) 
est store a2
reghdfe LnET d3 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id year) 
est store a3
reghdfe LnET d4 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id year)
est store a4 
reghdfe LnET d5 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS LnET1, a(id year) 
est store a5
**# sys_GMM #2

xtabond2 LnET pca_ce LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , gmm(LnET, l(1 2))  twostep robust 
**# IV-GMM #3

ivreg2 LnCED LnYPC LnCF_M LnTR LnUPS LnIN ( LnDE =l.LnDE ), gmm2s first robust
ivreg2 LnET pca_ce LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS  ( LnDE =l.LnDE ), gmm2s first robust

// xtabond2 log_ET log_AI_stock log_URH log_GDP log_FDI log_DE, gmm(log_ET, l(1 2))  twostep robust 
// est store e1
// xtabond2 log_ET L1.log_ET log_AI_stock log_URH log_GDP log_FDI log_DE, gmm(log_ET, l(1 2))  twostep robust 
// est store e2
// xtabond2 log_ET L2.log_ET log_AI_stock log_URH log_GDP log_FDI log_DE, gmm(log_ET, l(1 2))  twostep robust 
// est store e3
// xtabond2 log_ET L3.log_ET log_AI_stock log_URH log_GDP log_FDI log_DE, gmm(log_ET, l(1 2))  twostep robust 
// est store e4
// esttab e?, b(%6.4f) t(%6.2f) stats(ar2p hansenp N,fmt(3 3 0)) order(log_AI_stock L1.log_ET L2.log_ET L3.log_ET) star(* 0.10 ** 0.05 *** 0.01) obslast compress nogap 
// esttab e? using Table6-Robust1.rtf, replace b(%6.4f) t(%6.2f) stats(ar2p hansenp N,fmt(3 3 0)) order(log_AI_stock L1.log_ET L2.log_ET L3.log_ET) star(* 0.1 ** 0.05 *** 0.01) nogap


xtabond2 LnET pca_ce LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , gmm(LnET, l(1 2))  twostep robust

xtabond2 LnET d1 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , gmm(LnET, l(1 2))  twostep robust
xtabond2 LnET d2 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , gmm(LnET, l(1 2))  twostep robust
xtabond2 LnET d3 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , gmm(LnET, l(1 2))  twostep robust
xtabond2 LnET d4 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , gmm(LnET, l(1 2))  twostep robust
xtabond2 LnET d5 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS , gmm(LnET, l(1 2))  twostep robust



xtabond2 LnET L1.LnET pca_ce LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS, gmm(LnET, l(1 2))  twostep robust 

xtabond2 LnET L1.LnET d1 LnURP LnGDP LnGrFin LnFins LnREIT LnCCUS, gmm(LnET, l(1 2))  twostep robust 




