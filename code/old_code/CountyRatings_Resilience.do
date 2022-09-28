capture log close
clear

*******************************************************************************

log using output/Output_CountyRatings.log, replace
use "input/Data_Clean_CountyRatings.dta", clear
xtset cfips year

***********************County CREDIT RATINGS***********************************

*regressions for crisismanagement: M1
***do several robustness checks by swapping fiscal measures

set more off
xtologit crate crisismanagement mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)
eststo M1crateOlmRated

/*
set more off
xtologit crate crisismanagement mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)

set more off
xtologit crate crisismanagement mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lninterest_longtermtotal if crate > 0, nolog vce(robust)

set more off
xtreg crate crisismanagement mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, vce(robust)
eststo M1crateLinRated
*/


*regressions for security: M2
***do several robustness checks by swapping fiscal measures

set more off
xtologit crate security mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)
eststo M2crateOlmRated


/*

set more off
xtologit crate security mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)

set more off
xtologit crate security mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lninterest_longtermtotal if crate > 0, nolog vce(robust)

set more off
xtreg crate security mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, vce(robust)
eststo M2crateLinRated

*/


*regressions for preparedness: M3
***do several robustness checks by swapping fiscal measures

set more off
xtologit crate preparedness mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)
eststo M3crateOlmRated


/*

set more off
xtologit crate preparedness mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)

set more off
xtologit crate preparedness mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lninterest_longtermtotal if crate > 0, nolog vce(robust)

set more off
xtreg crate preparedness mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, vce(robust)
eststo M3crateLinRated

*/


*regressions for riskmanagement: M4
***do several robustness checks by swapping fiscal measures

set more off
xtologit crate riskmanagement mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)
eststo M4crateOlmRated

/*

set more off
xtologit crate riskmanagement mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)

set more off
xtologit crate riskmanagement mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lninterest_longtermtotal if crate > 0, nolog vce(robust)

set more off
xtreg crate riskmanagement mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, vce(robust)
eststo M4crateLinRated

*/


* Table 3: Ordered Logistic Models for the Covariates of Underlying County Credit Ratings.

esttab M1crateOlmRated M2crateOlmRated M3crateOlmRated M4crateOlmRated, ///
	mtitles(M1CrisisMangmnt M2Security M3Prepared M4RiskMangmnt) ///
    aic bic sca(ll) p star(* 0.05 ** 0.01 *** 0.001)



*regressions for resiliencereact Static Resilience: Ma1
***do several robustness checks by swapping fiscal measures

set more off
xtologit crate resiliencereact mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)
eststo Ma1crateOlmRated

/*
set more off
xtologit crate resiliencereact mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)

set more off
xtologit crate resiliencereact mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lninterest_longtermtotal if crate > 0, nolog vce(robust)

set more off
xtreg crate resiliencereact mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, vce(robust)
eststo Ma1crateLinRated

set more off
xtreg crate resiliencereact mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, fe vce(robust)
eststo Ma1crateLinFE
estat ic

*/


*regressions for resilienceproact Dynamic Resilience: Ma2
***do several robustness checks by swapping fiscal measures

set more off
xtologit crate resilienceproact mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)
eststo Ma2crateOlmRated

/*

set more off
xtologit crate resilienceproact mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, nolog vce(robust)

set more off
xtologit crate resilienceproact mrp_ideology_mean ///
	lnrevenuepc lnag_market_value ///
	median_income unemp_rate lnpop ///
	lninterest_longtermtotal if crate > 0, nolog vce(robust)

set more off
xtreg crate resilienceproact mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, vce(robust)
eststo Ma2crateLinRated
return list

set more off
xtreg crate resilienceproact mrp_ideology_mean ///
	lntransferstotal lnag_market_value ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral if crate > 0, fe vce(robust)
eststo Ma2crateLinFE
estat ic

*/


*Table A1. Ordered Logistic Panel Models for the Covariates of Underlying
* 	County Credit Ratings, Collapsed Measurement of Signals of Resilience.

esttab Ma1crateOlmRated Ma2crateOlmRated, ///
	mtitles(Ma1StaticRes Ma2DynamicRes) ///
    aic bic sca(ll) p star(* 0.05 ** 0.01 *** 0.001)

*Figure A2. Binary Distribution between Collapsed Measures of Signals of Resilience

twoway (lfit resiliencereact resilienceproact, lwidth(thin)) ///
	(scatter resiliencereact resilienceproact, ///
	mcolor(blue) msymbol(oh) mlabsize(tiny) mlabcolor(black)), ///
	graphregion(color(white)) ///
	ytitle("Reactive Resilience") ///
	xtitle("Proactive Resilience") ///
	title("Reactive vs. Proactive Resilience") ///
	scheme(economist) legend(off)
graph export TwoWay_React_Proact_Resilience.pdf, replace


*Table 2. Descriptive Statistics: County Ratings Part

sum crate crisismanagement  security resiliencereact ///
	preparedness riskmanagement resilienceproact ///
	mrp_ideology_mean ///
	lntransferstotal lnag_market_value lnbalance lnrevenuepc lnexpendpc ///
	median_income unemp_rate lnpop ///
	lnliabilities_longtermgeneral lninterest_longtermtotal ///
	if _est_Ma1crateOlmRated == 1

*close and keep the log file
log close
