capture log close
clear

*******************************************************************************

log using output/Output_DealRatings.log, replace
use "input/Data_Clean_DealRatings.dta", clear

***********************DEAL CREDIT RATINGS*************************************

*regressions for crisismanagement: M5
set more off
ologit crate_deal crisismanagement mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M5crateCat

*regressions for security: M6

set more off
ologit crate_deal security mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M6crateCat

*regressions for preparedness: M7

set more off
ologit crate_deal preparedness mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M7crateCat

*regressions for riskmanagement: M8

set more off
ologit crate_deal riskmanagement mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M8crateCat


*Table 4. Ordered Logistic Models for the Covariates of County Deal Level Credit Ratings.

esttab M5crateCat M6crateCat M7crateCat M8crateCat, ///
	mtitles(M5CrisisMngmt M6Security M7Prepared M8RiskMangmnt) ///
    aic bic sca(ll) p star(* 0.05 ** 0.01 *** 0.001)




*regressions for M9 resiliencereact Static Resilience where crisis management & security/ safety combined

set more off
ologit crate_deal resiliencereact mrp_ideology_mean ///
	lnbalance unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M9crateCat

*regressions for 10 resilienceproact Dynamic Resilience where risk management & preparedness combined

set more off
ologit crate_deal resilienceproact mrp_ideology_mean ///
	lnbalance unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M10crateCat


*Table 5. Ordered Logistic Models for the Covariates of County Deal Level ///
*	Credit Ratings, Collapsed Resilience Signal Measures.

esttab M9crateCat M10crateCat, ///
	mtitles(M9crateCat M10crateCat) ///
    aic bic sca(ll) p star(* 0.05 ** 0.01 *** 0.001)



*****************Deal level Ratings********************************

histogram crate_deal if crate_deal > 0, discrete frequency ///
	graphregion(color(white)) fcolor(blue) ///
	xtitle("Deal Credit Ratings=highest score, if multiple") ///
	title("Deal Credit Ratings") ///
	scheme(economist)
graph export Hist_highestdealratings.png, replace



*Table 2. Descriptive Statistics: Deal Ratings Part

sum crate_deal crisismanagement  security resiliencereact ///
	preparedness riskmanagement resilienceproact ///
	mrp_ideology_mean lnbalance unemp_rate lnpop ///
	lnprincipal_amnt matyears guaranty_yes ///
	source_tax source_rev source_other callable fedtaxable ///
	fy2013 fy2014 fy2015 fy2016 fy2017 ///
	if _est_M5crateCat == 1


*close and keep the log file
log close
