
*snp_enhanced moodys_enhanced fitch_enhanced


/*
*S&P
tab snp_enhanced, m
list issuercounty if snp_enhanced == "AAA"
list issuercounty if snp_enhanced == "AA+"
list issuercounty if snp_enhanced == "AA"
list issuercounty if snp_enhanced == "AA-"
list issuercounty if snp_enhanced == "A+"
list issuercounty if snp_enhanced == "A"
list issuercounty if snp_enhanced == "A-"
list issuercounty if snp_enhanced == "BBB+"
list issuercounty if snp_enhanced == "BBB-"

gen snp_dealrate = 0
replace snp_dealrate = 1 if snp_enhanced == "BBB-"
replace snp_dealrate = 1 if snp_enhanced == "BBB+"
replace snp_dealrate = 2 if snp_enhanced == "A-"
replace snp_dealrate = 3 if snp_enhanced == "A"
replace snp_dealrate = 4 if snp_enhanced == "A+"
replace snp_dealrate = 5 if snp_enhanced == "AA-"
replace snp_dealrate = 6 if snp_enhanced == "AA"
replace snp_dealrate = 7 if snp_enhanced == "AA+"
replace snp_dealrate = 8 if snp_enhanced == "AAA"
tab snp_dealrate if snp_dealrate > 0, m
histogram snp_dealrate, discrete

histogram snp_dealrate if snp_dealrate > 0, discrete ///
	graphregion(color(white)) fcolor(blue) ///
	xtitle("1=BBB; ... 8=AAA") ///
	title("S&P Deal Ratings") normal normopts(lcolor(red)) ///
	scheme(economist)
graph export Hist_snpdealratings.png, replace

gen snp_unrated = 0
replace snp_unrated = 1 if snp_enhanced == "NR"
tab snp_unrated, m

gen snpAaa = 0
replace snpAaa = 1 if snp_enhanced == "AAA"
tab snpAaa, m

gen snpAaplus = 0
replace snpAaplus = 1 if snp_enhanced == "AA+"
tab snpAaplus, m

gen snpAa = 0
replace snpAa = 1 if snp_enhanced == "AA"
tab snpAa, m

gen snpAaminus = 0
replace snpAaminus = 1 if snp_enhanced == "AA-"
tab snpAaminus, m

gen snpAplus = 0
replace snpAplus = 1 if snp_enhanced == "A+"
tab snpAplus, m

gen snpA = 0
replace snpA = 1 if snp_enhanced == "A"
tab snpA, m

gen snpAminus = 0
replace snpAminus = 1 if snp_enhanced == "A-"
tab snpAminus, m

gen snpBaa = 0
replace snpBaa = 1 if snp_enhanced == "BBB+"
replace snpBaa = 1 if snp_enhanced == "BBB-"
tab snpBaa, m


*snpAaa snpAaplus snpAa snpAaminus snpAplus snpA snpAminus snpBaa

*Moodys
tab moodys_enhanced, m
list issuercounty if moodys_enhanced == "Aaa"
list issuercounty if moodys_enhanced == "Aa1"
list issuercounty if moodys_enhanced == "Aa2"
list issuercounty if moodys_enhanced == "Aa3"
list issuercounty if moodys_enhanced == "A1"
list issuercounty if moodys_enhanced == "A2"
list issuercounty if moodys_enhanced == "A3"
list issuercounty if moodys_enhanced == "Baa1"

gen moodys_dealrate = 0
replace moodys_dealrate = 1 if moodys_enhanced == "Baa1"
replace moodys_dealrate = 2 if moodys_enhanced == "A3"
replace moodys_dealrate = 3 if moodys_enhanced == "A2"
replace moodys_dealrate = 4 if moodys_enhanced == "A1"
replace moodys_dealrate = 5 if moodys_enhanced == "Aa3"
replace moodys_dealrate = 6 if moodys_enhanced == "Aa2"
replace moodys_dealrate = 7 if moodys_enhanced == "Aa1"
replace moodys_dealrate = 8 if moodys_enhanced == "Aaa"
tab moodys_dealrate if moodys_dealrate > 0, m

histogram moodys_dealrate if moodys_dealrate > 0, discrete ///
	graphregion(color(white)) fcolor(blue) ///
	xtitle("1=Baa; ... 8=Aaa") ///
	title("Moody's Deal Ratings") normal normopts(lcolor(red)) ///
	scheme(economist)
graph export Hist_Mdysdealratings.png, replace

gen moodys_unrated = 0
replace moodys_unrated = 1 if moodys_enhanced == "NR"
tab moodys_unrated, m

*Fitch
tab fitch_enhanced, m
list issuercounty if fitch_enhanced == "Aaa"
list issuercounty if fitch_enhanced == "AA+"
list issuercounty if fitch_enhanced == "AA"
list issuercounty if fitch_enhanced == "AA-"
list issuercounty if fitch_enhanced == "A+"
list issuercounty if fitch_enhanced == "A"
list issuercounty if fitch_enhanced == "A-"
list issuercounty if fitch_enhanced == "BBB-"

gen fitch_dealrate = 0
replace fitch_dealrate = 1 if fitch_enhanced == "BBB-"
replace fitch_dealrate = 2 if fitch_enhanced == "A-"
replace fitch_dealrate = 3 if fitch_enhanced == "A"
replace fitch_dealrate = 4 if fitch_enhanced == "A+"
replace fitch_dealrate = 5 if fitch_enhanced == "AA-"
replace fitch_dealrate = 6 if fitch_enhanced == "AA"
replace fitch_dealrate = 7 if fitch_enhanced == "AA+"
replace fitch_dealrate = 8 if fitch_enhanced == "Aaa"
tab fitch_dealrate if fitch_dealrate > 0, m

histogram fitch_dealrate if fitch_dealrate > 0, discrete ///
	graphregion(color(white)) fcolor(blue) ///
	xtitle("1=BBB; ... 8=AAA") ///
	title("Fitch Deal Ratings") normal normopts(lcolor(red)) ///
	scheme(economist)
graph export Hist_Fitchdealratings.png, replace

gen fitch_unrated = 0
replace fitch_unrated = 1 if fitch_enhanced == "NR"
tab fitch_unrated, m

tab snp_dealrate moodys_dealrate, m
tab snp_dealrate fitch_dealrate, m
tab moodys_dealrate fitch_dealrate, m

*/

*****************Deal level Ratings********************************

tab crate_deal, m

histogram crate_deal if crate_deal > 0, discrete frequency ///
	graphregion(color(white)) fcolor(blue) ///
	xtitle("Deal Credit Ratings=highest score, if multiple") ///
	title("Deal Credit Ratings") ///
	scheme(economist)
graph export Hist_highestdealratings.png, replace













*******************************************************************************
*********************************VARIABLES*************************************

set more off
*total issuance cost
gen lntotissuecost = ln(totissuecost)
histogram lntotissuecost, ///
	graphregion(color(white)) fcolor(blue) ///
	xtitle("ln(total issuance costs)") ///
	title("Total Issuance Cost") normal normopts(lcolor(red)) ///
	scheme(economist)
graph export Hist_lntotissuecost.png, replace


*TIC
histogram tic if tic_yes == 1, ///
	graphregion(color(white)) fcolor(blue) ///
	xtitle("True interest cost(IRR)") ///
	title("Total Issuance Cost") normal normopts(lcolor(red)) ///
	scheme(economist)
graph export Hist_tic.png, replace

* issuer type
tab consoldtedcitycounty, m
tab countygov, m
tab countyother, m

*county size
gen lnpop = ln(total_population)
histogram lnpop, normal

*issue size
gen lnprincipal_amnt = ln(principal_amnt)
histogram lnprincipal_amnt, normal

*years to maturity
histogram matyears, normal

*variable rate
tab varsec, m

*debt tax and payment sources
tab fedtaxable, m
tab source_genfund, m 
tab source_tax, m 
tab source_rev, m 
tab source_other, m


*negot
tab negot, m
list debt_type if negot == 0

*****************

*balance
gen balance = total_revenue-total_expenditure
gen lnbalance = ln(balance)
histogram lnbalance, normal

*unmployment rate
histogram unemp_rate

*median incomes
sum median_income
histogram median_income

*county_rev_per_capita & county_exp_per_capita
histogram county_rev_per_capita 
gen lnrevenuepc = ln(county_rev_per_capita)
histogram lnrevenuepc
histogram county_exp_per_capita
gen lnexpendpc = ln(county_exp_per_capita)
histogram lnexpendpc

*fiscal dependence
sum totalstate totalfederal
histogram totalstate 
histogram totalfederal

gen lntransferstotal = ln(totalstate + totalfederal)
histogram lntransferstotal

*aggregate market value
sum ag_market_value
histogram ag_market_value
gen lnag_market_value = ln(ag_market_value)
histogram lnag_market_value

*debt burden
histogram interestonlongtermdebt_total 
gen lninterest_longtermtotal = ln(interestonlongtermdebt_total)
histogram lninterest_longtermtotal

*debt burden long term
histogram totalliabilities_generallongterm 
gen lnliabilities_longtermgeneral = ln(totalliabilities_generallongterm)
histogram lnliabilities_longtermgeneral

*debt burden short term
histogram currentliabilities_general 
gen lncurrentliabilities_general = ln(currentliabilities_general)
histogram lncurrentliabilities_general

sum lninterest_longtermtotal lnliabilities_longtermgeneral /// 
	lncurrentliabilities_general
corr lninterest_longtermtotal lnliabilities_longtermgeneral /// 
	 lncurrentliabilities_general


*******************************************************************************
*******************CREDIT RATING REGRESSIONS***********************************
corr rtc_mac rtc_mic ptc_mac ptc_mic
corr long_term source_tax source_rev source_other
corr crate_deal crate

sum crate_deal crate rtc_mac mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt lntotissuecost matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2013 fy2014 fy2015 fy2016
	
tab year crate_deal if crate_deal > 0
sum rtc_mac if year == 2012
sum rtc_mac if year == 2016

sum rtc_mac if year == 2017
drop if year == 2017

sum rtc_mac if fy2013 == 1
sum rtc_mac if fy2014 == 1
sum rtc_mac if fy2015 == 1
sum rtc_mac if fy2016 == 1
sum rtc_mac if fy2017 == 1



regress crate_deal rtc_mac rtc_mic mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0
vif 
hettest

regress crate_deal ptc_mac ptc_mic mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0
vif 
hettest


*regressions for retrospective: M1 macro
set more off
ologit crate_deal rtc_mac mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M1crateCatMacro

*regressions for retrospective: M1 micro
set more off
ologit crate_deal rtc_mic mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M1crateCatMicro



*regressions for prospective: M2 macro
set more off
ologit crate_deal ptc_mac mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M2crateCatMacro


*regressions for prospective: M2 micro
set more off
ologit crate_deal ptc_mic mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M2crateCatMicro

esttab M1crateCatMacro M2crateCatMacro M1crateCatMicro M2crateCatMicro, ///
	mtitles(M1crateCat M2crateCat M3crateCat M4crateCat M5crateCat) ///
    aic bic sca(ll) star(* 0.05 ** 0.01 *** 0.001)


*regressions for retrospective and prospective: M3 ALL
set more off
ologit crate_deal rtc_mac rtc_mic ptc_mac ptc_mic mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M3crateCatAll

esttab M1crateCatMacro M2crateCatMacro M1crateCatMicro M2crateCatMicro M3crateCatAll, ///
	mtitles(M1crateCatMacro M2crateCatMacro M1crateCatMicro M2crateCatMicro M3crateCatAll) ///
    aic bic sca(ll) star(* 0.05 ** 0.01 *** 0.001)




	
	

corr crisismanagement security preparedness riskmanagement 

gen resiliencereact = crisismanagement + security
histogram resiliencereact
gen resilienceproact = preparedness + riskmanagement
histogram resilienceproact

corr resiliencereact resilienceproact


*regressions: M6 resiliencereact where crisis management & security/ safety combined
set more off
ologit crate_deal resiliencereact mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M6crateCat

*regressions: M7 resilienceproact where risk management & preparedness combined
set more off
ologit crate_deal resilienceproact mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M7crateCat

esttab M6crateCat M7crateCat, ///
	mtitles(M6crateCat M7crateCat) ///
    aic bic sca(ll) star(* 0.05 ** 0.01 *** 0.001)


***************individual measures***************************

*regressions for crisismanagement: M8
set more off
ologit crate_deal crisismanagement mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M8crateCat


*regressions for security: M9

set more off
ologit crate_deal security mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M9crateCat


*regressions for preparedness: M10

set more off
ologit crate_deal preparedness mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M10crateCat


*regressions for riskmanagement: M11

set more off
ologit crate_deal riskmanagement mrp_ideology_mean ///
	lnbalance lnag_market_value unemp_rate lnpop  ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017 if crate_deal > 0, vce(robust)
eststo M11crateCat

esttab M8crateCat M9crateCat M10crateCat M11crateCat, ///
	mtitles(M8crateCat M9crateCat M10crateCat M11crateCat) ///
    aic bic sca(ll) star(* 0.05 ** 0.01 *** 0.001)
	

sum crate_deal crisismanagement  security resiliencereact /// 
	preparedness riskmanagement resilienceproact ///
	mrp_ideology_mean lnbalance lnag_market_value unemp_rate lnpop ///
	lnprincipal_amnt matyears guaranty_yes ///
	varsec source_tax source_rev source_other callable fedtaxable /// 
	fy2013 fy2014 fy2015 fy2016 fy2017 ///
	if _est_M11crateCat == 1
		
		
		
	

	
/*	
*******************************************************************************
**********************TOTAL COST REGRESSIONS***********************************

*Total interest cost regressions for : rtc_mac rtc_mic ptc_mac ptc_mic

*regressions for rtc_mac rtc_mic
regress lntotissuecost rtc_mac rtc_mic crate_deal ///
	consoldtedcitycounty countyother ///
	lnprincipal_amnt matyears guaranty_yes /// 
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017, robust
eststo M12react
vif

*regressions for ptc_mac ptc_mic
regress lntotissuecost ptc_mac ptc_mic crate_deal ///
	consoldtedcitycounty countyother ///
	lnprincipal_amnt matyears guaranty_yes /// 
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017, robust
eststo M13proact
vif

*regressions for all
regress lntotissuecost rtc_mac rtc_mic ptc_mac ptc_mic crate_deal ///
	consoldtedcitycounty countyother ///
	lnprincipal_amnt matyears guaranty_yes /// 
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017, robust
eststo M14all
vif

*regressions for combined reactive 
regress lntotissuecost resiliencereact crate_deal ///
	consoldtedcitycounty countyother ///
	lnprincipal_amnt matyears guaranty_yes /// 
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017, robust
eststo M15combreact
vif


*regressions for combined proactive 
regress lntotissuecost resilienceproact crate_deal ///
	consoldtedcitycounty countyother ///
	lnprincipal_amnt matyears guaranty_yes /// 
	varsec source_tax source_rev source_other callable fedtaxable ///
	fy2014 fy2015 fy2016 fy2017, robust
eststo M16combproact
vif
*/








							*END OF DO FILE*
*******************************************************************************
*******************************************************************************

***SAVING CHANGES TO DATASET***
save Data_AfterAnalysis_Resilience_InfoContent.dta, replace

*closing the log file that keeps a copy of all output
log close

