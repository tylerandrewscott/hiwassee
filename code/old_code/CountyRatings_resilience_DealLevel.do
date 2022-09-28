*Get data from Github: https://raw.githubusercontent.com/tylerandrewscott/hiwassee/main/input/Data_Clean_DealRatings_V2.txt


* Generate the dummy vars for source 
gen source_genfund =.
gen source_tax =.
gen source_rev =.
gen source_other =.

replace source_genfund = 1 if sourcerepayment =="General Fund of Lessee"
replace source_genfund = 1 if sourcerepayment =="General Fund of issuing jurisdiction"
replace source_genfund = 0 if source_genfund !=1

replace source_tax =1 if sourcerepayment =="Property tax revenues"
replace source_tax =1 if sourcerepayment =="Sales tax revenues"
replace source_tax =0 if source_tax !=1

replace source_rev =1 if sourcerepayment =="Public enterprise revenues"
replace source_rev =1 if sourcerepayment =="Special assessments"
replace source_rev =0 if source_rev !=1

replace source_other =1 if sourcerepayment =="Bond proceeds"
replace source_other =1 if sourcerepayment =="Private obligor payments"
replace source_other =1 if sourcerepayment =="Tax increment"
replace source_other =0 if source_other !=1


* Create deal level credit rating variables
* I did not code the short term ratings unless they were combined with a long-term rating

gen snp_dealrate = 0 
replace snp_dealrate = 1 if snp == "S:BBB-"
replace snp_dealrate = 1 if snp == "S:BBB+"
replace snp_dealrate = 2 if snp == "S:A-"
replace snp_dealrate = 2 if snp == "S:A/A-"
replace snp_dealrate = 3 if snp == "S:A/A-1"
replace snp_dealrate = 3 if snp == "S:A"
replace snp_dealrate = 4 if snp == "S:A+"
replace snp_dealrate = 4 if snp == "S:A+/A-1"
replace snp_dealrate = 5 if snp == "S:AA-"
replace snp_dealrate = 5 if snp == "S:AA-/A"
replace snp_dealrate = 5 if snp == "S:AA-/BBB-"
replace snp_dealrate = 5 if snp == "S:AA-/BBB+"
replace snp_dealrate = 6 if snp == "S:AA"
replace snp_dealrate = 6 if snp == "S:AA/A"
replace snp_dealrate = 6 if snp == "S:AA/A+"
replace snp_dealrate = 6 if snp == "S:AA/A-"
replace snp_dealrate = 7 if snp == "S:AA+"
replace snp_dealrate = 7 if snp == "S:AA+/A-1"
replace snp_dealrate = 8 if snp == "S:AAA"

* SnP Short-Term Credit Rating Issues are rated A-1 (highest), A-2, A-3, B, C, D
* SnP Short-Term Credit Notes are rated SP-1+, SP-1, SP-2, SP-3

gen moodys_dealrate = 0
replace moodys_dealrate = 1 if moodys == "M:Baa1"
replace moodys_dealrate = 2 if moodys == "M:A3"
replace moodys_dealrate = 2 if moodys == "M:A3/Baa1"
replace moodys_dealrate = 3 if moodys == "M:A2"
replace moodys_dealrate = 4 if moodys == "M:A1"
replace moodys_dealrate = 4 if moodys == "M:A1/VMIG-1"
replace moodys_dealrate = 5 if moodys == "M:Aa3"
replace moodys_dealrate = 6 if moodys == "M:Aa2"
replace moodys_dealrate = 6 if moodys == "M:Aa2/VMIG1"
replace moodys_dealrate = 7 if moodys == "M:Aa1"
replace moodys_dealrate = 8 if moodys == "M:Aaa"

* Moody's short-term credit rating are MIG 1 (highest), MIG 2, MIG 3, SG
* Variable Rate Demand Obligations are rated VMIG 1, VMIG 2, VMIG 3, SG
* Short-Term Insurance Financial Strength Ratings are opinions of the ability of the insurance company to repay punctually its short-term senior policyholder claims and obligations and they are rated P-1, P-2, P-3, NP
* See Moody's rating definitions: https://www.moodys.com/sites/products/productattachments/moodys%20rating%20symbols%20and%20definitions.pdf

gen fitch_dealrate =0
replace fitch_dealrate = 1 if fitch == "F:BBB-"
replace fitch_dealrate = 2 if fitch == "F:A-"
replace fitch_dealrate = 3 if fitch == "F:A"
replace fitch_dealrate = 4 if fitch == "F:A+"
replace fitch_dealrate = 5 if fitch == "F:AA-"
replace fitch_dealrate = 6 if fitch == "F:AA"
replace fitch_dealrate = 6 if fitch == "F:AA/F1"
replace fitch_dealrate = 6 if fitch == "F:AA/F1+"
replace fitch_dealrate = 7 if fitch == "F:AA+"
replace fitch_dealrate = 8 if fitch == "F:AAA" 
replace fitch_dealrate = 8 if fitch == "F:AAA/F1+" 

*Fitch's short term ratings are F1 (highest), F2, F3, B, C, RD, Data_Clean_DealRatings_V2
* See Fitch rating definitions: https://www.fitchratings.com/research/structured-finance/rating-definitions-21-03-2022

*Deal level CRATE = Max credit rating from the three 
egen crate_deal = rowmax(snp_dealrate moodys_dealrate fitch_dealrate)

*Other Deal Level Variables
* matyears --> matyears = finalmaturity - saledate
* Need to fix the finalmaturity dates


* guaranty_yes --> guaranty
gen guaranty_yes =0
replace guaranty_yes =1 if guaranty!=""

* varsec --> (variable rate) interesttype
gen varsec =0
replace varsec =1 if interesttype =="VAR"

* callable --> calldate
gen callable =0
replace callable =1 if calldate!=""

* fedtaxable --> federallytaxable
gen fedtaxable = 0
replace fedtaxable =1 if federallytaxable =="Federally Taxable"
