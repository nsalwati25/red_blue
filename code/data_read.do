
/*******************************
*MASTER FILE FOR READING IN DATA
********************************
	This file produces the levels by month for the following datasets:
	-ELECTION RESULTS
	-CPS
	-BEA
	-OPPORTUNITY INSIGHTS
	-ZILLOW 
	-VACCINATION DATA 
********************************/
*SET DIRECTORY TO redblue folder 
global redblue "C:\Users\nsalwati\Downloads\red_blue"

		global data_raw "${redblue}/data-raw"
		global data "${redblue}/data"
		global output "${redblue}/output"
		global code "${redblue}/code"

/*******************
*CPS DATA BY STATE AND MSA 
Source:IPUMS
*******************/
clear all 
cd  "${data_raw}/cps"
/*
do cps_00045.do
do "${code}/statefip_to_stateabb.do"
save cps_raw, replace 
*/
use "cps_raw", clear 
replace age = 80 if age>80
*get ageshares 
drop if age<16 | empstat == 0 | empstat == 1 
keep if year == 2019
tab age, gen(agedum)
collapse agedum* [pw=wtfinl], by(sex stateabb)

reshape long agedum , i(stateabb sex) j(age)
replace age = age+15
save shares_temp, replace 

use "cps_raw", clear 
replace age = 80 if age>80
*get ageshares 
drop if age<16 | empstat == 0 | empstat == 1 
keep if year == 2019
gen fage = wtfinl if sex==2
gen mage = wtfinl if sex==1
egen totf = sum(fage),by(stateabb)
egen totm = sum(mage),by(stateabb)
gen femsh = totf/(totm+totf)
collapse femsh, by(age sex stateabb)
merge m:1 age sex stateabb using shares_temp
rm shares_temp.dta
drop _merge
replace agedum = agedum*femsh if sex==2
replace agedum = agedum*(1-femsh) if sex==1
drop femsh
rename agedum agsx_share
gen male = (sex == 1)
save "${data}/cps_agemale_shares", replace

***education weights by age
use "cps_raw", clear 
replace age = 80 if age>80
*get ageshares 
drop if age<16 | empstat == 0 | empstat == 1 
keep if year == 2019
tab age, gen(agedum)
gen educ_c = (educ >= 111)
collapse agedum* [pw=wtfinl], by(educ_c stateabb)

reshape long agedum , i(stateabb educ_c) j(age)
replace age = age+15
save shares_temp, replace 

use "cps_raw", clear 
replace age = 80 if age>80
*get ageshares 
drop if age<16 | empstat == 0 | empstat == 1 
keep if year == 2019
gen educ_c = (educ >= 111)
gen fage = wtfinl if educ_c==1
gen mage = wtfinl if educ_c==0
egen totf = sum(fage),by(stateabb)
egen totm = sum(mage),by(stateabb)
gen femsh = totf/(totm+totf)
collapse femsh, by(age educ_c stateabb)
merge m:1 age educ_c stateabb using shares_temp
rm shares_temp.dta
drop _merge
replace agedum = agedum*femsh if educ_c==1
replace agedum = agedum*(1-femsh) if educ_c==0
drop femsh
rename agedum agsx_share
save "${data}/cps_ageeduc_c_shares", replace



***
*Define and collapse
***
foreach lvl in educ_c male{
	cd "${data_raw}/cps"
use cps_raw, clear 
replace age = 80 if age>80
*drop below 16, armed forces and missing empstat  
drop if age<16 | empstat == 0 | empstat == 1 
gen newtime = year+(month-1)/12

gen emp = (empstat == 10 | empstat == 12)
gen inlf = (labforce == 2)
gen unemp = (emp == 0 & inlf == 1)

gen male = (sex == 1)
gen educ_c = (educ >= 111)
gen remwork = (covidtelew == 2)


preserve 
collapse emp inlf remwork unemp year month[pw=wtfinl],by(age `lvl' newtime stateabb)
save "${data}/cps_collapse", replace 
restore 

preserve 
collapse emp inlf remwork unemp year month[pw=wtfinl],by(newtime stateabb)
save "${data}/cps_collapse_unw", replace 
restore 

****
*age weigh
****
cd "${data}"
use "cps_collapse", clear
merge m:1 stateabb `lvl' age using cps_age`lvl'_shares

foreach v in emp inlf remwork unemp{
	gen temp = `v'*agsx_share
	egen `v'_w = sum(temp),by(newtime stateabb)
	drop temp*
}

forval y = 16(5)80{
	local z = `y'+4
	*generate dummies for each age group 
	gen _`y'`z' = (age>=`y' & age<=`z')
	
		*get the total age share for the broad age group
		gen temp = agsx_share if _`y'`z' == 1
		egen agsx_`y'`z' = sum(temp),by(newtime stateabb) 
		drop temp*
		
		*get the weighted variables for each group 
		foreach v in emp inlf unemp remwork{
			gen temp = `v'*agsx_share if _`y'`z' == 1
			egen temp0 = sum(temp),by(newtime stateabb)
			gen `v'_`y'`z' = temp0/agsx_`y'`z'
			drop temp*
		}
			
			
	*generate dummies for each age group by sex
	gen _`y'`z'0`lvl' = (age>=`y' & age<=`z' & `lvl' == 0) 
	gen _`y'`z'1`lvl' = (age>=`y' & age<=`z' & `lvl' == 1) 
	
	foreach gnd in 0`lvl' 1`lvl'{
	*get the total age share for the broad age group
		gen temp = agsx_share if _`y'`z'`gnd' == 1
		egen agsx_`y'`z'`gnd' = sum(temp),by(newtime stateabb) 
		drop temp*
		
		*get the weighted variables for each group 
		foreach v in emp inlf unemp remwork{
			gen temp = `v'*agsx_share if _`y'`z'`gnd' == 1
			egen temp0 = sum(temp),by(newtime stateabb)
			gen `v'_`y'`z'`gnd' = temp0/agsx_`y'`z'`gnd'
			drop temp*
		}
}
}


collapse emp_* inlf_* unemp_* remwork_* year month,by(newtime stateabb)
merge 1:1 newtime stateabb using cps_collapse_unw

drop _merge 
rename emp emp_uw
rename inlf inlf_uw
rename unemp unemp_uw 
rename remwork remwork_uw

forval y = 16(5)80{
	local z = `y'+4
	foreach v in `y'`z'{
		gen ur_`v' = 1 - emp_`v'/inlf_`v'
		gen epop_`v' = emp_`v'
		gen lfp_`v' = inlf_`v'
		gen remw_`v' = remwork_`v'
		
}

foreach v in `y'`z'0`lvl' `y'`z'1`lvl'{
		gen ur_`v' = 1 - emp_`v'/inlf_`v'
		gen epop_`v' = emp_`v'
		gen lfp_`v' = inlf_`v'
		gen remw_`v' = remwork_`v'
		
}
}

	foreach v in w uw{
		gen ur_`v' = 1 - emp_`v'/inlf_`v'
		gen epop_`v' = emp_`v'
		gen lfp_`v' = inlf_`v'
		gen remw_`v' = remwork_`v'
}


keep epop* lfp* ur* remw_* year month newtime stateabb
rename stateabb stateabbrev

save `lvl', replace
}

*now doing remote work**********************
	cd "${data_raw}/cps"
use cps_raw, clear 
replace age = 80 if age>80
*drop below 16, armed forces and missing empstat  
drop if age<16 | empstat == 0 | empstat == 1 
gen newtime = year+(month-1)/12

gen emp = (empstat == 10 | empstat == 12)
gen inlf = (labforce == 2)
gen unemp = (emp == 0 & inlf == 1)

gen remwork = (covidtelew == 2)
gen male = (sex == 1)

preserve 
collapse emp inlf unemp year month[pw=wtfinl],by(age male remwork newtime stateabb)
save "${data}/cps_collapse", replace 
restore 

****
*age weigh
****
cd "${data}"
use "cps_collapse", clear
merge m:1 stateabb age male using cps_agemale_shares

foreach v in emp inlf unemp{
	gen temp = `v'*agsx_share if remwork == 1
	egen `v'_w_1remwork = sum(temp),by(newtime stateabb)
	drop temp*
	
	gen temp = `v'*agsx_share if remwork == 0
	egen `v'_w_0remwork = sum(temp),by(newtime stateabb)
	drop temp*
}

forval y = 16(5)80{
	local z = `y'+4
	*generate dummies for each age group 
	gen _`y'`z' = (age>=`y' & age<=`z')
	
		*get the total age share for the broad age group
		gen temp = agsx_share if _`y'`z' == 1
		egen agsx_`y'`z' = sum(temp),by(newtime stateabb) 
		drop temp*
		
		*get the weighted variables for each group 
		foreach v in emp inlf unemp{
			gen temp = `v'*agsx_share if _`y'`z' == 1
			egen temp0 = sum(temp),by(newtime stateabb)
			gen `v'_`y'`z' = temp0/agsx_`y'`z'
			drop temp*
		}
			
			
	*generate dummies for each age group by sex
	gen _`y'`z'0male_0remwork = (age>=`y' & age<=`z' & male == 0 & remwork == 0) 
	gen _`y'`z'0male_1remwork = (age>=`y' & age<=`z' & male == 0 & remwork == 1)  
	
	gen _`y'`z'1male_0remwork = (age>=`y' & age<=`z' & male == 1 & remwork == 0) 
	gen _`y'`z'1male_1remwork = (age>=`y' & age<=`z' & male == 1 & remwork == 1)  
	
	
	foreach gnd in 0male_0remwork 0male_1remwork 1male_0remwork 1male_1remwork{
	*get the total age share for the broad age group
		gen temp = agsx_share if _`y'`z'`gnd' == 1
		egen agsx_`y'`z'`gnd' = sum(temp),by(newtime stateabb) 
		drop temp*
		
		*get the weighted variables for each group 
		foreach v in emp inlf unemp{
			gen temp = `v'*agsx_share if _`y'`z'`gnd' == 1
			egen temp0 = sum(temp),by(newtime stateabb)
			gen `v'_`y'`z'`gnd' = temp0/agsx_`y'`z'`gnd'
			drop temp*
		}
}
}


collapse emp_* inlf_* unemp_* year month,by(newtime stateabb)

forval y = 16(5)80{
	local z = `y'+4

foreach v in `y'`z'0male_0remwork `y'`z'0male_1remwork `y'`z'1male_0remwork `y'`z'1male_1remwork{
		gen ur_`v' = 1 - emp_`v'/inlf_`v'
		gen epop_`v' = emp_`v'
		gen lfp_`v' = inlf_`v'
		
}
}

	foreach v in w_1remwork w_0remwork{
		gen ur_`v' = 1 - emp_`v'/inlf_`v'
		gen epop_`v' = emp_`v'
		gen lfp_`v' = inlf_`v'
}


keep epop* lfp* ur* year month newtime stateabb
rename stateabb stateabbrev

save remwork, replace

*****merging everything********
cd "${data}"
use male, clear 
merge 1:1 newtime stateabbrev using educ_c 
drop _merge

merge 1:1 newtime stateabbrev using remwork
drop _merge 

drop if missing(newtime)
save cps_merged, replace 

*save cps, replace 
rm cps_agesex_shares.dta 
rm cps_collapse.dta
rm cps_collapse_unw.dta 


/**************************************************
*OPPORTUNITY INSIGHTS 
Source:GITHUB, EconomicTracker: https://github.com/OpportunityInsights/EconomicTracker
**************************************************/
clear all 
cd  "${data_raw}/oi"

*READ OI CSV AND SAVE AS DTA
foreach dataset in geoids affinity covid mobility ui womply zearn geoids_county{
    import delimited "oi_`dataset'", clear 
	save "`dataset'", replace 
} 

foreach dataset in affinity covid mobility ui womply zearn{
    use `dataset', clear 
	merge m:1 statefips using geoids 
	drop _merge 
	drop if year == 2020 & month<2 
	save oi_`dataset', replace 
	rm `dataset'.dta
}


use oi_affinity, clear 
foreach dataset in covid mobility womply ui zearn{
merge m:m month year statefips using oi_`dataset'
drop _merge 
rm oi_`dataset'.dta
}
rm oi_affinity.dta
drop if year == 2023 & month>1
collapse spend* case_rate death_rate fullvaccine_rate gps* merchants* revenue* badges* engagement* initclaims_rate*,by(year month statefips stateabbrev)
save "${data}/oi", replace 

********************************
*STATE POPULATIONS
*source: https://fred.stlouisfed.org/release/tables?rid=118&eid=259194&od=2019-01-01#
********************************
cd "${data_raw}/fred"
import excel using state_pop, firstrow clear 
rename State statename
do "${code}/statename_to_stateabbrev.do"
reshape long pop_, i(statename) j(year)
rename pop_ pop
save "${data}/pop.dta", replace 


**************************
*BEA PCE DATA
*Source: SAPCE3 Personal consumption expenditures (PCE) by state by type of product 1/
**************************
clear all 
cd "${data_raw}/bea"

*geoids for merging 
	import delimited "Table", clear 
	drop if missing(v2) 
	rename v1 statefips 
	rename v2 region
	rename v3 linecode 
	rename v4 desc 
	rename v5 pce2018
	rename v6 pce2019
	rename v7 pce2020 
	rename v8 pce2021
	rename v9 desc1
	drop if _n==1
	
	foreach v in pce2018 pce2019 pce2020 pce2021{
		replace `v' = "." if `v' == "(L)"
	}
	
	foreach v in statefips linecode pce2018 pce2019 pce2020 pce2021{
		destring `v', replace 
	}
	replace statefips=statefips/1000
	keep if statefips>0 & statefips<60
	
	reshape long pce,i(statefips linecode) j(year) string 
	destring year, replace 
	drop if linecode == 46
	sort linecode 
	egen lc = group(linecode)
	drop linecode 
	rename lc linecode 
	sort linecode 
	gen flag = .
	foreach v in 000000000{
		replace flag = 1 if strpos(desc1, "`v'")>0
	}
	drop if flag == 1
	save bea_long, replace 
	
	*******************
	use bea_long, clear 
	collapse linecode flag,by(desc desc1)
	tostring flag, replace 
	foreach v in 00 000 0000 00000 000000 0000000 0000000{
		replace flag = "`v'" if strpos(desc1, "`v'")>0
	}
	
	gen newcat = "."
	replace newcat = "Durable goods" if linecode>2 & linecode<25
	replace newcat = "Non durable goods" if linecode>=25 & linecode<46
	replace newcat = "Household services" if linecode>=46 & linecode<110
	replace newcat = "NPISH" if linecode>=110
	replace newcat = desc if flag == "00" | flag == "0"
	replace newcat = "Headline PCE" if newcat == "."
	drop desc1 flag 
	save bea_code, replace 
	************************
	
	use bea_long, clear 
	drop desc*
	reshape wide pce,i(statefips year) j(linecode)
	
	keep pce* statefip year 
	merge m:1 statefips using "${data_raw}/oi/geoids"
	drop _merge 
	
	*now merge yearly population data
	merge m:1 stateabbrev year using "${data}/pop"
	drop if _merge!=3 
	drop _merge
	
	preserve 
		drop state* year
		ds 
		restore 
		local varlist `r(varlist)'
		
		
	foreach i of local varlist{ 
	   replace `i' = (`i'/pop)*10^3
	   
	   gen psh_`i' = `i'/pce1
	}
	
	sort statefips year 
	save "${data}/bea", replace 



/**************************************************
*Percapita real personal income 
SARPI Real personal income and real personal Consumption Expenditures (PCE) by state
Real per capita personal income (Constant 2012 dollars)
State or DC
**************************************************/

clear all 
cd "${data_raw}/bea"

*geoids for merging 
	import excel using real_personal_income, firstrow clear
	do "${code}/statename_to_stateabbrev.do"
	reshape long pi_, i(statename) j(year)
	rename pi pce_pi
		save "${data}/bea_income", replace 
		
/**************************************************
*Percapita real personal consumption expenditures
SARPI Real personal income and real personal Consumption Expenditures (PCE) by state
Real per capita personal income (Constant 2012 dollars)
State or DC
**************************************************/

clear all 
cd "${data_raw}/bea"

*geoids for merging 
	import excel using real_pce, firstrow clear
	do "${code}/statename_to_stateabbrev.do"
	reshape long pce_, i(statename) j(year)
	rename pce pce_real
		save "${data}/bea_pce_real", replace 

/**************************************************
*ZILLOW  
Source:https://www.zillow.com/research/data/
**************************************************/

clear all 
cd "${data_raw}/zillow"

*geoids for merging 
	import delimited "County_zori_sm_month.csv", clear 
	gen s = "."
	forval i = 1/9{
		replace s = "0`i'" if statecodefips == `i'
	}
	forval i = 10/99{
		replace s = "`i'" if statecodefips == `i'
	}
	
	
	gen c = "."
	forval i = 1/9{
		replace c = "00`i'" if municipalcodefips == `i'
	}
	forval i = 10/99{
		replace c = "0`i'" if municipalcodefips == `i'
	}
	forval i = 100/999{
		replace c = "`i'" if municipalcodefips == `i'
	}
	
	gen countyfips = s + c
	
reshape long v, i(countyfips) j(time)
sort time 
*series starts march 2015
gen year = 2015 if time >=10 & time<=19
replace year = 2016 if time >=20 & time<=31 
replace year = 2017 if time >=32 & time<=43 
replace year = 2018 if time >=44 & time<=55 
replace year = 2019 if time >=56 & time<=67
replace year = 2020 if time >=68 & time<=79
replace year = 2021 if time >=80 & time<=91 
replace year = 2022 if time >=92 & time<=103
replace year = 2023 if time >=104

sort countyfips year time 
by countyfips year: gen month = _n
replace month = month+2 if year == 2015 
destring countyfips, replace 


merge m:1 countyfips using "${data_raw}/oi/geoids_county"
drop if _merge == 2
tab state if _merge!=3 

rename v rental_index 
foreach v in rental_index{
    gen `v'_pop = `v'*county_pop2019
}

collapse (sum) *_pop*,by(stateabbrev year month)
foreach v in rental_index{
    gen `v' = `v'_pop/county_pop2019
}
drop *pop*

merge m:1 stateabbrev using "${data_raw}/oi/geoids"
drop _merge 

gen newtime = year + (month-1)/12

save "${data}/zillow", replace 


/**************************************************
*VACCINATION DATA 
Source:https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc
**************************************************/
clear all 
cd "${data_raw}/cdc"

import excel "vaccination_rates", sheet("raw") firstrow clear 

gen month = month(Date)
gen year = year(Date)

forval i = 1/10{
	disp "----------`i'------------"
gen flag = 1 if strpos(Location, "`i'")>0
drop if flag == 1 
drop flag
}
drop if Location == "LTC"

rename Location statecode 

rename Series_Complete_Pop_Pct vaxrate_all 
rename Series_Complete_5PlusPop_Pct vaxrate_5p
rename Series_Complete_12PlusPop_Pct vaxrate_12p
rename Series_Complete_18PlusPop_Pct vaxrate_18p 
rename Series_Complete_65PlusPop_Pct vaxrate_65p 

rename Series_Complete_Yes vax_all 
rename Series_Complete_5Plus vax_5p
rename Series_Complete_12Plus vax_12p
rename Series_Complete_18Plus vax_18p 
rename Series_Complete_65Plus vax_65p 

save temp,replace 
use temp, clear 

foreach v in all 5p 12p 18p 65p{
	replace vaxrate_`v' = 0 if missing(vaxrate_`v')
}

collapse(max) vaxrate* vax_*,by(statecode month year)

drop if year == 2020 & month<2

rename statecode stateabbrev
merge m:1 stateabbrev using "${data_raw}/oi/geoids"
drop if _merge !=3
drop _merge 


save "${data}/vax", replace 


/**************************************************
*ELECTION DATA BY STATE AND MSA 
https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX
***************************************/
clear all
cd "${data_raw}/mit"
	import delimited "election_historical.csv", clear
	rename state_po statecode 
	rename year election_year 
	drop version office party_detailed
	rename party_simplified party

gen party_new = "blue" if party == "DEMOCRAT"
replace party_new = "red" if party == "REPUBLICAN"
replace party_new = "other" if missing(party_new)
keep state statecode candidate party_new candidatevotes totalvotes election_year 
rename party_new party
collapse(sum) candidatevotes,by(party statecode election_year)

 egen totalvotes = sum(candidatevotes),by(statecode election_year)
 gen share_ = candidatevotes/totalvotes

 drop candidatevotes totalvotes

reshape wide share_*,i(statecode election_year) j(party) string

rename statecode stateabbrev
merge m:1 stateabbrev using "${data_raw}/oi/geoids"
drop _merge 

sort stateabbrev
keep if election_year>2010
reshape wide share_blue share_red share_other,i(stateabbrev) j(election_year)
save "${data}/election", replace 



/**************************************************
*DEMOGRAPHICS FROM CENSUS 
https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-detail.html#par_textimage_673542126
***************************************/
cd "${data_raw}/census"
clear all
import delimited sc-est2019-alldata5, clear 

gen black=(race == 2)
gen black_pop = popestimate2019 if black ==1 

gen sixtyfivep =(age>64)
gen sixtyfivep_pop =popestimate2019 if sixtyfivep==1

collapse(sum) black_pop sixtyfivep_pop popestimate2019,by(state)

foreach v in black sixtyfivep{
	gen `v'_share19 = `v'_pop/popestimate2019
}
rename state statefips 
keep state* *share19

save "${data}/demographics", replace 


/******************************************
SERVICES SHARE FROM BEA
https://apps.bea.gov/iTable/?reqid=70&step=1&acrdn=4#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyNCwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCIzMyJdLFsiQ2xhc3NpZmljYXRpb24iLCJOQUlDUyJdLFsiTWFqb3JfQXJlYSIsIjEwIl0sWyJTdGF0ZSIsWyIxMCJdXSxbIkFyZWEiLFsiWFgiXV0sWyJTdGF0aXN0aWMiLFsiLTEiXV0sWyJVbml0X29mX21lYXN1cmUiLCJMZXZlbHMiXSxbIlllYXIiLFsiMjAxOSJdXSxbIlllYXJCZWdpbiIsIi0xIl0sWyJZZWFyX0VuZCIsIi0xIl1dfQ==
*******************************************/
cd "${data_raw}/bea"
clear all
import excel using "Table (3)", firstrow clear
rename CAEMP25NTotalfulltimeandpar geofips 
rename B geoname 
rename C linecode
rename D desc 
rename E employees
drop F 
gen lc = -1
forval i = 1/3000{
	replace lc = `i' if linecode == "`i'"
}
drop if lc<70
drop if lc == 70
gen s = ((lc >= 800 & lc<=1900))
gen hosp = (lc == 1800)
gen totnf = (lc == 80)
keep if totnf == 1 | s == 1 | hosp == 1
drop if geofips == "00998" | geofips == "00999"
destring geofips, replace 
gen statename= substr(geoname, 1, strpos(geoname, " (")-1)
do "${code}/statename_to_stateabbrev.do"

replace employees = "0" if employees == "(D)"
gen employees_new = subinstr(employees, "E", "", .)
replace employees_new = subinstr(employees_new, ",", "", .)
replace employees_new = subinstr(employees_new, "D", "", .)


foreach v in totnf s hosp{
	gen `v'_emp = `v'*employees_new
	destring `v'_emp, replace 
}

collapse(sum) *_emp,by(stateabbrev)

rename totnf total_nonfarm
rename s_ services 
rename hos acc_food

foreach v in services acc_food{
	gen `v'_share = `v'/total_nonfarm
}
drop if length(stateabbrev) != 2

save "${data}/employment_share", replace 


*********************************
*SET DIRECTORY TO redblue folder 
global redblue "C:\Users\nsalwati\Downloads\red_blue"

		global data_raw "${redblue}/data-raw"
		global data "${redblue}/data"
		global output "${redblue}/output"
		global code "${redblue}/code"

		
*********************************
*MERGE ALL TO CREATE MASTER DATA SET
*********************************
cd "${data}"
use election, clear 

merge 1:m stateabbrev using cps_merged
rename _merge cps_merge  
drop if year<2016 //can bring back if needed 

merge m:1 stateabbrev year using pop 
rename _merge pop_merge 

merge m:1 stateabbrev year using bea //only have 2018, 2019, 2020, 2021
rename _merge bea_merge 
drop if year<2016 

merge m:1 stateabbrev year using bea_income //only have 2018, 2019, 2020, 2021
rename _merge bea_income_merge 
drop if year<2016 

merge m:1 stateabbrev year using bea_pce_real //only have 2018, 2019, 2020, 2021
rename _merge bea_pce_merge 
drop if year<2016 

gen consumption_share = pce_real/pce_pi

foreach df in oi zillow vax{
	merge 1:1 stateabbrev year month using `df'
	drop if year<2016
	rename _merge `df'_merge
	disp "----------------`df'---------------" //oi is missing pre-2020 data, vax only goes up to oct 2022
}

*state controls
merge m:m statefips using demographics
drop _merge 
*oil state 
gen oil_state = (stateabbrev == "TX")
foreach v in ND NM OK CO AK{
	replace oil_state = 1 if stateabbrev=="`v'"
}

*employment share
merge m:1 stateabbrev using employment_share
drop _merge 
 
keep state* share* year month newtime epop* lfp* ur* pce* spend* gps* remw* psh* merchants* revenue* vax* fullvaccine* case_rate* consumption_share death_rate* rental_index* black_share19 oil_state total_nonfarm services acc_food services_share acc_food_share sixtyfivep_share19

egen final_vax = max(vaxrate_all),by(stateabbrev)

save "${output}/merged_data", replace 