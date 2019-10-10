import delimited "data/num.csv", varnames(1) numericcols(5 6 8) clear 
keep adsh tag version ddate coreg qtrs uom value
tempfile num
save "`num'"

import delimited "data/pre.csv", varnames(1) clear
keep adsh stmt tag version
tempfile pre
save "`pre'"

import delimited "data/sub.csv", varnames(1) clear
keep adsh cik name sic countryba period fp fye
drop if countryba != "US"
drop countryba
drop if substr(fp, 1, 1) != "Q"
egen long max_period = max(period), by(cik)
keep if max_period == period
drop max_period

merge 1:n adsh using "`pre'"
drop if _merge == 2
drop _merge

drop if stmt != "IS"
drop if ((tag != "Revenues") & ///
	(tag != "RevenueFromContractWithCustomerExcludingAssessedTax") & ///
	(tag != "RevenueFromContractWithCustomerIncludingAssessedTax"))
drop stmt

duplicates drop
merge 1:n adsh tag version using "`num'"
drop if _merge == 2
drop _merge

drop if ddate != period
drop if value == .
drop if qtrs != 1
drop if coreg != "NA"
drop if uom != "USD"
bysort adsh cik name sic fp fye ddate (value): keep if _n==_N
rename value total_revenue
drop tag version qtrs coreg uom

bysort cik (period): keep if _n==_N

count
