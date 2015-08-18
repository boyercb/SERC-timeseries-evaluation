
clear
clear matrix
set matsize 800
cd "~/Desktop/"
use AllBirths,clear

	
gen statadate = date
format statadate %td
gen mdate = mofd(statadate)
format mdate %tm
sort mdate

gen FaLevel=.
replace FaLevel=. if level=="CHPS"
replace FaLevel=1 if level=="Health Centre/Clinic"
replace FaLevel=2 if level=="Hospital"

gen txgrp = .
replace txgrp = 0 if treatment=="control"
replace txgrp = 1 if treatment=="treatment"

xtset facility mdate
xtreg births i.mdate SERC#FaLevel, re cluster(facility) robust
xtreg births i.mdate time SERC#FaLevel, re cluster(facility) robust
xtscc births i.mdate SERC#FaLevel
xtgls births SERC#FaLevel, panels(hetero) corr(ar1) force
xtreg births i.mdate SERC#t, cluster(facility) robust
estimates store random
xtoverid, cluster(facility)

reg births i.facility SERC
reg y x, robust
reg y x, cluster(firmid)
reg y x, cluster(year)

	
clear
clear matrix
cd "~/Desktop/"
use AllCSRate,clear

gen statadate = date
format statadate %td
gen mdate = mofd(statadate)
format mdate %tm
sort mdate

xtset facility mdate
xtreg csections i.mdate SERC, fe cluster(facility) robust

clear
clear matrix
cd "~/Desktop/"
use AllDeaths,clear

gen statadate = date
format statadate %td
gen mdate = mofd(statadate)
format mdate %tm
sort mdate
gen txgrp = .
replace txgrp = 0 if treatment=="control"
replace txgrp = 1 if treatment=="treatment"

gen lbirths = log(births)


xtset facility mdate
meglm deaths mdate txgrp##policy, family(poisson) link(log) offset(lbirths) vce(cluster facility) eform
meglm deaths births mdate txgrp##policy, family(poisson) link(log) vce(robust) eform

