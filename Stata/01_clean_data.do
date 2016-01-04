/*-------------------------------*
 |file:    01_clean_data.do      |
 |project: SERC timeseries       |
 |author:  christopher boyer     |
 |date:    26 oct 2015           |
 *-------------------------------*
  description:
    this file merges and cleans the raw DHIMs data files into a master .dta file
*/

clear
version 13

// change to raw data directory (global must be defined by 00_master.do)
cd "${rawdata}"

// <====================== local definitions ======================> //

// set name and location of cleaned data set
local dtafile "${cleandata}/DHIMS_data.dta"

// copy contents of raw data directory into local
local files: dir "${rawdata}" files "*.csv"

// erase the existing dtafile if present
capture confirm file "`dtafile'"
if _rc == 0 {
    erase "`dtafile'"
}

// <================== loop and clean .csv files ==================> //

// loop through files, import, and append to .dta file
foreach file in `files' {
    // import file
    import delimited using `file', clear
	
	// generate new variables
	generate district = strproper(subinstr(regexr("`file'", ".csv", ""), "_", " ", .))
	generate hospital = regexm(organisationunit, "Hospital")
	generate chps = regexm(organisationunit, "CHPS")
	generate clinic = !(hospital | chps)
	generate treatment = inlist(district, "Bongo", "Builsa North", "Builsa South")
	generate date = monthly(period, "MY")
	format date %tm

	drop period
	generate period = date >= monthly("2013-08", "YM")
	
	// rename existing variables
	#delimit ;
	rename ( organisationunit 
	         mothersalldeliveries 
			 totalmaternaldeaths 
			 caesareansectiondeliveries )
	       ( facility 
		     deliveries 
			 maternal_deaths 
			 csections );
    #delimit cr
	
	// keep and order variables
    keep date                          ///
		 facility                      ///
		 district                      ///
		 treatment                     ///
		 period                        ///
		 chps                          ///
		 clinic                        ///
		 hospital                      ///
	     deliveries                    ///
		 maternal_deaths               ///
	     csections
	
	// drop blank/unidentifiable observations
	drop if missing(date) | missing(facility)
	//drop if facility == "Regional Hospital, Bolgatanga"
    // if the dta file exists append to the existing data set
    capture confirm file "`dtafile'"
	if _rc == 0 {
		append using "`dtafile'"
	}
	
	// save updated dta file
	save "`dtafile'", replace
}

// <================== annotate final data set ==================> //

// label variables in cleaned data set.
label variable date "Date of observation."
label variable facility "Name of Ghana Health Service facility."
label variable district "Ghanaian political district."
label variable treatment "Dummy indicator of whether facility received SERC intervention."
label variable period "Dummy indicator for post intervention period."
label variable chps "Dummy indicator for CHPS facility (level A)."
label variable clinic "Dummy indicator for clinic of health center (level B)."
label variable hospital "Dummy indicator for hospital (level C)."
label variable deliveries "Number of deliveries recorded."
label variable maternal_deaths "Number of maternal deaths recorded."
label variable csections "Number of csections recorded."

// encode string variables
ds, has(type string)
foreach var in `r(varlist)' {
	encode `var', gen(new) label(test)
	drop `var'
	rename new `var'
}

// order variables
order date                          ///
	  facility                      ///
      district                      ///
      treatment                     ///
      period                        ///
	  chps                          ///
      clinic                        ///
	  hospital                      ///
	  deliveries                    ///
	  maternal_deaths               ///
	  csections
	  
// add notes to the data set
dtanotes, creator("01_clean_data.do")

// save updated dta file
save "`dtafile'", replace
