/*-------------------------------*
 |file:    00_master.do          |
 |project: SERC timeseries       |
 |author:  christopher boyer     |
 |date:    26 oct 2015           |
 *-------------------------------*
  description:
    this is the master do-file for the analysis of the impact of the Sustainable
  Emergency Referral Care Initiative on maternal and newborn health outcomes in 
  the Upper East Region of Ghana during the period of 2009 to 2014. running 
  the program replicates every step from cleaning the raw .dta files through 
  merging, reshaping, and analysis.
*/

clear
version 13

// <================== Section 1: Define global variables ==================> //

global proj "C:\Users\cboyer.IPA\Dropbox\ARCHeS\CU Practicum Ghana\2014\Christopher Boyer\projects\SERC-timeseries-evaluation"
global rawdata "${proj}/data/raw"
global cleandata "${proj}/data/clean"
global bin "${proj}/Stata"
global figures "${proj}/figures"
global tables "${proj}/tables"

// <================== Section 2: Run project do-files ==================> //
cd "${bin}"
run 01_clean_data.do

cd "${bin}"
run 02_analysis.do

cd "${bin}"
run 03_figures.do





