/*-------------------------------*
 |file:    02_analysis.do        |
 |project: SERC timeseries       |
 |author:  christopher boyer     |
 |date:    26 oct 2015           |
 *-------------------------------*
  description:
    this file performs the impact analysis on cleaned DHIMs data.
*/

clear
version 13

// use cleaned data set (global must be defined by 00_master.do)
use "${cleandata}/DHIMS_data.dta", replace

drop if date < monthly("2009-01", "YM") | date > monthly("2014-10", "YM") 
xtset facility date, delta(1 month)
xtreg deliveries i.treatment##i.period if hospital == 1
