
/*******************************
*FOR EXPLORING DATA OUTSIDE OF SHINY APP
********************************
	This file reads in the analysis.dta dataset that is produced in R
	It allows you to look at the outcome variables in the data 
********************************/
*SET DIRECTORY TO redblue folder 
global redblue "C:\Users\nsalwati\Downloads\red_blue"

		global data_raw "${redblue}/data-raw"
		global data "${redblue}/data"
		global output "${redblue}/output"
		global code "${redblue}/code"

use "${output}/analysis", clear 