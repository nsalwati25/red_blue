
librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, fredr, BrookingsInstitution/ggbrookings, ipumsr, here, haven, broom)

#Set the working directory to the red_blue folder here
setwd("/Users/nasiha/red_blue/red_blue")
main_dir<- getwd()
data_raw<- glue("{main_dir}/data-raw")
data<- glue("{main_dir}/data")

# Analysis Prep ----------------------------------------------------------------
source("code/functions.R")

#read in data from stata 
merged<- read_dta("output/merged_data.dta")

#create vectors of strings for variable names 
cps_vars<- vars_start_with(merged, c("epop", "lfp", "ur", "remw"))
bea_vars<- vars_start_with(merged, c("pce"))
oi_vars<- vars_start_with(merged, c("spend", "gps"))
zillow_vars<- vars_start_with(merged, c("rental_index"))
bea_shvars<- vars_start_with(merged, c("psh"))
#covid_vars<- vars_start_with(merged, c("vax", "fullvaccine", "case_rate", "death_rate"))

#create transformed data frame
transformed<- merged %>% multiply_by_factor(c(cps_vars, oi_vars), 100) %>% #multiply the cps vars and oi vars by 100
  delta_by_month(2019, cps_vars) %>% #delta from the same month in 2019 for the cps vars 
  pctchange_by_month(2019, c(bea_vars, zillow_vars)) %>% #% change from the same month for bea and zillow vars 
  delta_over_year(cps_vars) %>% #delta from last year for cps vars 
  pctchange_over_year(c(bea_vars, zillow_vars))%>% #% change from last year for bea and zillow vars 
  delta_by_month(2019, "consumption_share") %>%
  delta_over_year("consumption_share") %>%
  delta_by_month(2019, bea_shvars)%>%
  delta_over_year(bea_shvars)
  
analysis_vars<- vars_start_with(transformed, c("delta", "pctchange")) #variables to run regressions on, created from above 
analysis_vars<- c(analysis_vars, oi_vars) #add the oi variables to this 

taken_out_vars<- c(cps_vars, bea_vars, zillow_vars) 

write_rds(transformed, "output/transformed.rds")
write_rds(taken_out_vars, "output/taken_out_vars.rds")

#read transformed dataset back in 
transformed<-read_rds("output/transformed.rds")
analysis <- transformed %>% select((-c(cps_vars, bea_vars, zillow_vars)))

#add remote work variable to the analysis dataset 
remw_w<- transformed$remw_w
analysis$remw<- remw_w

write_rds(analysis, "output/analysis.rds")
write_rds(analysis_vars, "output/analysis_vars.rds")

# Regression --------------------------------------------------------------
source("code/functions.R")
analysis_vars<- read_rds("output/analysis_vars.rds")
analysis<- read_rds("output/analysis.rds") %>% 
  filter(newtime>=2018) %>%  #make the dataset smaller and more manageable 
  add_year_dummy(2020) %>% #add dummies for 2020, 2021, 2022
  add_year_dummy(2021) %>%
  add_year_dummy(2022) %>%
  create_interaction_vars(c("share_blue2020")) #interact dummies with the share blue variable 

#make dataset accessible in stata by shortening the variable names and writing the dataset 'stata' in the output folder
# create a copy of the dataset (optional, but recommended)
analysis_copy <- analysis

# get names
var_names <- names(analysis_copy)

# replace long parts with short ones
var_names <- gsub("delta", "d", var_names)
var_names <- gsub("pctchange", "p", var_names)
var_names <- gsub("male", "ml", var_names)
var_names <- gsub("remwork", "rmw", var_names)
var_names <- gsub("yoy", "y", var_names)

# apply new names
names(analysis_copy) <- var_names
write_dta(analysis_copy, "output/stata.dta") #create this to access the analysis dataset in stata 


#define the sets of variables for the regression 
nocontrols<- c("share_blue2020")
dummies<- c("share_blue2020_2020", "share_blue2020_2021", "share_blue2020_2022", "yes_2020", "yes_2021", "yes_2022")
remwork<- c("share_blue2020", "remw")
remwork_dummies<- c(dummies, "remw")


# Define the list of explanatory variable sets
explanatory_vars_list_monthly <- list(nocontrols, remwork)  

explanatory_vars_list_pooled <- list(dummies, remwork_dummies) 
source("code/functions.R")

# Loop over the list and call the regression_analysis function for each set of variables
for (explanatory_vars in explanatory_vars_list_monthly) {
  results <- run_reg_monthly(analysis, explanatory_vars, analysis_vars, "monthly")
}

# Loop over the list and call the regression_analysis function for each set of variables
for (explanatory_vars in explanatory_vars_list_pooled) {
  results <- run_reg_year_interaction(analysis, explanatory_vars, analysis_vars, "year_interaction")
}

# Coefficient Plots -------------------------------------------------------
source("code/functions.R")

#create dataset of monthly coefficients ---
coefficients_monthly<-regression_output("monthly")%>%
  filter(term == "share_blue2020")%>%
  get_labels() 

cleaned<-coefficients_monthly %>% 
  replace_strings_var("outcome_lab", "outcome_lab", 
                      "Pct change from prev year Pct change from Jan 2020", "Pct change from prev year") 

#create outcome_option as being the duplicate of outcome_lab
cleaned$outcome_option<- cleaned$outcome_lab

types<- c(
  "Change from prev year",
  "Change in change from prev year",
  "Change from 2019",
  "Pct change from prev year",
  "Pct change in pct change from prev year",
  "Pct change from Jan 2020",
  "Pct change from 2019")

substrings_to_remove <- c("^\\s*", "^,")
if (("type" %in% names(cleaned))) {
  cleaned[["type"]] <- NA
}

#remove substrings from outcome_option 
for (substring in types) {
  cleaned<- cleaned %>% 
    remove_substring("outcome_option", substring)%>%
    gen_new_var_condition(substring, "type", "outcome_lab")
}


cleaned$outcome_option<- gsub("^\\s*|^,", "", cleaned$outcome_option)
cleaned$outcome_option<- gsub("^\\s*|^,", "", cleaned$outcome_option)

# Group the data by id and variable
coefficients_grouped <- group_by(cleaned, explanatory) 
coefficients_grouped$controls_lab[coefficients_grouped$controls_lab == ""] <- "None"
write_rds(coefficients_grouped, "charts/coefficients_monthly.rds")



#create dataset of year interaction coefficients ---
coefficients_year_interaction<-regression_output("year_interaction")%>%
  get_labels() 

cleaned<-coefficients_year_interaction %>% 
  replace_strings_var("outcome_lab", "outcome_lab", 
                      "Pct change from prev year Pct change from Jan 2020", "Pct change from prev year") 

#create outcome_option as being the duplicate of outcome_lab
cleaned$outcome_option<- cleaned$outcome_lab

types<- c(
  "Change from prev year",
  "Change in change from prev year",
  "Change from 2019",
  "Pct change from prev year",
  "Pct change in pct change from prev year",
  "Pct change from Jan 2020",
  "Pct change from 2019")

substrings_to_remove <- c("^\\s*", "^,")
if (("type" %in% names(cleaned))) {
  cleaned[["type"]] <- NA
}

#remove substrings from outcome_option 
for (substring in types) {
  cleaned<- cleaned %>% 
    remove_substring("outcome_option", substring)%>%
    gen_new_var_condition(substring, "type", "outcome_lab")
}


cleaned$outcome_option<- gsub("^\\s*|^,", "", cleaned$outcome_option)
cleaned$outcome_option<- gsub("^\\s*|^,", "", cleaned$outcome_option)

# Group the data by id and variable
coefficients_grouped <- group_by(cleaned, explanatory) 
coefficients_grouped$controls_lab[coefficients_grouped$controls_lab == ""] <- "None"
coefficients_grouped$outcome_option_category<- coefficients_grouped$outcome_option
coefficients_grouped<- coefficients_grouped %>% add_category()
write_rds(coefficients_grouped, "charts/coefficients_year_interaction.rds")
