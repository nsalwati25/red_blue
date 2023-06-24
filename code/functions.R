
librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, fredr, BrookingsInstitution/ggbrookings, ipumsr, here, haven, broom, stringr)

main_dir<- here()
data_raw<- glue("{main_dir}/data-raw")
data<- glue("{main_dir}/data")

## Functions ---------------------------------------------------------------

# delta_by_month ----------------------------------------------------------
#gets the level change in the variable wrt the same month in the specified year 
delta_by_month <- function(df=df, 
                           year=year, 
                           variables) {
  # Sort the data by year and month
  df <- df[order(df$stateabbrev, df$month, df$year),]
  
  # Loop through each variable
  for (variable in variables) {
    # Create delta column for the current variable
    delta_col_name <- paste0("delta_", variable)
    df[[delta_col_name]] <- NA
    
    # Calculate deltas for each row
    for (i in 1:nrow(df)) {
      if (df$year[i] == year) {
        df[[delta_col_name]][i] <- 0
      } else {
        prev_val <- df[[variable]][df$year == year & df$month == df$month[i] & df$stateabbrev == df$stateabbrev[i]]
        df[[delta_col_name]][i] <- df[[variable]][i] - prev_val
      }
    }
  }
  return(df)
}


# mean_by_levels  -----------------------------------------------------------

mean_by_levels <- function(df, variables) {
  
  # Calculate mean values
  mean_df <- df %>%
    group_by(stateabbrev, year) %>%
    summarise(across
              (all_of(variables), 
                     ~mean(.x, na.rm = TRUE),
                .names = "{.col}_mean"))
  
  # Join the mean values back to the original data frame
  df <- df %>%
    left_join(mean_df, by = c("stateabbrev", "year"))
  
  return(df)
}


# pctchange_by_month ------------------------------------------------------
# gets the percent change in the variable wrt the same month in the specified year 
pctchange_by_month <- function(df=df, 
                               year=year, 
                               variables) {
  # Sort the data by year and month
  df <- df[order(df$stateabbrev, df$month, df$year),]
  
  # Loop through each variable
  for (variable in variables) {
    # Create pctchange column for the current variable
    pctchange_col_name <- paste0("pctchange_", variable)
    df[[pctchange_col_name]] <- NA
    
    # Calculate pctchanges for each row
    for (i in 1:nrow(df)) {
      if (df$year[i] == year) {
        df[[pctchange_col_name]][i] <- 0
      } else {
        prev_val <- df[[variable]][df$year == year & df$month == df$month[i] & df$stateabbrev == df$stateabbrev[i]]
        df[[pctchange_col_name]][i] <- (df[[variable]][i]/prev_val -1)*100 
      }
    }
  }
  return(df)
}


# lag_n_month_change ------------------------------------------------------
# gets the level change in the variable n months back
lag_n_month_change <- function(df = df, 
                               n = n, 
                               variables) {
  # Sort the data by state, year, and month
  df <- df[order(df$stateabbrev, df$year, df$month),]
  
  # Loop through each variable
  for (variable in variables) {
    # Create n-month change column for the current variable
    n_month_change <- paste0("lag_", n, "_month_change_", variable)
    df[[n_month_change]] <- NA
    
    #df$statefips[i] != df$statefips[i]-1
    
    # Calculate n-month changes for each row
    for (i in 1:nrow(df)) {
      prev_val <- df[[variable]][(i-n)%%nrow(df)+1] # use modulo to wrap around to previous year if needed
      
      if (i - n >= 1) {
        prev_state <- df[["stateabbrev"]][i-n]
      } else {
        prev_state <- NA
      }
      cur_state <- df[["stateabbrev"]][i]
      
      
      if (!is.na(cur_state) && !is.na(prev_state) && cur_state == prev_state) {
        df[[n_month_change]][i] <- df[[variable]][i] - prev_val
      } else {
        df[[n_month_change]][i] <- NA
      }
      
    }
  }
  return(df)
}


# vars_start_with ---------------------------------------------------------
# gets the variables that start with the strings you specify
vars_start_with <- function(df, strings) {
  var_names <- names(df)
  result <- var_names[grep(paste0("^(", paste(strings, collapse="|"), ")"), var_names)]
  return(result)
}



# regression_output --------------------------------------------------------
#creates data frame of all regression output
regression_output <- function(prefix) {
  # List all .rds files in the output folder that start with the specified prefix
  file_list <- list.files("output", pattern = paste0("^", prefix, ".*\\.rds$"))
  results_list <- list()
  
  # Loop over the file list and read in each file as a data frame
  for (file in file_list) {
    # Extract the name of the data frame from the file name
    name <- gsub(paste0("^", prefix, "_"), "", file)
    name <- gsub("\\.rds$", "", name)
    
    # Read in the data frame from the file and append it to the results list
    results_list[[name]] <- readRDS(paste0("output/", file))
    # Add the control file name as a new column to the data frame
    results_list[[name]]$controls <- name
  }
  
  # Combine all rows from each data frame in the results list into one dataframe
  coefficients <- do.call(rbind, results_list)
  # Remove the prefix from the controls column
  coefficients$controls <- gsub(paste0("^", prefix, "_"), "", coefficients$controls)
  
  return(coefficients)
}


# test_yearly_coefficients ------------------------------------------------

test_yearly_coefficients <- function(df, year, variable) {
  results <- data.frame()
  
  # Get unique outcomes
  outcomes <- unique(df$outcome)
  
  # Loop through each unique outcome
  for (outcome in outcomes) {
    # Filter data for a given year, outcome and variable
    subdata <- df %>%
      filter(newtime == year, outcome == outcome, term == variable)
    
    # Skip if there are no observations for the current outcome
    if(nrow(subdata) == 0) {
      next
    }
    
    # Extract coefficients
    coefficients <- subdata$estimate
    
    # Perform one-sample t-test (null hypothesis: mean = 0)
    t_test <- t.test(coefficients, mu = 0)
    
    # Extract p-value and confidence interval
    p_value <- t_test$p.value
    conf_int <- t_test$conf.int
    
    # Create a dataframe to store the result
    result_df <- data.frame(
      outcome = outcome,
      p.value = p_value,
      lower_confidence_interval = conf_int[1],
      upper_confidence_interval = conf_int[2],
      significantly_diff_from_zero = ifelse(p_value < 0.05, 'yes', 'no')
    )
    
    # Append the result to the main results dataframe
    results <- rbind(results, result_df)
  }
  
  # Return the dataframe
  return(results)
}


# generate_outcome ----------------------------------------------------------
#creates outcome that is descriptive
generate_outcome <- function(variable) {
  outcome <- gsub("delta_", "Change from same month in 2019, ", variable)
  outcome <- gsub("pctchange_", "Pct change from same month in 2019, ", outcome)
  outcome <- gsub("spend_", "Pct change from Jan 2020, spending ", outcome)
  outcome <- gsub("gps_", "Pct change from Jan 2020, time spent ", outcome)
  outcome <- gsub("merchants_", "Pct change from Jan 2020, small business openings ", outcome)
  outcome <- gsub("revenue_", "Pct change from Jan 2020, small business_revenues ", outcome)
  
  outcome <- gsub("_1620", ",16-20 year olds ", outcome)
  outcome <- gsub("_2125", ",21-25 year olds ", outcome)
  outcome <- gsub("_2630", ",26-30 year olds ", outcome)
  outcome <- gsub("_3135", ",31-35 year olds ", outcome)
  outcome <- gsub("_3640", ",36-40 year olds ", outcome)
  outcome <- gsub("_4145", ",41-45 year olds ", outcome)
  outcome <- gsub("_4650", ",46-50 year olds ", outcome)
  outcome <- gsub("_5155", ",51-55 year olds ", outcome)
  outcome <- gsub("_5660", ",56-60 year olds ", outcome)
  outcome <- gsub("_6165", ",61-65 year olds ", outcome)
  outcome <- gsub("_6670", ",66-70 year olds ", outcome)
  outcome <- gsub("_7175", ",71-75 year olds ", outcome)
  outcome <- gsub("_7680", ",76-80 year olds ", outcome)
  outcome <- gsub("w$", " age-sex weighted", outcome)
  outcome <- gsub("uw$", " unweighted", outcome)
  
  outcome <- gsub("spending all", "spending,all merchants", outcome)
  outcome <- gsub(" aap", ",apparel and accessories", outcome)
  outcome <- gsub(" acf", ",accomodation and food service", outcome)
  outcome <- gsub(" aer", ",arts, entertainment, and recreation", outcome)
  outcome <- gsub(" apg", ",general merchandise stores + apparel and accessories", outcome)
  outcome <- gsub(" gen", ",general merchandise stores", outcome)
  outcome <- gsub(" grf", ",grocery and food stores", outcome)
  outcome <- gsub(" hcs", ",health care and social assistance", outcome)
  outcome <- gsub(" hic", ",home improvement centers", outcome)
  outcome <- gsub(" sgh", ",sporting goods and hobby", outcome)
  outcome <- gsub(" tws", ",transportation and warehousing", outcome)
  
  outcome<-gsub("Pct change from same month in 2019, pce1$", "Pct change from 2019, Headline PCE-Personal consumption expenditures", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce2$", "Pct change from 2019, Goods-Goods", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce3$", "Pct change from 2019, Durable goods-Durable goods", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce4$", "Pct change from 2019, Durable goods-Motor vehicles and parts", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce5$", "Pct change from 2019, Durable goods-New motor vehicles", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce6$", "Pct change from 2019, Durable goods-Net purchases of used motor vehicles", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce7$", "Pct change from 2019, Durable goods-Motor vehicle parts and accessories", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce8$", "Pct change from 2019, Durable goods-Furnishings and durable household equipment", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce9$", "Pct change from 2019, Durable goods-Furniture and furnishings", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce10$", "Pct change from 2019, Durable goods-Household appliances", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce11$", "Pct change from 2019, Durable goods-Glassware, tableware, and household utensils", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce12$", "Pct change from 2019, Durable goods-Tools and equipment for house and garden", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce13$", "Pct change from 2019, Durable goods-Recreational goods and vehicles", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce14$", "Pct change from 2019, Durable goods-Video, audio, photographic, and information processing equipment and media", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce15$", "Pct change from 2019, Durable goods-Sporting equipment, supplies, guns, and ammunition", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce16$", "Pct change from 2019, Durable goods-Sports and recreational vehicles", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce17$", "Pct change from 2019, Durable goods-Recreational books", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce18$", "Pct change from 2019, Durable goods-Musical instruments", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce19$", "Pct change from 2019, Durable goods-Other durable goods", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce20$", "Pct change from 2019, Durable goods-Jewelry and watches", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce21$", "Pct change from 2019, Durable goods-Therapeutic appliances and equipment", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce22$", "Pct change from 2019, Durable goods-Educational books", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce23$", "Pct change from 2019, Durable goods-Luggage and similar personal items", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce24$", "Pct change from 2019, Durable goods-Telephone and related communication equipment", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce25$", "Pct change from 2019, Non durable goods-Nondurable goods", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce26$", "Pct change from 2019, Non durable goods-Food and beverages purchased for off-premises consumption", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce27$", "Pct change from 2019, Non durable goods-Food and nonalcoholic beverages purchased for off-premises consumption", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce28$", "Pct change from 2019, Non durable goods-Alcoholic beverages purchased for off-premises consumption", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce29$", "Pct change from 2019, Non durable goods-Food produced and consumed on farms", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce30$", "Pct change from 2019, Non durable goods-Clothing and footwear", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce31$", "Pct change from 2019, Non durable goods-Garments", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce35$", "Pct change from 2019, Non durable goods-Other clothing materials and footwear", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce36$", "Pct change from 2019, Non durable goods-Gasoline and other energy goods", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce37$", "Pct change from 2019, Non durable goods-Motor vehicle fuels, lubricants, and fluids", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce38$", "Pct change from 2019, Non durable goods-Fuel oil and other fuels", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce39$", "Pct change from 2019, Non durable goods-Other nondurable goods", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce40$", "Pct change from 2019, Non durable goods-Pharmaceutical and other medical products", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce41$", "Pct change from 2019, Non durable goods-Recreational items", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce42$", "Pct change from 2019, Non durable goods-Household supplies", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce43$", "Pct change from 2019, Non durable goods-Personal care products", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce44$", "Pct change from 2019, Non durable goods-Tobacco", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce45$", "Pct change from 2019, Non durable goods-Magazines, newspapers, and stationery", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce46$", "Pct change from 2019, Services-Services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce47$", "Pct change from 2019, Household services-Household consumption expenditures (for services)", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce48$", "Pct change from 2019, Household services-Housing and utilities", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce49$", "Pct change from 2019, Household services-Housing", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce54$", "Pct change from 2019, Household services-Household utilities", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce59$", "Pct change from 2019, Household services-Health care", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce60$", "Pct change from 2019, Household services-Outpatient services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce64$", "Pct change from 2019, Household services-Hospital and nursing home services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce67$", "Pct change from 2019, Household services-Transportation services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce68$", "Pct change from 2019, Household services-Motor vehicle services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce71$", "Pct change from 2019, Household services-Public transportation", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce75$", "Pct change from 2019, Household services-Recreation services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce76$", "Pct change from 2019, Household services-Membership clubs, sports centers, parks, theaters, and museums", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce77$", "Pct change from 2019, Household services-Audio-video, photographic, and information processing equipment services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce78$", "Pct change from 2019, Household services-Gambling", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce79$", "Pct change from 2019, Household services-Other recreational services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce80$", "Pct change from 2019, Household services-Food services and accommodations", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce81$", "Pct change from 2019, Household services-Food services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce84$", "Pct change from 2019, Household services-Accommodations", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce85$", "Pct change from 2019, Household services-Financial services and insurance", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce86$", "Pct change from 2019, Household services-Financial services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce89$", "Pct change from 2019, Household services-Insurance", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce94$", "Pct change from 2019, Household services-Other services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce95$", "Pct change from 2019, Household services-Communication", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce99$", "Pct change from 2019, Household services-Education services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce103$", "Pct change from 2019, Household services-Professional and other services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce104$", "Pct change from 2019, Household services-Personal care and clothing services", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce105$", "Pct change from 2019, Household services-Social services and religious activities", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce106$", "Pct change from 2019, Household services-Household maintenance", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce107$", "Pct change from 2019, Household services-Net foreign travel", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce110$", "Pct change from 2019, NPISH-Final consumption expenditures of nonprofit institutions serving households (NPISHs)", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce111$", "Pct change from 2019, NPISH-Gross output of nonprofit institutions", outcome)
  outcome<-gsub("Pct change from same month in 2019, pce112$", "Pct change from 2019, NPISH-Less: Receipts from sales of goods and services by nonprofit institutions", outcome)
  
  return(outcome)
}

# replace_string ----------------------------------------------------------
replace_string <- function(variable, current_str, replacement) {
  variable <- gsub(current_str, replacement, variable, fixed = TRUE)
  return(variable)
}


# remove_substring --------------------------------------------------------

remove_substring <- function(df, var, substring) {
  df[[var]] <- sub(substring, "", df[[var]])
  return(df)
}


# multiply_by_factor ---------------------------------------------------------

multiply_by_factor <- function(df, var_names, factor_val) {
  # Loop through each variable name and multiply its values by the factor
  for (var_name in var_names) {
    df[[var_name]] <- df[[var_name]] * factor_val
  }
  # Return the updated data frame
  return(df)
}

# delta_over_year ---------------------------------------------------------

delta_over_year <- function(df, variables) {
  # Sort the data by state, year, and month
  df <- df[order(df$stateabbrev, df$year, df$month),]
  
  # Loop through each variable
  for (variable in variables) {
    # Create delta column for the current variable
    delta_col_name <- paste0("delta_yoy_", variable)
    df[[delta_col_name]] <- NA
    
    # Calculate deltas for each row
    for (i in 1:nrow(df)) {
      # Get the value of the current row and the previous year's value
      current_val <- df[[variable]][i]
      prev_val <- df[[variable]][df$year == (df$year[i] - 1) & df$month == df$month[i] & df$stateabbrev == df$stateabbrev[i]]
      
      # If the previous year's value is missing, set the delta to NA
      if (length(prev_val) == 0) {
        df[[delta_col_name]][i] <- NA
      } else {
        df[[delta_col_name]][i] <- current_val - prev_val
      }
    }
  }
  
  return(df)
}

# pctchange_over_year ---------------------------------------------------------

pctchange_over_year <- function(df, variables) {
  # Sort the data by state, year, and month
  df <- df[order(df$stateabbrev, df$year, df$month),]
  
  # Loop through each variable
  for (variable in variables) {
    # Create pctchange column for the current variable
    pctchange_col_name <- paste0("pctchange_yoy_", variable)
    df[[pctchange_col_name]] <- NA
    
    # Calculate pctchanges for each row
    for (i in 1:nrow(df)) {
      # Get the value of the current row and the previous year's value
      current_val <- df[[variable]][i]
      prev_val <- df[[variable]][df$year == (df$year[i] - 1) & df$month == df$month[i] & df$stateabbrev == df$stateabbrev[i]]
      
      # If the previous year's value is missing, set the pctchange to NA
      if (length(prev_val) == 0) {
        df[[pctchange_col_name]][i] <- NA
      } else {
        df[[pctchange_col_name]][i] <- (current_val/prev_val -1) *100
      }
    }
  }
  
  return(df)
}


# replace_strings_var -----------------------------------------------------

replace_strings_var <- function(data, variable1, variable2, original_string, new_string) {
  if(!variable2 %in% names(data)) {
    data[[variable2]] <- ""
  }
  data[[variable2]] <- gsub(original_string, new_string, data[[variable1]])
  return(data)
}

# regression -------------------------------------------------------------------


unique_state_counts <- function(df, varname) {
  library(dplyr)
  
  # Group the data by unique newtime values
  grouped_df <- df %>% group_by(newtime)
  
  # Calculate the number of unique states that have observations for the specified variable
  state_counts <- grouped_df %>% 
    summarize(unique_states = n_distinct(stateabbrev[!is.na(!!sym(varname))]))
  
  return(state_counts)
}

##############
number_of_states <- function(df, varnames) {
  library(dplyr)
  
  # Initialize an empty list to store the results
  results <- list()
  
  # Loop through each variable name in the input vector
  for (varname in varnames) {
    # Use the unique_state_counts function to calculate the unique states for the current variable
    state_counts <- unique_state_counts(df, varname)
    
    # Add the results to the list
    results[[varname]] <- state_counts$unique_states
  }
  
  # Combine the results into a data frame
  output_df <- data.frame(newtime = unique(df$newtime))
  for (varname in varnames) {
    output_df[[paste0(varname)]] <- results[[varname]]
  }
  
  return(output_df)
}

##############
enough_obs <- function(df, n, time) {
  df_subset <- subset(df, newtime == time) # Subset the data to the specified time
  exceeding_vars <- names(df_subset)[which(df_subset > n)] # Find variable names where value > n
  return(exceeding_vars)
}

###############
common_strings <- function(vec1, vec2) {
  vecOUT <- intersect(vec1, vec2)
  return(vecOUT)
}


# run_reg -----------------------------------------------------------------


run_reg_monthly <- function(df, x, y, filename) {
  # Only keep unique values of newtime
  results <- data.frame()
  
  # Loop through each unique month in the data
  for (m in unique(df$newtime)) {
    # Subset the data to only include the current month
    subdata <- df[df$newtime == m,]
  
      # Create a logical index of finite observations in y_var
      for (y_var in y) {
          # Skip the loop for the current y_var if there are missing, infinite, or NaN values in y
          if (any(!is.finite(subdata[[y_var]])) & !startsWith(y_var, 'spend_')) {
            print(paste("Skipping variable:", y_var, "due to no finite observations in month", m))
            next
          }
        
        if (all(!is.finite(subdata[[y_var]])) & startsWith(y_var, 'spend_')) {
          print(paste("Skipping variable:", y_var, "due to no finite observations in month", m))
          next
        }
        
        if (any(is.finite(subdata[[y_var]])) & startsWith(y_var, 'spend_')){
          subdata<- subdata[!is.na(subdata[[y_var]]), ]
          subdata <- subdata[!is.infinite(subdata[[y_var]]), ]
        }
      
      # Run linear regression for each y variable with enough observations
      formula <- as.formula(paste(y_var, paste(x, collapse = "+"), sep = " ~ "))
      model <- lm(formula = formula, data = subdata)
      
      
      # Add the month and variables to the results data frame
      summary <- tidy(model)
      summary$newtime <- m
      summary$outcome <- y_var
      summary$explanatory<-  paste(x, collapse = "+")
      
      # Add number of observations
      summary$nobs <- nrow(subdata)
      
      # Add R-squared
      summary$rsquared <- summary(model)$r.squared
      
      # Append the results to the main data frame
      results <- rbind(results, summary)
    }
  }
  # Save the results data set in the location: output/regression_{explanatory variables}
  write_rds(results, paste0("output/", paste0(filename, c(x), collapse="_"), ".rds"))
  # Return the results data frame
  return(results)
}


# get_labels --------------------------------------------------------------

get_labels<- function(df){
  df<- df %>% 
    replace_strings_var("outcome", "outcome_lab", "delta_yoy_delta_yoy_", "Change in change from prev year ") %>%
    replace_strings_var("outcome_lab", "outcome_lab", "delta_yoy_", "Change from prev year " ) %>%
    replace_strings_var("outcome_lab", "outcome_lab", "delta_", "Change from 2019 ") %>%
    replace_strings_var("outcome_lab", "outcome_lab", "pctchange_yoy_pctchange_yoy_", "Pct change in pct change from prev year" ) %>%
    replace_strings_var("outcome_lab", "outcome_lab", "pctchange_yoy_", "Pct change from prev year " ) %>%
    replace_strings_var("outcome_lab", "outcome_lab", "pctchange_", "Pct change from 2019") %>%
    replace_strings_var("outcome_lab", "outcome_lab", "spend_", "Pct change from Jan 2020, spending ") %>%
    replace_strings_var("outcome_lab", "outcome_lab", "gps_", "Pct change from Jan 2020, time spent ") %>%
    replace_strings_var("outcome_lab", "outcome_lab", "merchants_", "Pct change from Jan 2020, small business openings ") %>%
    replace_strings_var("outcome_lab", "outcome_lab", "revenue_", "Pct change from Jan 2020, small business_revenues ") %>%
    
    replace_strings_var("outcome_lab", "outcome_lab", "_1620", ",16-20 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_2125", ",21-25 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_2630", ",26-30 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_3135", ",31-35 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_3640", ",36-40 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_4145", ",41-45 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_4650", ",46-50 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_5155", ",51-55 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_5660", ",56-60 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_6165", ",61-65 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_6670", ",66-70 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_7175", ",71-75 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_7680", ",76-80 year olds ")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_w$", " age-sex weighted")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "_uw$", " unweighted")%>%
    
    replace_strings_var("outcome_lab", "outcome_lab", "spending all", "spending,all merchants") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " aap", ",apparel and accessories") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " acf", ",accomodation and food service") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " aer", ",arts, entertainment, and recreation") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " apg", ",general merchandise stores + apparel and accessories") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " gen", ",general merchandise stores") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " grf", ",grocery and food stores") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " hcs", ",health care and social assistance") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " hic", ",home improvement centers") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " sgh", ",sporting goods and hobby") %>%
    replace_strings_var("outcome_lab", "outcome_lab", " tws", ",transportation and warehousing") %>%
    
    
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce1$", "Pct change from 2019, Headline PCE-Personal consumption expenditures")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce2$", "Pct change from 2019, Goods-Goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce3$", "Pct change from 2019, Durable goods-Durable goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce4$", "Pct change from 2019, Durable goods-Motor vehicles and parts")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce5$", "Pct change from 2019, Durable goods-New motor vehicles")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce6$", "Pct change from 2019, Durable goods-Net purchases of used motor vehicles")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce7$", "Pct change from 2019, Durable goods-Motor vehicle parts and accessories")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce8$", "Pct change from 2019, Durable goods-Furnishings and durable household equipment")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce9$", "Pct change from 2019, Durable goods-Furniture and furnishings")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce10$", "Pct change from 2019, Durable goods-Household appliances")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce11$", "Pct change from 2019, Durable goods-Glassware, tableware, and household utensils")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce12$", "Pct change from 2019, Durable goods-Tools and equipment for house and garden")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce13$", "Pct change from 2019, Durable goods-Recreational goods and vehicles")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce14$", "Pct change from 2019, Durable goods-Video, audio, photographic, and information processing equipment and media")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce15$", "Pct change from 2019, Durable goods-Sporting equipment, supplies, guns, and ammunition")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce16$", "Pct change from 2019, Durable goods-Sports and recreational vehicles")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce17$", "Pct change from 2019, Durable goods-Recreational books")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce18$", "Pct change from 2019, Durable goods-Musical instruments")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce19$", "Pct change from 2019, Durable goods-Other durable goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce20$", "Pct change from 2019, Durable goods-Jewelry and watches")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce21$", "Pct change from 2019, Durable goods-Therapeutic appliances and equipment")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce22$", "Pct change from 2019, Durable goods-Educational books")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce23$", "Pct change from 2019, Durable goods-Luggage and similar personal items")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce24$", "Pct change from 2019, Durable goods-Telephone and related communication equipment")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce25$", "Pct change from 2019, Non durable goods-Nondurable goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce26$", "Pct change from 2019, Non durable goods-Food and beverages purchased for off-premises consumption")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce27$", "Pct change from 2019, Non durable goods-Food and nonalcoholic beverages purchased for off-premises consumption")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce28$", "Pct change from 2019, Non durable goods-Alcoholic beverages purchased for off-premises consumption")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce29$", "Pct change from 2019, Non durable goods-Food produced and consumed on farms")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce30$", "Pct change from 2019, Non durable goods-Clothing and footwear")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce31$", "Pct change from 2019, Non durable goods-Garments")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce35$", "Pct change from 2019, Non durable goods-Other clothing materials and footwear")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce36$", "Pct change from 2019, Non durable goods-Gasoline and other energy goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce37$", "Pct change from 2019, Non durable goods-Motor vehicle fuels, lubricants, and fluids")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce38$", "Pct change from 2019, Non durable goods-Fuel oil and other fuels")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce39$", "Pct change from 2019, Non durable goods-Other nondurable goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce40$", "Pct change from 2019, Non durable goods-Pharmaceutical and other medical products")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce41$", "Pct change from 2019, Non durable goods-Recreational items")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce42$", "Pct change from 2019, Non durable goods-Household supplies")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce43$", "Pct change from 2019, Non durable goods-Personal care products")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce44$", "Pct change from 2019, Non durable goods-Tobacco")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce45$", "Pct change from 2019, Non durable goods-Magazines, newspapers, and stationery")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce46$", "Pct change from 2019, Services-Services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce47$", "Pct change from 2019, Household services-Household consumption expenditures (for services)")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce48$", "Pct change from 2019, Household services-Housing and utilities")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce49$", "Pct change from 2019, Household services-Housing")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce54$", "Pct change from 2019, Household services-Household utilities")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce59$", "Pct change from 2019, Household services-Health care")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce60$", "Pct change from 2019, Household services-Outpatient services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce64$", "Pct change from 2019, Household services-Hospital and nursing home services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce67$", "Pct change from 2019, Household services-Transportation services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce68$", "Pct change from 2019, Household services-Motor vehicle services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce71$", "Pct change from 2019, Household services-Public transportation")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce75$", "Pct change from 2019, Household services-Recreation services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce76$", "Pct change from 2019, Household services-Membership clubs, sports centers, parks, theaters, and museums")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce77$", "Pct change from 2019, Household services-Audio-video, photographic, and information processing equipment services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce78$", "Pct change from 2019, Household services-Gambling")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce79$", "Pct change from 2019, Household services-Other recreational services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce80$", "Pct change from 2019, Household services-Food services and accommodations")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce81$", "Pct change from 2019, Household services-Food services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce84$", "Pct change from 2019, Household services-Accommodations")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce85$", "Pct change from 2019, Household services-Financial services and insurance")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce86$", "Pct change from 2019, Household services-Financial services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce89$", "Pct change from 2019, Household services-Insurance")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce94$", "Pct change from 2019, Household services-Other services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce95$", "Pct change from 2019, Household services-Communication")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce99$", "Pct change from 2019, Household services-Education services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce103$", "Pct change from 2019, Household services-Professional and other services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce104$", "Pct change from 2019, Household services-Personal care and clothing services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce105$", "Pct change from 2019, Household services-Social services and religious activities")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce106$", "Pct change from 2019, Household services-Household maintenance")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce107$", "Pct change from 2019, Household services-Net foreign travel")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce110$", "Pct change from 2019, NPISH-Final consumption expenditures of nonprofit institutions serving households (NPISHs)")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce111$", "Pct change from 2019, NPISH-Gross output of nonprofit institutions")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "Pct change from 2019pce112$", "Pct change from 2019, NPISH-Less: Receipts from sales of goods and services by nonprofit institutions")%>%
    
    
    replace_strings_var("outcome_lab", "outcome_lab", "pce1$",",Headline PCE-Personal consumption expenditures")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce2$",",Goods-Goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce3$",",Durable goods-Durable goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce4$",",Durable goods-Motor vehicles and parts")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce5$",",Durable goods-New motor vehicles")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce6$",",Durable goods-Net purchases of used motor vehicles")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce7$",",Durable goods-Motor vehicle parts and accessories")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce8$",",Durable goods-Furnishings and durable household equipment")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce9$",",Durable goods-Furniture and furnishings")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce10$",",Durable goods-Household appliances")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce11$",",Durable goods-Glassware, tableware, and household utensils")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce12$",",Durable goods-Tools and equipment for house and garden")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce13$",",Durable goods-Recreational goods and vehicles")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce14$",",Durable goods-Video, audio, photographic, and information processing equipment and media")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce15$",",Durable goods-Sporting equipment, supplies, guns, and ammunition")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce16$",",Durable goods-Sports and recreational vehicles")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce17$",",Durable goods-Recreational books")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce18$",",Durable goods-Musical instruments")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce19$",",Durable goods-Other durable goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce20$",",Durable goods-Jewelry and watches")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce21$",",Durable goods-Therapeutic appliances and equipment")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce22$",",Durable goods-Educational books")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce23$",",Durable goods-Luggage and similar personal items")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce24$",",Durable goods-Telephone and related communication equipment")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce25$",",Non durable goods-Nondurable goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce26$",",Non durable goods-Food and beverages purchased for off-premises consumption")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce27$",",Non durable goods-Food and nonalcoholic beverages purchased for off-premises consumption")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce28$",",Non durable goods-Alcoholic beverages purchased for off-premises consumption")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce29$",",Non durable goods-Food produced and consumed on farms")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce30$",",Non durable goods-Clothing and footwear")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce31$",",Non durable goods-Garments")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce35$",",Non durable goods-Other clothing materials and footwear")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce36$",",Non durable goods-Gasoline and other energy goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce37$",",Non durable goods-Motor vehicle fuels, lubricants, and fluids")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce38$",",Non durable goods-Fuel oil and other fuels")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce39$",",Non durable goods-Other nondurable goods")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce40$",",Non durable goods-Pharmaceutical and other medical products")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce41$",",Non durable goods-Recreational items")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce42$",",Non durable goods-Household supplies")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce43$",",Non durable goods-Personal care products")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce44$",",Non durable goods-Tobacco")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce45$",",Non durable goods-Magazines, newspapers, and stationery")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce46$",",Services-Services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce47$",",Household services-Household consumption expenditures (for services)")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce48$",",Household services-Housing and utilities")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce49$",",Household services-Housing")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce54$",",Household services-Household utilities")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce59$",",Household services-Health care")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce60$",",Household services-Outpatient services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce64$",",Household services-Hospital and nursing home services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce67$",",Household services-Transportation services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce68$",",Household services-Motor vehicle services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce71$",",Household services-Public transportation")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce75$",",Household services-Recreation services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce76$",",Household services-Membership clubs, sports centers, parks, theaters, and museums")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce77$",",Household services-Audio-video, photographic, and information processing equipment services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce78$",",Household services-Gambling")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce79$",",Household services-Other recreational services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce80$",",Household services-Food services and accommodations")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce81$",",Household services-Food services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce84$",",Household services-Accommodations")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce85$",",Household services-Financial services and insurance")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce86$",",Household services-Financial services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce89$",",Household services-Insurance")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce94$",",Household services-Other services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce95$",",Household services-Communication")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce99$",",Household services-Education services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce103$",",Household services-Professional and other services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce104$",",Household services-Personal care and clothing services")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce105$",",Household services-Social services and religious activities")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce106$",",Household services-Household maintenance")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce107$",",Household services-Net foreign travel")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce110$",",NPISH-Final consumption expenditures of nonprofit institutions serving households (NPISHs)")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce111$",",NPISH-Gross output of nonprofit institutions")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce112$",",NPISH-Less: Receipts from sales of goods and services by nonprofit institutions") %>%
    
    
    replace_strings_var("outcome_lab", "outcome_lab", "pce1_$",",Headline PCE-Personal consumption expenditures 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce2_$",",Goods-Goods 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce3_$",",Durable goods-Durable goods 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce4_$",",Durable goods-Motor vehicles and parts 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce5_$",",Durable goods-New motor vehicles 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce6_$",",Durable goods-Net purchases of used motor vehicles 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce7_$",",Durable goods-Motor vehicle parts and accessories 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce8_$",",Durable goods-Furnishings and durable household equipment 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce9_$",",Durable goods-Furniture and furnishings 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce10_$",",Durable goods-Household appliances 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce11_$",",Durable goods-Glassware, tableware, and household utensils 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce12_$",",Durable goods-Tools and equipment for house and garden 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce13_$",",Durable goods-Recreational goods and vehicles 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce14_$",",Durable goods-Video, audio, photographic, and information processing equipment and media 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce15_$",",Durable goods-Sporting equipment, supplies, guns, and ammunition 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce16_$",",Durable goods-Sports and recreational vehicles 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce17_$",",Durable goods-Recreational books 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce18_$",",Durable goods-Musical instruments 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce19_$",",Durable goods-Other durable goods 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce20_$",",Durable goods-Jewelry and watches 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce21_$",",Durable goods-Therapeutic appliances and equipment 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce22_$",",Durable goods-Educational books 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce23_$",",Durable goods-Luggage and similar personal items 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce24_$",",Durable goods-Telephone and related communication equipment 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce25_$",",Non durable goods-Nondurable goods 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce26_$",",Non durable goods-Food and beverages purchased for off-premises consumption 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce27_$",",Non durable goods-Food and nonalcoholic beverages purchased for off-premises consumption 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce28_$",",Non durable goods-Alcoholic beverages purchased for off-premises consumption 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce29_$",",Non durable goods-Food produced and consumed on farms 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce30_$",",Non durable goods-Clothing and footwear 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce31_$",",Non durable goods-Garments 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce35_$",",Non durable goods-Other clothing materials and footwear 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce36_$",",Non durable goods-Gasoline and other energy goods 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce37_$",",Non durable goods-Motor vehicle fuels, lubricants, and fluids 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce38_$",",Non durable goods-Fuel oil and other fuels 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce39_$",",Non durable goods-Other nondurable goods 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce40_$",",Non durable goods-Pharmaceutical and other medical products 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce41_$",",Non durable goods-Recreational items 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce42_$",",Non durable goods-Household supplies 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce43_$",",Non durable goods-Personal care products 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce44_$",",Non durable goods-Tobacco 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce45_$",",Non durable goods-Magazines, newspapers, and stationery 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce46_$",",Services-Services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce47_$",",Household services-Household consumption expenditures (for services) 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce48_$",",Household services-Housing and utilities 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce49_$",",Household services-Housing 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce54_$",",Household services-Household utilities 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce59_$",",Household services-Health care 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce60_$",",Household services-Outpatient services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce64_$",",Household services-Hospital and nursing home services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce67_$",",Household services-Transportation services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce68_$",",Household services-Motor vehicle services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce71_$",",Household services-Public transportation 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce75_$",",Household services-Recreation services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce76_$",",Household services-Membership clubs, sports centers, parks, theaters, and museums 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce77_$",",Household services-Audio-video, photographic, and information processing equipment services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce78_$",",Household services-Gambling 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce79_$",",Household services-Other recreational services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce80_$",",Household services-Food services and accommodations 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce81_$",",Household services-Food services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce84_$",",Household services-Accommodations 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce85_$",",Household services-Financial services and insurance 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce86_$",",Household services-Financial services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce89_$",",Household services-Insurance 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce94_$",",Household services-Other services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce95_$",",Household services-Communication 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce99_$",",Household services-Education services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce103_$",",Household services-Professional and other services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce104_$",",Household services-Personal care and clothing services 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce105_$",",Household services-Social services and religious activities 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce106_$",",Household services-Household maintenance 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce107_$",",Household services-Net foreign travel 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce110_$",",NPISH-Final consumption expenditures of nonprofit institutions serving households (NPISHs) 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce111_$",",NPISH-Gross output of nonprofit institutions 2019 pop")%>%
    replace_strings_var("outcome_lab", "outcome_lab", "pce112_$",",NPISH-Less: Receipts from sales of goods and services by nonprofit institutions 2019 pop")%>%
  
    replace_strings_var("explanatory", "controls_lab", "share_blue2020+", "")%>%
    replace_strings_var("controls_lab", "controls_lab", "share_blue2020", "-")%>%
    replace_strings_var("controls_lab", "controls_lab", "-+", "") %>%
    replace_strings_var("controls_lab", "controls_lab", "black_share19", "Percent Black 2019 ") %>%
    replace_strings_var("controls_lab", "controls_lab", "sixtyfivep_share19", "Percent 65plus 2019 ") %>%
    replace_strings_var("controls_lab", "controls_lab", "death_rate", "COVID deaths per 100,000 people ") %>%
    replace_strings_var("controls_lab", "controls_lab", "final_vax", "Fully vaccinated rate as of Oct 2022 ") %>%
    replace_strings_var("controls_lab", "controls_lab", "lag_3_month_change_case_rate", "COVID cases per 100,000 people, 3 month change ") %>%
    replace_strings_var("controls_lab", "controls_lab", "oil_state", "Oil State: TX ND NM OK CO AK ") %>%
    replace_strings_var("controls_lab", "controls_lab", "acc_food_share", "Emp share 2019-accomodation and food services ") %>%
    replace_strings_var("controls_lab", "controls_lab", "services_share", "Emp share 2019- services ")%>%
    replace_strings_var("controls_lab", "controls_lab", "_2020+_2021+_2022+yes_2020+yes_2021+yes_2022+remw", "with controls for remote work")%>%
    replace_strings_var("controls_lab", "controls_lab", "_2020+_2021+_2022+yes_2020+yes_2021+yes_2022", "without controls for remote work")
  return(df)
}


# strings_not_in_common ---------------------------------------------------
library(stringr)
strings_not_in_common <- function(str1, str2) {
  common_substring <- str_match(str1, paste0("(?i)", str2))[1]
  if (is.na(common_substring)) {
    return(str1)
  } else {
    result <- str_remove(str1, common_substring)
    return(result)
  }
}


# renamed_vars_df ---------------------------------------------------------

renamed_vars_df<- function(df){
  df<- df %>%
rename_vars("delta_yoy_delta_yoy_", "Change from prev year " ) %>%
  rename_vars("delta_yoy_", "Change from prev year " ) %>%
  rename_vars("delta_", "Change from 2019 ") %>%
  rename_vars("pctchange_yoy_pctchange_yoy_", "Pct change in pct change from prev year" ) %>%
  rename_vars("pctchange_yoy_", "Pct change from prev year " ) %>%
  rename_vars("pctchange_", "Pct change from 2019") %>%
  rename_vars("spend_", "Pct change from Jan 2020, spending ") %>%
  rename_vars("gps_", "Pct change from Jan 2020, time spent ") %>%
  rename_vars("merchants_", "Pct change from Jan 2020, small business openings ") %>%
  rename_vars("revenue_", "Pct change from Jan 2020, small business_revenues ")%>%
  rename_vars("_1620", ",16-20 year olds ")%>%
  rename_vars("_2125", ",21-25 year olds ")%>%
  rename_vars("_2630", ",26-30 year olds ")%>%
  rename_vars("_3135", ",31-35 year olds ")%>%
  rename_vars("_3640", ",36-40 year olds ")%>%
  rename_vars("_4145", ",41-45 year olds ")%>%
  rename_vars("_4650", ",46-50 year olds ")%>%
  rename_vars("_5155", ",51-55 year olds ")%>%
  rename_vars("_5660", ",56-60 year olds ")%>%
  rename_vars("_6165", ",61-65 year olds ")%>%
  rename_vars("_6670", ",66-70 year olds ")%>%
  rename_vars("_7175", ",71-75 year olds ")%>%
  rename_vars("_7680", ",76-80 year olds ")%>%
  rename_vars("_w$", " age-sex weighted")%>%
  rename_vars("_uw$", " unweighted")%>%
  
  rename_vars("spending all", "spending,all merchants") %>%
  rename_vars(" aap", ",apparel and accessories") %>%
  rename_vars(" acf", ",accomodation and food service") %>%
  rename_vars(" aer", ",arts, entertainment, and recreation") %>%
  rename_vars(" apg", ",general merchandise stores + apparel and accessories") %>%
  rename_vars(" gen", ",general merchandise stores") %>%
  rename_vars(" grf", ",grocery and food stores") %>%
  rename_vars(" hcs", ",health care and social assistance") %>%
  rename_vars(" hic", ",home improvement centers") %>%
  rename_vars(" sgh", ",sporting goods and hobby") %>%
  rename_vars(" tws", ",transportation and warehousing") %>%
  
  
  rename_vars("Pct change from 2019pce1$", "Pct change from 2019, Headline PCE-Personal consumption expenditures")%>%
  rename_vars("Pct change from 2019pce2$", "Pct change from 2019, Goods-Goods")%>%
  rename_vars("Pct change from 2019pce3$", "Pct change from 2019, Durable goods-Durable goods")%>%
  rename_vars("Pct change from 2019pce4$", "Pct change from 2019, Durable goods-Motor vehicles and parts")%>%
  rename_vars("Pct change from 2019pce5$", "Pct change from 2019, Durable goods-New motor vehicles")%>%
  rename_vars("Pct change from 2019pce6$", "Pct change from 2019, Durable goods-Net purchases of used motor vehicles")%>%
  rename_vars("Pct change from 2019pce7$", "Pct change from 2019, Durable goods-Motor vehicle parts and accessories")%>%
  rename_vars("Pct change from 2019pce8$", "Pct change from 2019, Durable goods-Furnishings and durable household equipment")%>%
  rename_vars("Pct change from 2019pce9$", "Pct change from 2019, Durable goods-Furniture and furnishings")%>%
  rename_vars("Pct change from 2019pce10$", "Pct change from 2019, Durable goods-Household appliances")%>%
  rename_vars("Pct change from 2019pce11$", "Pct change from 2019, Durable goods-Glassware, tableware, and household utensils")%>%
  rename_vars("Pct change from 2019pce12$", "Pct change from 2019, Durable goods-Tools and equipment for house and garden")%>%
  rename_vars("Pct change from 2019pce13$", "Pct change from 2019, Durable goods-Recreational goods and vehicles")%>%
  rename_vars("Pct change from 2019pce14$", "Pct change from 2019, Durable goods-Video, audio, photographic, and information processing equipment and media")%>%
  rename_vars("Pct change from 2019pce15$", "Pct change from 2019, Durable goods-Sporting equipment, supplies, guns, and ammunition")%>%
  rename_vars("Pct change from 2019pce16$", "Pct change from 2019, Durable goods-Sports and recreational vehicles")%>%
  rename_vars("Pct change from 2019pce17$", "Pct change from 2019, Durable goods-Recreational books")%>%
  rename_vars("Pct change from 2019pce18$", "Pct change from 2019, Durable goods-Musical instruments")%>%
  rename_vars("Pct change from 2019pce19$", "Pct change from 2019, Durable goods-Other durable goods")%>%
  rename_vars("Pct change from 2019pce20$", "Pct change from 2019, Durable goods-Jewelry and watches")%>%
  rename_vars("Pct change from 2019pce21$", "Pct change from 2019, Durable goods-Therapeutic appliances and equipment")%>%
  rename_vars("Pct change from 2019pce22$", "Pct change from 2019, Durable goods-Educational books")%>%
  rename_vars("Pct change from 2019pce23$", "Pct change from 2019, Durable goods-Luggage and similar personal items")%>%
  rename_vars("Pct change from 2019pce24$", "Pct change from 2019, Durable goods-Telephone and related communication equipment")%>%
  rename_vars("Pct change from 2019pce25$", "Pct change from 2019, Non durable goods-Nondurable goods")%>%
  rename_vars("Pct change from 2019pce26$", "Pct change from 2019, Non durable goods-Food and beverages purchased for off-premises consumption")%>%
  rename_vars("Pct change from 2019pce27$", "Pct change from 2019, Non durable goods-Food and nonalcoholic beverages purchased for off-premises consumption")%>%
  rename_vars("Pct change from 2019pce28$", "Pct change from 2019, Non durable goods-Alcoholic beverages purchased for off-premises consumption")%>%
  rename_vars("Pct change from 2019pce29$", "Pct change from 2019, Non durable goods-Food produced and consumed on farms")%>%
  rename_vars("Pct change from 2019pce30$", "Pct change from 2019, Non durable goods-Clothing and footwear")%>%
  rename_vars("Pct change from 2019pce31$", "Pct change from 2019, Non durable goods-Garments")%>%
  rename_vars("Pct change from 2019pce35$", "Pct change from 2019, Non durable goods-Other clothing materials and footwear")%>%
  rename_vars("Pct change from 2019pce36$", "Pct change from 2019, Non durable goods-Gasoline and other energy goods")%>%
  rename_vars("Pct change from 2019pce37$", "Pct change from 2019, Non durable goods-Motor vehicle fuels, lubricants, and fluids")%>%
  rename_vars("Pct change from 2019pce38$", "Pct change from 2019, Non durable goods-Fuel oil and other fuels")%>%
  rename_vars("Pct change from 2019pce39$", "Pct change from 2019, Non durable goods-Other nondurable goods")%>%
  rename_vars("Pct change from 2019pce40$", "Pct change from 2019, Non durable goods-Pharmaceutical and other medical products")%>%
  rename_vars("Pct change from 2019pce41$", "Pct change from 2019, Non durable goods-Recreational items")%>%
  rename_vars("Pct change from 2019pce42$", "Pct change from 2019, Non durable goods-Household supplies")%>%
  rename_vars("Pct change from 2019pce43$", "Pct change from 2019, Non durable goods-Personal care products")%>%
  rename_vars("Pct change from 2019pce44$", "Pct change from 2019, Non durable goods-Tobacco")%>%
  rename_vars("Pct change from 2019pce45$", "Pct change from 2019, Non durable goods-Magazines, newspapers, and stationery")%>%
  rename_vars("Pct change from 2019pce46$", "Pct change from 2019, Services-Services")%>%
  rename_vars("Pct change from 2019pce47$", "Pct change from 2019, Household services-Household consumption expenditures (for services)")%>%
  rename_vars("Pct change from 2019pce48$", "Pct change from 2019, Household services-Housing and utilities")%>%
  rename_vars("Pct change from 2019pce49$", "Pct change from 2019, Household services-Housing")%>%
  rename_vars("Pct change from 2019pce54$", "Pct change from 2019, Household services-Household utilities")%>%
  rename_vars("Pct change from 2019pce59$", "Pct change from 2019, Household services-Health care")%>%
  rename_vars("Pct change from 2019pce60$", "Pct change from 2019, Household services-Outpatient services")%>%
  rename_vars("Pct change from 2019pce64$", "Pct change from 2019, Household services-Hospital and nursing home services")%>%
  rename_vars("Pct change from 2019pce67$", "Pct change from 2019, Household services-Transportation services")%>%
  rename_vars("Pct change from 2019pce68$", "Pct change from 2019, Household services-Motor vehicle services")%>%
  rename_vars("Pct change from 2019pce71$", "Pct change from 2019, Household services-Public transportation")%>%
  rename_vars("Pct change from 2019pce75$", "Pct change from 2019, Household services-Recreation services")%>%
  rename_vars("Pct change from 2019pce76$", "Pct change from 2019, Household services-Membership clubs, sports centers, parks, theaters, and museums")%>%
  rename_vars("Pct change from 2019pce77$", "Pct change from 2019, Household services-Audio-video, photographic, and information processing equipment services")%>%
  rename_vars("Pct change from 2019pce78$", "Pct change from 2019, Household services-Gambling")%>%
  rename_vars("Pct change from 2019pce79$", "Pct change from 2019, Household services-Other recreational services")%>%
  rename_vars("Pct change from 2019pce80$", "Pct change from 2019, Household services-Food services and accommodations")%>%
  rename_vars("Pct change from 2019pce81$", "Pct change from 2019, Household services-Food services")%>%
  rename_vars("Pct change from 2019pce84$", "Pct change from 2019, Household services-Accommodations")%>%
  rename_vars("Pct change from 2019pce85$", "Pct change from 2019, Household services-Financial services and insurance")%>%
  rename_vars("Pct change from 2019pce86$", "Pct change from 2019, Household services-Financial services")%>%
  rename_vars("Pct change from 2019pce89$", "Pct change from 2019, Household services-Insurance")%>%
  rename_vars("Pct change from 2019pce94$", "Pct change from 2019, Household services-Other services")%>%
  rename_vars("Pct change from 2019pce95$", "Pct change from 2019, Household services-Communication")%>%
  rename_vars("Pct change from 2019pce99$", "Pct change from 2019, Household services-Education services")%>%
  rename_vars("Pct change from 2019pce103$", "Pct change from 2019, Household services-Professional and other services")%>%
  rename_vars("Pct change from 2019pce104$", "Pct change from 2019, Household services-Personal care and clothing services")%>%
  rename_vars("Pct change from 2019pce105$", "Pct change from 2019, Household services-Social services and religious activities")%>%
  rename_vars("Pct change from 2019pce106$", "Pct change from 2019, Household services-Household maintenance")%>%
  rename_vars("Pct change from 2019pce107$", "Pct change from 2019, Household services-Net foreign travel")%>%
  rename_vars("Pct change from 2019pce110$", "Pct change from 2019, NPISH-Final consumption expenditures of nonprofit institutions serving households (NPISHs)")%>%
  rename_vars("Pct change from 2019pce111$", "Pct change from 2019, NPISH-Gross output of nonprofit institutions")%>%
  rename_vars("Pct change from 2019pce112$", "Pct change from 2019, NPISH-Less: Receipts from sales of goods and services by nonprofit institutions")%>%
  
  
  rename_vars("pce1$",",Headline PCE-Personal consumption expenditures")%>%
  rename_vars("pce2$",",Goods-Goods")%>%
  rename_vars("pce3$",",Durable goods-Durable goods")%>%
  rename_vars("pce4$",",Durable goods-Motor vehicles and parts")%>%
  rename_vars("pce5$",",Durable goods-New motor vehicles")%>%
  rename_vars("pce6$",",Durable goods-Net purchases of used motor vehicles")%>%
  rename_vars("pce7$",",Durable goods-Motor vehicle parts and accessories")%>%
  rename_vars("pce8$",",Durable goods-Furnishings and durable household equipment")%>%
  rename_vars("pce9$",",Durable goods-Furniture and furnishings")%>%
  rename_vars("pce10$",",Durable goods-Household appliances")%>%
  rename_vars("pce11$",",Durable goods-Glassware, tableware, and household utensils")%>%
  rename_vars("pce12$",",Durable goods-Tools and equipment for house and garden")%>%
  rename_vars("pce13$",",Durable goods-Recreational goods and vehicles")%>%
  rename_vars("pce14$",",Durable goods-Video, audio, photographic, and information processing equipment and media")%>%
  rename_vars("pce15$",",Durable goods-Sporting equipment, supplies, guns, and ammunition")%>%
  rename_vars("pce16$",",Durable goods-Sports and recreational vehicles")%>%
  rename_vars("pce17$",",Durable goods-Recreational books")%>%
  rename_vars("pce18$",",Durable goods-Musical instruments")%>%
  rename_vars("pce19$",",Durable goods-Other durable goods")%>%
  rename_vars("pce20$",",Durable goods-Jewelry and watches")%>%
  rename_vars("pce21$",",Durable goods-Therapeutic appliances and equipment")%>%
  rename_vars("pce22$",",Durable goods-Educational books")%>%
  rename_vars("pce23$",",Durable goods-Luggage and similar personal items")%>%
  rename_vars("pce24$",",Durable goods-Telephone and related communication equipment")%>%
  rename_vars("pce25$",",Non durable goods-Nondurable goods")%>%
  rename_vars("pce26$",",Non durable goods-Food and beverages purchased for off-premises consumption")%>%
  rename_vars("pce27$",",Non durable goods-Food and nonalcoholic beverages purchased for off-premises consumption")%>%
  rename_vars("pce28$",",Non durable goods-Alcoholic beverages purchased for off-premises consumption")%>%
  rename_vars("pce29$",",Non durable goods-Food produced and consumed on farms")%>%
  rename_vars("pce30$",",Non durable goods-Clothing and footwear")%>%
  rename_vars("pce31$",",Non durable goods-Garments")%>%
  rename_vars("pce35$",",Non durable goods-Other clothing materials and footwear")%>%
  rename_vars("pce36$",",Non durable goods-Gasoline and other energy goods")%>%
  rename_vars("pce37$",",Non durable goods-Motor vehicle fuels, lubricants, and fluids")%>%
  rename_vars("pce38$",",Non durable goods-Fuel oil and other fuels")%>%
  rename_vars("pce39$",",Non durable goods-Other nondurable goods")%>%
  rename_vars("pce40$",",Non durable goods-Pharmaceutical and other medical products")%>%
  rename_vars("pce41$",",Non durable goods-Recreational items")%>%
  rename_vars("pce42$",",Non durable goods-Household supplies")%>%
  rename_vars("pce43$",",Non durable goods-Personal care products")%>%
  rename_vars("pce44$",",Non durable goods-Tobacco")%>%
  rename_vars("pce45$",",Non durable goods-Magazines, newspapers, and stationery")%>%
  rename_vars("pce46$",",Services-Services")%>%
  rename_vars("pce47$",",Household services-Household consumption expenditures (for services)")%>%
  rename_vars("pce48$",",Household services-Housing and utilities")%>%
  rename_vars("pce49$",",Household services-Housing")%>%
  rename_vars("pce54$",",Household services-Household utilities")%>%
  rename_vars("pce59$",",Household services-Health care")%>%
  rename_vars("pce60$",",Household services-Outpatient services")%>%
  rename_vars("pce64$",",Household services-Hospital and nursing home services")%>%
  rename_vars("pce67$",",Household services-Transportation services")%>%
  rename_vars("pce68$",",Household services-Motor vehicle services")%>%
  rename_vars("pce71$",",Household services-Public transportation")%>%
  rename_vars("pce75$",",Household services-Recreation services")%>%
  rename_vars("pce76$",",Household services-Membership clubs, sports centers, parks, theaters, and museums")%>%
  rename_vars("pce77$",",Household services-Audio-video, photographic, and information processing equipment services")%>%
  rename_vars("pce78$",",Household services-Gambling")%>%
  rename_vars("pce79$",",Household services-Other recreational services")%>%
  rename_vars("pce80$",",Household services-Food services and accommodations")%>%
  rename_vars("pce81$",",Household services-Food services")%>%
  rename_vars("pce84$",",Household services-Accommodations")%>%
  rename_vars("pce85$",",Household services-Financial services and insurance")%>%
  rename_vars("pce86$",",Household services-Financial services")%>%
  rename_vars("pce89$",",Household services-Insurance")%>%
  rename_vars("pce94$",",Household services-Other services")%>%
  rename_vars("pce95$",",Household services-Communication")%>%
  rename_vars("pce99$",",Household services-Education services")%>%
  rename_vars("pce103$",",Household services-Professional and other services")%>%
  rename_vars("pce104$",",Household services-Personal care and clothing services")%>%
  rename_vars("pce105$",",Household services-Social services and religious activities")%>%
  rename_vars("pce106$",",Household services-Household maintenance")%>%
  rename_vars("pce107$",",Household services-Net foreign travel")%>%
  rename_vars("pce110$",",NPISH-Final consumption expenditures of nonprofit institutions serving households (NPISHs)")%>%
  rename_vars("pce111$",",NPISH-Gross output of nonprofit institutions")%>%
  rename_vars("pce112$",",NPISH-Less: Receipts from sales of goods and services by nonprofit institutions")

return(df)
}

add_not_in_common_column <- function(data, var1, var2) {
  result <- character(nrow(data))
  for (i in seq_along(result)) {
    result[i] <- strings_not_in_common(data[[var1]][i], data[[var2]][i])
  }
  colname <- paste(var1, "_", var2)
  data[[colname]] <- result
  return(data)
}



gen_new_var_condition <- function(df, string, var2, var1) {
  # If var2 does not exist in df, create it as a new column filled with NAs
  if (!(var2 %in% names(df))) {
    df[[var2]] <- NA
  }
  # Replace the observations in var2 with the string if the string appears in var1
  df[[var2]][grepl(string, df[[var1]], ignore.case = TRUE)] <- string
  # Return the modified df
  return(df)
}


combine_vars <- function(df, varA, varB, varC) {
  # Combine the strings in varA and varB with a delimiter "-"
  combined_strings <- paste(df[[varA]], df[[varB]], sep = "-")
  # Add the combined strings as a new variable in the dataframe
  df[[varC]] <- combined_strings
  # Return the modified dataframe
  return(df)
}


# correlation_coeffs ------------------------------------------------------

compute_correlations <- function(analysis, analysis_vars, vecA, vecB) {
  # Split the dataframe by unique values of newtime
  time_groups <- split(analysis, analysis$newtime)
  
  # Create an empty dataframe to store the correlation coefficients
  correlations <- data.frame(time = numeric(),
                             var1 = character(),
                             var2 = character(),
                             cor = numeric(),
                             stringsAsFactors = FALSE)
  
  # Loop through each unique value of newtime
  for (time in unique(analysis$newtime)) {
    # Extract the data for the current value of newtime
    time_data <- time_groups[[as.character(time)]]
    
    # Extract the variables in vecA and vecB from the analysis_vars
    varsA <- intersect(analysis_vars, vecA)
    varsB <- intersect(analysis_vars, vecB)
    
    # Compute the correlation coefficients between pairs of variables in vecA and vecB
    cor_matrix <- cor(time_data[, c(varsA, varsB)])
    
    # Convert the correlation matrix to a long format dataframe
    cor_df <- reshape2::melt(cor_matrix, varnames = c("var1", "var2"), value.name = "cor")
    
    # Filter out the correlations between variables in vecA or vecB
    cor_df <- subset(cor_df, !(var1 %in% varsA & var2 %in% varsA) & !(var1 %in% varsB & var2 %in% varsB))
    
    # Add the time column to the correlation dataframe
    cor_df$time <- time
    
    # Append the correlation dataframe to the main dataframe
    correlations <- rbind(correlations, cor_df)
  }
  
  # Return the final dataframe of correlation coefficients
  return(correlations)
}


# add_prefix --------------------------------------------------------------

add_prefix <- function(varA, prefix) {
  paste0(prefix, varA)
}


# rename_vars -------------------------------------------------------------

rename_vars <- function(data, old_str, new_str) {
  # Find and replace the old string with the new string in variable names
  names(data) <- names(data) %>% 
    str_replace_all(old_str, new_str)
  
  # Return the modified data frame
  return(data)
}

# create dummy -------------------------------------------------------------

#The function takes the data frame and a year as inputs, and adds a new variable 
# "yes_{year}" where "{year}" is the input year. The new variable is 1 if newtime is 
# within the year specified, and 0 otherwise.

add_year_dummy <- function(df, year) {
  year_var_name <- paste0("yes_", year)
  
  y<- year 
  y_1 <- year+1
  
  df$year_var_name<- 0
  
  df <- df %>%
    mutate({{year_var_name}} := ifelse(newtime >= y & newtime <y_1, 1, 0))
  return(df)
}


# create_interaction_vars -------------------------------------------------

create_interaction_vars <- function(df, analysis_vars) {
  for (var in analysis_vars) {
    for (year in 2020:2022) {
      interaction_var_name <- paste0(var, "_", year)
      year_var_name <- paste0("yes_", year)
      
      # Create interaction variable
      df <- df %>% mutate(!!interaction_var_name := .data[[var]] * .data[[year_var_name]])
    }
  }
  
  return(df)
}


# test_yearly_coefficients ------------------------------------------------

test_yearly_coefficients <- function(df, year, variable) {
  results <- data.frame()
  y<- year 
  y_1<- year+1
  # Get unique outcomes
  outcomes <- unique(df$outcome)
  
  # Loop through each unique outcome
  for (outcome in outcomes) {
    # Filter data for a given year, outcome and variable
    subdata <- df %>%
      filter(newtime>=y & newtime<y_1, outcome == outcome, term == variable)
    
    # Skip if there are no observations for the current outcome
    if(nrow(subdata) == 0) {
      next
    }
    
    # Extract coefficients
    coefficients <- subdata$estimate
    
    # Print debugging information
    print(paste("Outcome:", outcome, "| Number of rows in subdata:", nrow(subdata), "| Length of coefficients:", length(coefficients)))
    
    # Perform one-sample t-test (null hypothesis: mean = 0)
    t_test <- t.test(coefficients, mu = 0)
    
    # Extract p-value and confidence interval
    p_value <- t_test$p.value
    conf_int <- t_test$conf.int
    
    # Create a dataframe to store the result
    result_df <- data.frame(
      outcome = outcome,
      p.value = p_value,
      lower_confidence_interval = conf_int[1],
      upper_confidence_interval = conf_int[2],
      significantly_diff_from_zero = ifelse(p_value < 0.05, 'yes', 'no')
    )
    
    # Append the result to the main results dataframe
    results <- rbind(results, result_df)
  }
  
  # Return the dataframe
  return(results)
}

# run_reg -----------------------------------------------------------------


run_reg_pooled <- function(df, x, y, filename) {
  # Only keep unique values of newtime
  results <- data.frame()
  
 
    # Subset the data to only include the current month
    subdata <- df
    
    # Create a logical index of finite observations in y_var
    for (y_var in y) {
      print(paste("Running", y_var))
      # Skip the loop for the current y_var if there are missing, infinite, or NaN values in y
      # if (any(!is.finite(subdata[[y_var]])) & !startsWith(y_var, 'spend_')) {
      #   print(paste("Skipping variable:", y_var))
      #   next
      # }
      # 
      # if (all(!is.finite(subdata[[y_var]])) & startsWith(y_var, 'spend_')) {
      #   print(paste("Skipping variable:", y_var))
      #   next
      # }

      #if (all(!is.finite(subdata[[y_var]]))){
        #subdata<- subdata[!is.na(subdata[[y_var]]), ]
        #subdata <- subdata[!is.infinite(subdata[[y_var]]), ]
      #}
      
      # Run linear regression for each y variable with enough observations
      formula <- as.formula(paste(y_var, paste(x, collapse = "+"), sep = " ~ "))
      model <- lm(formula = formula, data = subdata)
      
      
      # Add the month and variables to the results data frame
      summary <- tidy(model)
      summary$outcome <- y_var
      summary$explanatory<-  paste(x, collapse = "+")
      
      # Add number of observations
      summary$nobs <- nrow(subdata)
      
      # Add R-squared
      summary$rsquared <- summary(model)$r.squared
      
      # Append the results to the main data frame
      results <- rbind(results, summary)
    }
  
  # Save the results data set in the location: output/regression_{explanatory variables}
  write_rds(results, paste0("output/", paste0(filename, c(x), collapse="_"), ".rds"))
  # Return the results data frame
  return(results)
}



run_reg_year_interaction <- function(df, x, y, filename) {
  results <- data.frame()
  
  # Create a list to store problematic datasets
  problematic_datasets <- list()
  
  for (y_var in y) {
    print(paste("Running", y_var))
    
    subdata <- df
    
      
      if (any(!is.finite(subdata[[y_var]]))){
      subdata<- subdata[!is.na(subdata[[y_var]]), ]
      subdata <- subdata[!is.infinite(subdata[[y_var]]), ]
      }
      
    
    # Run linear regression for each y variable with enough observations
    formula <- as.formula(paste(y_var, paste(x, collapse = "+"), sep = " ~ "))
    model <- lm(formula = formula, data = subdata)
    
    summary <- tidy(model)
    summary$outcome <- y_var
    summary$explanatory <- paste(x, collapse = "+")
    summary$nobs <- nrow(subdata)
    summary$rsquared <- summary(model)$r.squared
    
    results <- rbind(results, summary)
  }
  
  write_rds(results, paste0("output/", paste0(filename, c(x), collapse="_"), ".rds"))
  
  # Return the results data frame and the problematic datasets
  return(results)
  #return(list(results = results, problematic_datasets = problematic_datasets))
}


# replace_strings_var2 ----------------------------------------------------
replace_strings_var2 <- function(data, variable, original_string, new_string) {
  if(!variable %in% names(data)) {
    data[[variable]] <- ""
  }
  data[[variable]] <- str_replace_all(data[[variable]], fixed(original_string), new_string)
  return(data)
}


# add_category ------------------------------------------------------------

add_category <- function(df){
  df<- df %>% 
    replace_strings_var("outcome_option_category","outcome_option_category","epop,16-20 year olds ","Employment-to-population ratio")%>%
  replace_strings_var("outcome_option_category","outcome_option_category","epop,21-25 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,26-30 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,31-35 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,36-40 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,41-45 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,46-50 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,51-55 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,56-60 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,61-65 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,66-70 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,71-75 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop,76-80 year olds ","Employment-to-population ratio")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","epop age-sex weighted","Employment-to-population ratio")%>%
    
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,16-20 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,21-25 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,26-30 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,31-35 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,36-40 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,41-45 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,46-50 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,51-55 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,56-60 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,61-65 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,66-70 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,71-75 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp,76-80 year olds ","Labor force participation rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","lfp age-sex weighted","Labor force participation rate")%>%
    
    replace_strings_var("outcome_option_category","outcome_option_category","ur,16-20 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,21-25 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,26-30 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,31-35 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,36-40 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,41-45 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,46-50 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,51-55 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,56-60 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,61-65 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,66-70 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,71-75 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur,76-80 year olds ","Unemployment rate")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","ur age-sex weighted","Unemployment rate")%>%
    
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,all merchants","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,apparel and accessories","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,accomodation and food service","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,arts, entertainment, and recreation","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,general merchandise stores + apparel and accessories","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,general merchandise stores","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,grocery and food stores","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,health care and social assistance","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,home improvement centers","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,sporting goods and hobby","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending,transportation and warehousing","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending durables","Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending durables","Opportunity Insights - Spending")%>%
    
    replace_strings_var("outcome_option_category","outcome_option_category", "spending nondurables","Opportunity Insights - Spending")%>%
    
    replace_strings_var("outcome_option_category","outcome_option_category", "spending inperson" ,"Opportunity Insights - Spending")%>%
    
    replace_strings_var("outcome_option_category","outcome_option_category", "spending inpersonmisc"  ,"Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending remoteservices"    ,"Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending retail_w_grocery"     ,"Opportunity Insights - Spending")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "spending retail_no_grocery"   ,"Opportunity Insights - Spending")%>%
    
    
    replace_strings_var("outcome_option_category","outcome_option_category", "Opportunity Insights - Spending_q1"   ,"Opportunity Insights - Spending by quartile")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "Opportunity Insights - Spending_q2"      ,"Opportunity Insights - Spending by quartile")%>%
    replace_strings_var("outcome_option_category","outcome_option_category","Opportunity Insights - Spending_q3"            ,"Opportunity Insights - Spending by quartile")%>%
    replace_strings_var("outcome_option_category","outcome_option_category", "Opportunity Insights - Spending_q4"   ,"Opportunity Insights - Spending by quartile")%>%
    replace_strings_var("outcome_option_category", "outcome_option_category", "Opportunity Insights - Spending /+ apparel and accessories", "Opportunity Insights - Spending")%>%
    replace_strings_var2("outcome_option_category", "Opportunity Insights - Spending + apparel and accessories","Opportunity Insights - Spending")%>%
    
  replace_strings_var2("outcome_option_category", "Headline PCE-Personal consumption expenditures","PCE")%>%
    replace_strings_var2("outcome_option_category", "Goods-Goods","PCE")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Durable goods","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Motor vehicles and parts","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-New motor vehicles","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Net purchases of used motor vehicles","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Motor vehicle parts and accessories","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Furnishings and durable household equipment","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Furniture and furnishings","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Household appliances","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Glassware tableware and household utensils","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Tools and equipment for house and garden","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Recreational goods and vehicles","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Video audio photographic and information processing equipment and media","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Sporting equipment supplies guns and ammunition","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Sports and recreational vehicles","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Recreational books","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Musical instruments","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Other durable goods","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Jewelry and watches","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Therapeutic appliances and equipment","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Educational books","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Luggage and similar personal items","PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Telephone and related communication equipment","PCE - Durables")%>%
    
    replace_strings_var2("outcome_option_category", "Non durable goods-Nondurable goods","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Food and beverages purchased for off-premises consumption","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Food and nonalcoholic beverages purchased for off-premises consumption","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Alcoholic beverages purchased for off-premises consumption","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Food produced and consumed on farms","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Clothing and footwear","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Garments","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Other clothing materials and footwear","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Gasoline and other energy goods","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Motor vehicle fuels lubricants and fluids","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Fuel oil and other fuels","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Other nondurable goods","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Pharmaceutical and other medical products","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Recreational items","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Household supplies","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Personal care products","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Tobacco","PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Magazines newspapers and stationery","PCE - Non durables")%>%
    
    replace_strings_var2("outcome_option_category", "Services-Services","PCE")%>%
    replace_strings_var2("outcome_option_category", "Household services-Household consumption expenditures (for services)","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Housing and utilities","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Housing","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Household utilities","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Health care","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Outpatient services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Hospital and nursing home services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Transportation services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Motor vehicle services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Public transportation","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Recreation services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Membership clubs sports centers parks theaters and museums","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Audio-video photographic and information processing equipment services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Gambling","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Other recreational services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Food services and accommodations","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Food services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Accommodations","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Financial services and insurance","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Financial services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Insurance","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Other services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Communication","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Education services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Professional and other services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Personal care and clothing services","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Social services and religious activities","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Household maintenance","PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Net foreign travel","PCE - Household services")%>%
    
    replace_strings_var2("outcome_option_category", "NPISH-Final consumption expenditures of nonprofit institutions serving households (NPISHs)","PCE - NPISH")%>%
    replace_strings_var2("outcome_option_category", "NPISH-Gross output of nonprofit institutions","PCE - NPISH")%>%
    replace_strings_var2("outcome_option_category", "NPISH-Less: Receipts from sales of goods and services by nonprofit institutions ","PCE - NPISH")%>%
    
    replace_strings_var2("outcome_option_category", "Durable goods-Glassware, tableware, and household utensils", "PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Video, audio, photographic, and information processing equipment and media" , "PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Durable goods-Sporting equipment, supplies, guns, and ammunition" , "PCE - Durables")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Motor vehicle fuels, lubricants, and fluids", "PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "Household services-Membership clubs, sports centers, parks, theaters, and museums"  , "PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Household services-Audio-video, photographic, and information processing equipment services", "PCE - Household services")%>%
    replace_strings_var2("outcome_option_category", "Non durable goods-Magazines, newspapers, and stationery", "PCE - Non durables")%>%
    replace_strings_var2("outcome_option_category", "NPISH-Less: Receipts from sales of goods and services by nonprofit institutions", "PCE - NPISH") %>%
    
    replace_strings_var2("outcome_option_category", "psh_", "Share of headline")
    
    
  return(df)
}

