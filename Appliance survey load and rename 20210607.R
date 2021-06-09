# Load rebate data, rename columns, load libraries

library(ggplot2)
library(gdata) # To reorder factors
library(plyr) # To map factor values
library(grDevices) # To save transparent graphics
library(scales) # To have commas in y-axis labels
library(gridExtra) # To alter axis tics

setwd("~/Documents/Python/Appliance survey")
appliance_survey_results_init <- read.csv("California_rebate_survey-most recent_October 22, 2017_First_100.csv", 
                                          header = FALSE, row.names = NULL, skip = 3)
appliance_survey_results_second <- read.csv("California_rebate_survey-second round_December 8, 2017_12.20.csv", 
                                            header = FALSE, row.names = NULL, skip = 5)

# Ensure second dataset has the same number of columns as the first
appliance_survey_results_second <- appliance_survey_results_second[, 1:95]

# Set new column names
new_colnames_second <- c(colnames(appliance_survey_results_second[, 1:17]),
                         "have_appliance", #Q42
                         "bought_appliance", #Q45
                         "rebate", #Q3
                         "knew_rebate", #Q37
                         "knew_rebate_description",#38
                         "age_AC",#25#1_3
                         "age_Dryer",#25#1_33
                         "age_Washer",#25#1_37
                         "age_Water_Heater",#25#1_6
                         "age_Pool_Pump",#25#1_7
                         "age_Refrigerator",#25#1_2
                         "old_AC",#25#2_3
                         "old_Dryer",#25#2_33
                         "old_Washer",#25#2_37
                         "old_Water_Heater",#25#2_6
                         "old_Pool_Pump",#25#2_7
                         "old_Refrigerator",#25#2_2
                         "money_AC",#25#3_3
                         "money_Dryer",#25#3_33
                         "money_Washer",#25#3_37
                         "money_Water_Heater",#25#3_6
                         "money_Pool_Pump",#25#3_7
                         "money_Refrigerator",#25#3_2
                         "timing_AC",#25#4_3
                         "timing_Dryer",#25#4_33
                         "timing_Washer",#25#4_6
                         "timing_Water_Heater",#25#4_6
                         "timing_Pool_Pump",#25#4_7
                         "timing_Refrigerator",#25#4_2
                         "got_AC",#25#5_3
                         "got_Dryer",#25#5_33
                         "got_Washer",#25#5_37
                         "got_Water_Heater",#25#5_6
                         "got_Pool_Pump",#25#5_7
                         "got_Refrigerator",#25#5_2
                         "did_AC",#34#1_3
                         "did_Dryer",#34#1_33
                         "did_Washer",#34#1_37
                         "did_Water_Heater",#34#1_6
                         "did_Pool_Pump", #34#1_7
                         "did_Refrigerator", #34#1_2
                         "Rent/Own", #36
                         "Rent/Own_Other", #36_3_TEXT
                         "Moved?", #30
                         "Moved_year", #31
                         "Appliance_age", #39
                         "Renovation", #48
                         "Renovation_when", #49
                         "Renovation_rebate", #50
                         "Renovation_rebate_size", #51,
                         "Renovation_rebate_payment", #53
                         "Renovation_description", #55
                         "Utility", #17
                         "Utility_other", #18
                         "Utility_programs_PGE", #9
                         "Utility_other_PGE", #32
                         "CARE_length_PGE", #47
                         "Utility_programs_SCE", #31
                         "Utility_other_SCE", #33
                         "CARE_length_SCE", #48
                         "Utility_programs_SDGE", #34
                         "Utility_other_SDGE", #35
                         "CARE_length_SDGE", #49
                         "Pays_bill", #19
                         "Pays_bill_text", #20
                         "Age", #37
                         "Gender", #38
                         "Race", #39
                         "Income", #49
                         "Education", #44
                         "HHld_size", #41
                         "HHld_size_change", #56
                         "When_HHld_change", #58
                         "Prev_HHld_size", #57
                         "ZIP", #42
                         "Open", #43
                         "MTurk_ID", #43
                         "MTurk_Code")

# Colnames for first 100 results
new_colnames_init <- c(colnames(appliance_survey_results_init[, 1:17]),
                       "have_appliance", #Q42
                       "bought_appliance", #Q45
                       "rebate", #Q3
                       "knew_rebate", #Q37
                       "knew_rebate_description",#38
                       "age_AC",#25#1_3
                       "age_Dryer",#25#1_33
                       "age_Washer",#25#1_37
                       "age_Water_Heater",#25#1_6
                       "age_Pool_Pump",#25#1_7
                       "age_Refrigerator",#25#1_2
                       "old_AC",#25#2_3
                       "old_Dryer",#25#2_33
                       "old_Washer",#25#2_37
                       "old_Water_Heater",#25#2_6
                       "old_Pool_Pump",#25#2_7
                       "old_Refrigerator",#25#2_2
                       "money_AC",#25#3_3
                       "money_Dryer",#25#3_33
                       "money_Washer",#25#3_37
                       "money_Water_Heater",#25#3_6
                       "money_Pool_Pump",#25#3_7
                       "money_Refrigerator",#25#3_2
                       "timing_AC",#25#4_3
                       "timing_Dryer",#25#4_33
                       "timing_Washer",#25#4_6
                       "timing_Water_Heater",#25#4_6
                       "timing_Pool_Pump",#25#4_7
                       "timing_Refrigerator",#25#4_2
                       "got_AC",#25#5_3
                       "got_Dryer",#25#5_33
                       "got_Washer",#25#5_37
                       "got_Water_Heater",#25#5_6
                       "got_Pool_Pump",#25#5_7
                       "got_Refrigerator",#25#5_2
                       "did_AC",#34#1_3
                       "did_Dryer",#34#1_33
                       "did_Washer",#34#1_37
                       "did_Water_Heater",#34#1_6
                       "did_Pool_Pump", #34#1_7
                       "did_Refrigerator", #34#1_2
                       "Rent/Own", #36
                       "Rent/Own_Other", #36_3_TEXT
                       "Moved?", #30
                       "Moved_year", #31
                       "Appliance_age", #39
                       "Utility", #17
                       "Utility_other", #18
                       "Utility_programs_PGE", #9
                       "Utility_other_PGE", #32
                       "CARE_length_PGE", #47
                       "Utility_programs_SCE", #31
                       "Utility_other_SCE", #33
                       "CARE_length_SCE", #48
                       "Utility_programs_SDGE", #34
                       "Utility_other_SDGE", #35
                       "CARE_length_SDGE", #49
                       "Pays_bill", #19
                       "Pays_bill_text", #20
                       "Age", #37
                       "Gender", #38
                       "Race", #39
                       "Income", #49
                       "Education", #44
                       "HHld_size", #41
                       "ZIP", #42
                       "Open", #43
                       "MTurk_ID", #43
                       "MTurk_Code")

# Rename columns in main panel
colnames(appliance_survey_results_second) <- new_colnames_second

# Rename columns in main panel
colnames(appliance_survey_results_init) <- new_colnames_init

# Drop the first seven rows of _init, as they have invalid MTurk IDs
appliance_survey_results_init <- appliance_survey_results_init[-c(1:7), ]

# Drop incomplete survey results from _second
appliance_survey_results_second <- appliance_survey_results_second[
  appliance_survey_results_second$MTurk_ID != "", ]

# Add missing columns to _init
appliance_survey_results_init[, c("HHld_size_change", #56,
                                  "When_HHld_change", #58
                                  "Prev_HHld_size", #57
                                  "Renovation", #48
                                  "Renovation_when", #49
                                  "Renovation_rebate", #50
                                  "Renovation_rebate_size", #51,
                                  "Renovation_rebate_payment", #53
                                  "Renovation_description" )] <- NA

# Merge data frames
appliance_survey_results <- rbind(appliance_survey_results_second, 
                                  appliance_survey_results_init)

# Utility abbreviation
appliance_survey_results$UtilityAbbr <- factor(appliance_survey_results$Utility, 
                                               levels = 
                                                 levels(appliance_survey_results$Utility), labels = c("", "LADWP", "Other", "PG&E", "SMUD", "SDG&E", "SCE"))
# Total sample size
sample_size <- dim(appliance_survey_results)[1]

# Appliance types
appliance.types <- c("Refrigerator", "Window Air Conditioner", "Clothes Dryer", "Clothes Washer", "Water Heater", "Pool pump/motor")

# Create variable for any rebate
appliance_survey_results$any_rebate <- appliance_survey_results$rebate != ""

# Moved year grouped
appliance_survey_results$Moved_year_grouped <- factor(unlist(sapply(appliance_survey_results$Moved_year,
                                                                    function(x) {if (x %in% 2017) 
                                                                    {"Less than 1 year"} else if (x %in% c(2014, 2015, 2016))
                                                                    {"1-3 years"} else if (x %in% c(2011, 2012, 2013))
                                                                    {"4-6 years"} else if (x %in% c(2007, 2008, 2009, 2010))
                                                                    {"7-10 years"} else ""})))

# Household changed year grouped
appliance_survey_results$When_HHld_change_grouped <- factor(unlist(sapply(appliance_survey_results$When_HHld_change,
                                                                          function(x) {if (x %in% 2017) 
                                                                          {"Less than 1 year"} else if (x %in% c(2014, 2015, 2016))
                                                                          {"1-3 years"} else if (x %in% c(2011, 2012, 2013))
                                                                          {"4-6 years"} else if (x %in% c(2007, 2008, 2009, 2010))
                                                                          {"7-10 years"} else ""})))

# Change in household size
appliance_survey_results$HHld_size_delta <- as.numeric(appliance_survey_results[, "HHld_size"]) -  as.numeric(appliance_survey_results[, "HHld_size_change"])

# Timing of increases and decreases in household size
appliance_survey_results$When_HHld_increase_grouped <-  
  factor(levels(appliance_survey_results$When_HHld_change_grouped)
         [ unlist(sapply(rownames(appliance_survey_results), 
                         function (x) 
                         {if (is.na(appliance_survey_results[x, "HHld_size_delta"])) 
                         {NA} else if (appliance_survey_results[x, "HHld_size_delta"] > 0)  
                         {appliance_survey_results[x, "When_HHld_change_grouped"]} 
                           else NA}))])

appliance_survey_results$When_HHld_decrease_grouped <-  
  factor(levels(appliance_survey_results$When_HHld_change_grouped)
         [ unlist(sapply(rownames(appliance_survey_results), 
                         function (x) 
                         {if (is.na(appliance_survey_results[x, "HHld_size_delta"])) 
                         {NA} else if (appliance_survey_results[x, "HHld_size_delta"] < 0)  
                         {appliance_survey_results[x, "When_HHld_change_grouped"]} 
                           else NA}))])

# Create program summary statistics
appliance_survey_results$all_programs <- paste(appliance_survey_results$Utility_programs_PGE,
                                               appliance_survey_results$Utility_programs_SCE,
                                               appliance_survey_results$Utility_programs_SDGE, sep = "")
appliance_survey_results$CARE <- grepl("California Alternate Rates for Energy", appliance_survey_results$all_programs, fixed=TRUE)
appliance_survey_results$ESAP <- grepl("Energy Savings Assistance Program", appliance_survey_results$all_programs, fixed=TRUE)