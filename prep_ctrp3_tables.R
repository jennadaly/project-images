library(dplyr)
library(datapkg)
library(readxl)
library(reshape)
library(plotly)
library(lubridate)
library(stringr)
library(knitr)
library(kableExtra)
library(tidyr)

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "stops.csv") 
raw_hartford <- dir(path_to_raw, recursive=T, pattern = "^Hartford") 
portal_data <- dir(path_to_raw, recursive=T, pattern = "fixed.csv") 

path_to_new <- (paste0(getwd(), "/", raw_location, "/", "new"))
new_data <- dir(path_to_new, recursive=T, pattern = "stops.csv") 

new_df <- read.csv(paste0(path_to_new, "/", new_data), stringsAsFactors = F, header=T)

raw_orgs <- read.csv(paste0(path_to_raw, "/", "orgs_hash.csv"), stringsAsFactors = F, header=T)
raw_depts <- read.csv(paste0(path_to_raw, "/", "departments_hash.csv"), stringsAsFactors = F, header=T)

orgs_depts <- merge(raw_orgs, raw_depts, by = "department_id", all=T)

#Setup date and time columns
new_df$InterventionYear <- substr(new_df$InterventionDateTime,1,4)
new_df$InterventionMonth <- substr(new_df$InterventionDateTime,6,7)

#Assign FFY
new_df$Federal.Fiscal.Year <- NA
new_df$Federal.Fiscal.Year[which(new_df$InterventionYear == "2013" & 
                                   ((new_df$InterventionMonth == "10" | 
                                       new_df$InterventionMonth == "11" | 
                                       new_df$InterventionMonth == "12")))] <- "2013-2014"
new_df$Federal.Fiscal.Year[which(new_df$InterventionYear == "2014" & 
                                   ((new_df$InterventionMonth == "01" |
                                       new_df$InterventionMonth == "02" |
                                       new_df$InterventionMonth == "03" |
                                       new_df$InterventionMonth == "04" |
                                       new_df$InterventionMonth == "05" |
                                       new_df$InterventionMonth == "06" |
                                       new_df$InterventionMonth == "07" |
                                       new_df$InterventionMonth == "08" |
                                       new_df$InterventionMonth == "09")))] <- "2013-2014"

new_df$Federal.Fiscal.Year[which(new_df$InterventionYear == "2014" & 
                                   ((new_df$InterventionMonth == "10" | 
                                       new_df$InterventionMonth == "11" | 
                                       new_df$InterventionMonth == "12")))] <- "2014-2015"
new_df$Federal.Fiscal.Year[which(new_df$InterventionYear == "2015" & 
                                   ((new_df$InterventionMonth == "01" |
                                       new_df$InterventionMonth == "02" |
                                       new_df$InterventionMonth == "03" |
                                       new_df$InterventionMonth == "04" |
                                       new_df$InterventionMonth == "05" |
                                       new_df$InterventionMonth == "06" |
                                       new_df$InterventionMonth == "07" |
                                       new_df$InterventionMonth == "08" |
                                       new_df$InterventionMonth == "09")))] <- "2014-2015"

new_df$Federal.Fiscal.Year[which(new_df$InterventionYear == "2015" & 
                                   ((new_df$InterventionMonth == "10" | 
                                       new_df$InterventionMonth == "11" | 
                                       new_df$InterventionMonth == "12")))] <- "2015-2016"
new_df$Federal.Fiscal.Year[which(new_df$InterventionYear == "2016" & 
                                   ((new_df$InterventionMonth == "01" |
                                       new_df$InterventionMonth == "02" |
                                       new_df$InterventionMonth == "03" |
                                       new_df$InterventionMonth == "04" |
                                       new_df$InterventionMonth == "05" |
                                       new_df$InterventionMonth == "06" |
                                       new_df$InterventionMonth == "07" |
                                       new_df$InterventionMonth == "08" |
                                       new_df$InterventionMonth == "09" |
                                       new_df$InterventionMonth == "10")))] <- "2015-2016"

#Clean up columns for processing
###################################################################################################################

new_df$OrganizationIdentificationID <- trimws(new_df$OrganizationIdentificationID)
new_df <- merge(new_df, orgs_depts, by.x = "OrganizationIdentificationID", by.y = "org_id", all.x=T)

#Rename codes to names
new_df$SubjectRaceCode[which(new_df$SubjectRaceCode == "W")] <- "White"
new_df$SubjectRaceCode[which(new_df$SubjectRaceCode == "B")] <- "Black" 
new_df$SubjectRaceCode[which(new_df$SubjectRaceCode == "A")] <- "Asian Pacific" 
new_df$SubjectRaceCode[which(new_df$SubjectRaceCode == "I")] <- "American Indian / Alaska Native" 
new_df$SubjectRaceCode[which(new_df$SubjectRaceCode == "")] <- "Unknown" 

new_df$SubjectEthnicityCode[which(new_df$SubjectEthnicityCode == "H")] <- "Hispanic"
new_df$SubjectEthnicityCode[which(new_df$SubjectEthnicityCode == "N")] <- "Non-Hispanic"
new_df$SubjectEthnicityCode[which(new_df$SubjectEthnicityCode == "M")] <- "Non-Hispanic"
new_df$SubjectEthnicityCode[which(new_df$SubjectEthnicityCode == "")] <- "Unknown"

new_df$SubjectSexCode[which(new_df$SubjectSexCode == "M")] <- "Male"
new_df$SubjectSexCode[which(new_df$SubjectSexCode == "F")] <- "Female"
new_df$SubjectSexCode[which(new_df$SubjectSexCode == "")] <- "Unknown"

new_df$InterventionReasonCode[which(new_df$InterventionReasonCode == "V")] <- "Motor Vehicle Violation"
new_df$InterventionReasonCode[which(new_df$InterventionReasonCode == "I")] <- "Investigative Stop"
new_df$InterventionReasonCode[which(new_df$InterventionReasonCode == "E")] <- "Equipment Violation"
new_df$InterventionReasonCode[which(new_df$InterventionReasonCode == "")] <- "Unknown"

new_df$InterventionTechniqueCode[which(new_df$InterventionTechniqueCode == "B")] <- "Blind"
new_df$InterventionTechniqueCode[which(new_df$InterventionTechniqueCode == "G")] <- "General"
new_df$InterventionTechniqueCode[which(new_df$InterventionTechniqueCode == "S")] <- "Spot"
new_df$InterventionTechniqueCode[which(new_df$InterventionTechniqueCode == "")] <- "Unknown"

new_df$SearchAuthorizationCode[which(new_df$SearchAuthorizationCode == "C")] <- "Consent"
new_df$SearchAuthorizationCode[which(new_df$SearchAuthorizationCode == "I")] <- "Inventory"
new_df$SearchAuthorizationCode[which(new_df$SearchAuthorizationCode == "N")] <- "Not Applicable"
new_df$SearchAuthorizationCode[which(new_df$SearchAuthorizationCode == "O")] <- "Other"
new_df$SearchAuthorizationCode[which(new_df$SearchAuthorizationCode == "")] <- "Unknown"

new_df$InterventionDispositionCode[which(new_df$InterventionDispositionCode == "I")] <- "Ticket"
new_df$InterventionDispositionCode[which(new_df$InterventionDispositionCode == "I ")] <- "Ticket"
new_df$InterventionDispositionCode[which(new_df$InterventionDispositionCode == "M")] <- "Misdemeanor Summons"
new_df$InterventionDispositionCode[which(new_df$InterventionDispositionCode == "N")] <- "No Disposition"
new_df$InterventionDispositionCode[which(new_df$InterventionDispositionCode == "U")] <- "Uniform Arrest Report"
new_df$InterventionDispositionCode[which(new_df$InterventionDispositionCode == "V")] <- "Verbal Warning"
new_df$InterventionDispositionCode[which(new_df$InterventionDispositionCode == "W")] <- "Written Warning"
new_df$InterventionDispositionCode[which(new_df$InterventionDispositionCode == "")] <- "Unknown"

new_df$StatutoryReasonForStop <- trimws(new_df$StatutoryReasonForStop)

#should be empty
blanks <- new_df[new_df$InterventionYear == "",]
NAs <- new_df[is.na(new_df$Federal.Fiscal.Year),]

#TOTAL STOPS BY REASON FOR STOP
###########################################################################################################################################
#convert NaN to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

new_df$StatutoryReasonForStop <- trimws(new_df$StatutoryReasonForStop)

totals <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode) %>% 
  group_by(Federal.Fiscal.Year) %>% 
  summarize(`Total White Stops` = sum(SubjectRaceCode == "White"),
            `Total Black Stops` = sum(SubjectRaceCode == "Black"),
            `Total Asian Pacific Stops` = sum(SubjectRaceCode == "Asian Pacific"),
            `Total American Indian / Alaska Native Stops` = sum(SubjectRaceCode == "American Indian / Alaska Native"), 
            `Total Hispanic Stops` = sum(SubjectEthnicityCode == "Hispanic"),
            `Total Non-Hispanic Stops` = sum(SubjectEthnicityCode == "Non-Hispanic"), 
            `Total Unknown Race Stops` = sum(SubjectRaceCode == "Unknown"),
            `Total Unknown Ethnicity Stops` = sum(SubjectEthnicityCode == "Unknown")) %>% 
  mutate(`Total Stops` = (`Total White Stops` + `Total Black Stops` + `Total Asian Pacific Stops` + `Total American Indian / Alaska Native Stops` + `Total Unknown Race Stops`), 
         `Total Non-White Stops` = (`Total Black Stops`) + (`Total Asian Pacific Stops`) + (`Total American Indian / Alaska Native Stops`))

# Write to File
write.table(
  totals,
  file.path(getwd(), "ctrp3-race-plot", "static", "data", "totals-plot.csv"),
  sep = ",",
  row.names = F
)

write.table(
  totals,
  file.path(getwd(), "ctrp3-ethncity-plot", "static", "data", "totals-plot.csv"),
  sep = ",",
  row.names = F
)

reason_no_code <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode, StatutoryReasonForStop ) %>% 
  group_by(StatutoryReasonForStop, Federal.Fiscal.Year) %>% 
  summarise(`Total White` = sum(SubjectRaceCode == "White"), 
            `Total Black` = sum(SubjectRaceCode == "Black"),
            `Total Asian Pacific` = sum(SubjectRaceCode == "Asian Pacific"),
            `Total American Indian / Alaska Native` = sum(SubjectRaceCode == "American Indian / Alaska Native"), 
            `Total Unknown Race` = sum(SubjectRaceCode == "Unknown"),
            `Total Unknown Ethnicity` = sum(SubjectEthnicityCode == "Unknown"),
            `Total Hispanic` = sum(SubjectEthnicityCode == "Hispanic"),
            `Total Non-Hispanic` = sum(SubjectEthnicityCode == "Non-Hispanic")) %>% 
  mutate(`Total Stops Per Reason` = (`Total White` + `Total Black` + `Total Asian Pacific` + `Total American Indian / Alaska Native` + `Total Unknown Race`), 
         `Total Non-White` = (`Total Black`) + (`Total Asian Pacific`) + (`Total American Indian / Alaska Native`))

reason_no_code_with_totals <- merge(totals, reason_no_code, by = "Federal.Fiscal.Year", all=T)

reason_no_code_with_totals_calc <- reason_no_code_with_totals %>% 
  mutate(`Percent Total of that type of Reason` = (round((`Total Stops Per Reason` / `Total Stops`)*100, 2)), 
         `Percent White` = round((`Total White` / `Total Stops Per Reason`)*100, 2) ,
         `Percent Black` = round((`Total Black` / `Total Stops Per Reason`)*100, 2) ,
         `Percent Asian Pacific` = round((`Total Asian Pacific` / `Total Stops Per Reason`)*100, 2) ,
         `Percent American Indian / Alaska Native` = round((`Total American Indian / Alaska Native` / `Total Stops Per Reason`)*100, 2),
         `Percent Non-White` = round((`Total Non-White` / `Total Stops Per Reason`)*100, 2),
         `Percent Unknown Race` = round((`Total Unknown Race` / `Total Stops Per Reason`)*100, 2),
         `Percent Unknown Ethnicity` = round((`Total Unknown Ethnicity` / `Total Stops Per Reason`)*100, 2),
         `Percent Hispanic` = round((`Total Hispanic` / `Total Stops Per Reason`)*100, 2),
         `Percent Non-Hispanic` = round((`Total Non-Hispanic` / `Total Stops Per Reason`)*100, 2),
         #within given race/ethnicity, percent of each type of reason
         `Pct Reason - White` = round((`Total White` / `Total White Stops`)*100, 2),
         `Pct Reason - Black` = round((`Total Black` / `Total Black Stops`)*100, 2),
         `Pct Reason - Asian Pacific` = round((`Total Asian Pacific` / `Total Asian Pacific Stops`)*100, 2),
         `Pct Reason - American Indian / Alaska Native` = round((`Total American Indian / Alaska Native` / `Total American Indian / Alaska Native Stops`)*100, 2), 
         `Pct Reason - Non-White` = round((`Total Non-White` / `Total Non-White Stops`)*100,2), 
         `Pct Reason - Unknown Race` = round((`Total Unknown Race` / `Total Unknown Race Stops`)*100,2), 
         `Pct Reason - Unknown Ethnicity` = round((`Total Unknown Ethnicity` / `Total Unknown Ethnicity Stops`)*100,2), 
         `Pct Reason - Hispanic` = round((`Total Hispanic` / `Total Hispanic Stops`)*100,2), 
         `Pct Reason - Non-Hispanic` = round((`Total Non-Hispanic` / `Total Non-Hispanic Stops`)*100,2) 
  )

reason_no_code_with_totals_calc[is.nan(reason_no_code_with_totals_calc)] <- 0

reason_percent_totals <- reason_no_code_with_totals_calc %>% 
  select(Federal.Fiscal.Year, StatutoryReasonForStop, `Percent Total of that type of Reason`)

(tidyr)
reason_percent_totals <- spread(reason_percent_totals, StatutoryReasonForStop, `Percent Total of that type of Reason`)

reason_percent_totals[is.na(reason_percent_totals)] <- 0

reason_number_totals <- reason_no_code_with_totals_calc %>% 
  select(Federal.Fiscal.Year, StatutoryReasonForStop, `Total Stops Per Reason`)

reason_number_totals <- spread(reason_number_totals, StatutoryReasonForStop, `Total Stops Per Reason`)

reason_number_totals[is.na(reason_number_totals)] <- 0

reasons_no_code_final <- merge(reason_number_totals, reason_percent_totals, by = "Federal.Fiscal.Year")

# Write to File
write.table(
  reasons_no_code_final,
  file.path(getwd(), "ctrp3-reason-nocode-plot", "static", "data", "reason-nocode-plot.csv"),
  sep = ",",
  row.names = F
)
              

reason_percent_totals_table <- reason_percent_totals

names(reason_percent_totals_table) <- gsub(x = names(reason_percent_totals_table), pattern = "\\.", replacement = " ")

reason_percent_totals_table$`Administrative Offense`  <- paste0(reason_percent_totals_table$`Administrative Offense` , "%")  
reason_percent_totals_table$`Cell Phone`              <- paste0(reason_percent_totals_table$`Cell Phone`           , "%") 
reason_percent_totals_table$`Defective Lights`        <- paste0(reason_percent_totals_table$`Defective Lights`     , "%")  
reason_percent_totals_table$`Display of Plates`       <- paste0(reason_percent_totals_table$`Display of Plates`   , "%")  
reason_percent_totals_table$`Equipment Violation`     <- paste0(reason_percent_totals_table$`Equipment Violation`  , "%")  
reason_percent_totals_table$`Moving Violation`        <- paste0(reason_percent_totals_table$`Moving Violation`    , "%")   
reason_percent_totals_table$`Other`                   <- paste0(reason_percent_totals_table$`Other`              , "%")    
reason_percent_totals_table$`Registration`            <- paste0(reason_percent_totals_table$`Registration`      , "%")    
reason_percent_totals_table$`Seatbelt`                <- paste0(reason_percent_totals_table$`Seatbelt`          , "%")     
reason_percent_totals_table$`Speed Related`           <- paste0(reason_percent_totals_table$`Speed Related`     , "%")     
reason_percent_totals_table$`STC Violation`           <- paste0(reason_percent_totals_table$`STC Violation`     , "%")     
reason_percent_totals_table$`Stop Sign`               <- paste0(reason_percent_totals_table$`Stop Sign`         , "%")     
reason_percent_totals_table$`Suspended License`       <- paste0(reason_percent_totals_table$`Suspended License`   , "%")  
reason_percent_totals_table$`Traffic Control Signal`  <- paste0(reason_percent_totals_table$`Traffic Control Signal` , "%")
reason_percent_totals_table$`Unlicensed Operation`    <- paste0(reason_percent_totals_table$`Unlicensed Operation`   , "%")
reason_percent_totals_table$`Window Tint`             <- paste0(reason_percent_totals_table$`Window Tint`, "%")

reason_percent_totals_table <- reason_percent_totals_table %>% 
  select (`Federal Fiscal Year`, `Speed Related`, `Cell Phone`, `Registration`, `Defective Lights`, `Moving Violation`, `Traffic Control Signal`, `Stop Sign`, `Seatbelt`, `Display of Plates`, `Suspended License`, `Window Tint`, `Equipment Violation`, `Other`, `STC Violation`, `Administrative Offense`, `Unlicensed Operation`)

# Write to File
write.table(
  reason_percent_totals_table,
  file.path(getwd(), "ctrp3-reason-nocode-table", "static", "data", "reason-nocode-table.csv"),
  sep = ",",
  row.names = F
)           
              
reason_with_code <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode, InterventionReasonCode ) %>% 
  group_by(InterventionReasonCode, Federal.Fiscal.Year) %>% 
  summarise(`Total White` = sum(SubjectRaceCode == "White"), 
            `Total Black` = sum(SubjectRaceCode == "Black"),
            `Total Asian Pacific` = sum(SubjectRaceCode == "Asian Pacific"),
            `Total American Indian / Alaska Native` = sum(SubjectRaceCode == "American Indian / Alaska Native"), 
            `Total Unknown Race` = sum(SubjectRaceCode == "Unknown"),
            `Total Unknown Ethnicity` = sum(SubjectEthnicityCode == "Unknown"),
            `Total Hispanic` = sum(SubjectEthnicityCode == "Hispanic"),
            `Total Non-Hispanic` = sum(SubjectEthnicityCode == "Non-Hispanic")) %>% 
  mutate(`Total Stops Per Reason` = (`Total White` + `Total Black` + `Total Asian Pacific` + `Total American Indian / Alaska Native` + `Total Unknown Race`), 
         `Total Non-White` = (`Total Black`) + (`Total Asian Pacific`) + (`Total American Indian / Alaska Native`))

reason_with_code_with_totals <- merge(totals, reason_with_code, by = "Federal.Fiscal.Year", all=T)

reason_with_code_with_totals_calc <- reason_with_code_with_totals %>% 
  mutate(`Percent Total of that type of Reason` = (round((`Total Stops Per Reason` / `Total Stops`)*100, 2)), 
         `Percent White` = round((`Total White` / `Total Stops Per Reason`)*100, 2) ,
         `Percent Black` = round((`Total Black` / `Total Stops Per Reason`)*100, 2) ,
         `Percent Asian Pacific` = round((`Total Asian Pacific` / `Total Stops Per Reason`)*100, 2) ,
         `Percent American Indian / Alaska Native` = round((`Total American Indian / Alaska Native` / `Total Stops Per Reason`)*100, 2),
         `Percent Unknown Race` = round((`Total Unknown Race` / `Total Stops Per Reason`)*100, 2),
         `Percent Unknown Ethnicity` = round((`Total Unknown Ethnicity` / `Total Stops Per Reason`)*100, 2),
         `Percent Hispanic` = round((`Total Hispanic` / `Total Stops Per Reason`)*100, 2),
         `Percent Non-Hispanic` = round((`Total Non-Hispanic` / `Total Stops Per Reason`)*100, 2),
         #within given race/ethnicity, percent of each type of reason
         `Pct Reason - White` = round((`Total White` / `Total White Stops`)*100, 2),
         `Pct Reason - Black` = round((`Total Black` / `Total Black Stops`)*100, 2),
         `Pct Reason - Asian Pacific` = round((`Total Asian Pacific` / `Total Asian Pacific Stops`)*100, 2),
         `Pct Reason - American Indian / Alaska Native` = round((`Total American Indian / Alaska Native` / `Total American Indian / Alaska Native Stops`)*100, 2), 
         `Pct Reason - Non-White` = round((`Total Non-White` / `Total Non-White Stops`)*100,2), 
         `Pct Reason - Unknown Race` = round((`Total Unknown Race` / `Total Unknown Race Stops`)*100,2), 
         `Pct Reason - Unknown Ethnicity` = round((`Total Unknown Ethnicity` / `Total Unknown Ethnicity Stops`)*100,2), 
         `Pct Reason - Hispanic` = round((`Total Hispanic` / `Total Hispanic Stops`)*100,2), 
         `Pct Reason - Non-Hispanic` = round((`Total Non-Hispanic` / `Total Non-Hispanic Stops`)*100,2) 
  )

#convert NaN to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

reason_with_code_with_totals_calc[is.nan(reason_with_code_with_totals_calc)] <- 0

reason_with_code_percent_totals <- reason_with_code_with_totals_calc %>% 
  select(Federal.Fiscal.Year, InterventionReasonCode, `Percent Total of that type of Reason`)

reason_with_code_percent_totals <- spread(reason_with_code_percent_totals, InterventionReasonCode, `Percent Total of that type of Reason`)

reason_with_code_percent_totals[is.na(reason_with_code_percent_totals)] <- 0

reason_with_code_number_totals <- reason_with_code_with_totals_calc %>% 
  select(Federal.Fiscal.Year, InterventionReasonCode, `Total Stops Per Reason`)

reason_with_code_number_totals <- spread(reason_with_code_number_totals, InterventionReasonCode, `Total Stops Per Reason`)

reason_with_code_number_totals[is.na(reason_with_code_number_totals)] <- 0


reason_with_code_final <- merge(reason_with_code_number_totals, reason_with_code_percent_totals, by = "Federal.Fiscal.Year")

# Write to File
write.table(
  reason_with_code_final,
  file.path(getwd(), "ctrp3-reason-withcode-plot", "static", "data", "reason-withcode-plot.csv"),
  sep = ",",
  row.names = F
)         


reason_with_code_percent_totals_table <- reason_with_code_percent_totals

names(reason_with_code_percent_totals_table) <- gsub(x = names(reason_with_code_percent_totals_table), pattern = "\\.", replacement = " ")

reason_with_code_percent_totals_table$`Equipment Violation`  <- paste0(reason_with_code_percent_totals_table$`Equipment Violation` , "%")  
reason_with_code_percent_totals_table$`Investigative Stop`              <- paste0(reason_with_code_percent_totals_table$`Investigative Stop`           , "%") 
reason_with_code_percent_totals_table$`Motor Vehicle Violation`        <- paste0(reason_with_code_percent_totals_table$`Motor Vehicle Violation`     , "%")  

reason_with_code_percent_totals_table <- reason_with_code_percent_totals_table %>% 
  select(`Federal Fiscal Year`, `Motor Vehicle Violation`, `Equipment Violation`, `Investigative Stop`)

write.table(
  reason_with_code_percent_totals_table,
  file.path(getwd(), "ctrp3-reason-withcode-table", "static", "data", "reason-withcode-table.csv"),
  sep = ",",
  row.names = F
)

#PLOT SUB-CATEGORIES WITHIN EACH INTERVENTION CATEGORY (Equipment Violation, Investigative Stop, Motor Vehicle Violation)

reason_with_both <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode, InterventionReasonCode, StatutoryReasonForStop ) %>% 
  group_by(InterventionReasonCode, StatutoryReasonForStop, Federal.Fiscal.Year) %>% 
  summarise(`Total White` = sum(SubjectRaceCode == "White"), 
            `Total Black` = sum(SubjectRaceCode == "Black"),
            `Total Asian Pacific` = sum(SubjectRaceCode == "Asian Pacific"),
            `Total American Indian / Alaska Native` = sum(SubjectRaceCode == "American Indian / Alaska Native"), 
            `Total Unknown Race` = sum(SubjectRaceCode == "Unknown"),
            `Total Unknown Ethnicity` = sum(SubjectEthnicityCode == "Unknown"),
            `Total Hispanic` = sum(SubjectEthnicityCode == "Hispanic"),
            `Total Non-Hispanic` = sum(SubjectEthnicityCode == "Non-Hispanic")) %>% 
  mutate(`Total Stops Per Reason` = (`Total White` + `Total Black` + `Total Asian Pacific` + `Total American Indian / Alaska Native` + `Total Unknown Race`), 
         `Total Non-White` = (`Total Black`) + (`Total Asian Pacific`) + (`Total American Indian / Alaska Native`))

reason_with_both_all_races <- select(reason_with_both, StatutoryReasonForStop, InterventionReasonCode, Federal.Fiscal.Year, `Total Stops Per Reason`)

equip_violation3 <- reason_with_both_all_races[reason_with_both_all_races$Federal.Fiscal.Year == "2015-2016" & 
                                                 reason_with_both_all_races$InterventionReasonCode == "Equipment Violation",]

mv_violation3 <- reason_with_both_all_races[reason_with_both_all_races$Federal.Fiscal.Year == "2015-2016" & 
                                              reason_with_both_all_races$InterventionReasonCode == "Motor Vehicle Violation",]

investigative3 <- reason_with_both_all_races[reason_with_both_all_races$Federal.Fiscal.Year == "2015-2016" & 
                                               reason_with_both_all_races$InterventionReasonCode == "Investigative Stop",]

write.table(
  mv_violation3,
  file.path(getwd(), "ctrp3-mv-tree", "static", "data", "mv-tree.csv"),
  sep = ",",
  row.names = F
)

write.table(
  equip_violation3,
  file.path(getwd(), "ctrp3-equip-tree", "static", "data", "equip-tree.csv"),
  sep = ",",
  row.names = F
)

write.table(
  investigative3,
  file.path(getwd(), "ctrp3-invest-tree", "static", "data", "invest-tree.csv"),
  sep = ",",
  row.names = F
)

#OUTCOME BY RACE/ETHNICITY
##################################################################################################################################################

outcomes_by_RE <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode, InterventionDispositionCode ) %>% 
  group_by(InterventionDispositionCode, Federal.Fiscal.Year) %>% 
  summarise(`Total White` = sum(SubjectRaceCode == "White"), 
            `Total Black` = sum(SubjectRaceCode == "Black"),
            `Total Asian Pacific` = sum(SubjectRaceCode == "Asian Pacific"),
            `Total American Indian / Alaska Native` = sum(SubjectRaceCode == "American Indian / Alaska Native"), 
            `Total Unknown Race` = sum(SubjectRaceCode == "Unknown"), 
            `Total Hispanic` = sum(SubjectEthnicityCode == "Hispanic"),
            `Total Non-Hispanic` = sum(SubjectEthnicityCode == "Non-Hispanic"), 
            `Total Unknown Ethnicity` = sum(SubjectEthnicityCode == "Unknown")) %>% 
  mutate(`Total Stops Per Intervention` = (`Total White` + `Total Black` + `Total Asian Pacific` + `Total American Indian / Alaska Native` + `Total Unknown Race`), 
         `Total Non-White` = (`Total Black`) + (`Total Asian Pacific`) + (`Total American Indian / Alaska Native`))

outcomes_with_totals <- merge(totals, outcomes_by_RE, by = "Federal.Fiscal.Year", all=T)

outcomes_with_totals_calc <- outcomes_with_totals %>% 
  mutate(`Percent Total of that type of Stop` = (round((`Total Stops Per Intervention` / `Total Stops`)*100, 2)), 
         `Percent White` = round((`Total White` / `Total Stops Per Intervention`)*100, 2) ,
         `Percent Black` = round((`Total Black` / `Total Stops Per Intervention`)*100, 2) ,
         `Percent Asian Pacific` = round((`Total Asian Pacific` / `Total Stops Per Intervention`)*100, 2) ,
         `Percent American Indian / Alaska Native` = round((`Total American Indian / Alaska Native` / `Total Stops Per Intervention`)*100, 2), 
         `Percent Non-White` = round((`Total Non-White` / `Total Stops Per Intervention`)*100,2),
         `Percent Unknown Race` = round((`Total Unknown Race` / `Total Stops Per Intervention`)*100,2),
         `Percent Unknown Ethnicity` = round((`Total Unknown Ethnicity` / `Total Stops Per Intervention`)*100,2),
         `Percent Hispanic` = round((`Total Hispanic` / `Total Stops Per Intervention`)*100,2),
         `Percent Non-Hispanic` = round((`Total Non-Hispanic` / `Total Stops Per Intervention`)*100,2),
         #within given race/ethnicity, percent of each type of intervention
         `Pct Intervention - White` = round((`Total White` / `Total White Stops`)*100, 2),
         `Pct Intervention - Black` = round((`Total Black` / `Total Black Stops`)*100, 2),
         `Pct Intervention - Asian Pacific` = round((`Total Asian Pacific` / `Total Asian Pacific Stops`)*100, 2),
         `Pct Intervention - American Indian / Alaska Native` = round((`Total American Indian / Alaska Native` / `Total American Indian / Alaska Native Stops`)*100, 2), 
         `Pct Intervention - Non-White` = round((`Total Non-White` / `Total Non-White Stops`)*100,2), 
         `Pct Intervention - Unknown Race` = round((`Total Unknown Race` / `Total Unknown Race Stops`)*100,2), 
         `Pct Intervention - Unknown Ethnicity` = round((`Total Unknown Ethnicity` / `Total Unknown Ethnicity Stops`)*100,2), 
         `Pct Intervention - Hispanic` = round((`Total Hispanic` / `Total Hispanic Stops`)*100,2), 
         `Pct Intervention - Non-Hispanic` = round((`Total Non-Hispanic` / `Total Non-Hispanic Stops`)*100,2) 
  )

#convert NaN to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

outcomes_with_totals_calc[is.nan(outcomes_with_totals_calc)] <- 0
#######################################################################################################

percent_totals <- outcomes_with_totals_calc %>% 
  select(Federal.Fiscal.Year, InterventionDispositionCode, `Percent Total of that type of Stop`)

percent_totals <- spread(percent_totals, InterventionDispositionCode, `Percent Total of that type of Stop`)

number_totals <- outcomes_with_totals_calc %>% 
  select(Federal.Fiscal.Year, InterventionDispositionCode, `Total Stops Per Intervention`)

number_totals <- spread(number_totals, InterventionDispositionCode, `Total Stops Per Intervention`)

outcomes <- merge(percent_totals, number_totals, by = "Federal.Fiscal.Year")

write.table(
  outcomes,
  file.path(getwd(), "ctrp3-outcome-plot", "static", "data", "outcome-plot.csv"),
  sep = ",",
  row.names = F
)
     

outcomes_table <- outcomes[,1:8]
names(outcomes_table) <- gsub(".x", "", names(outcomes_table))
names(outcomes_table)[names(outcomes_table) == "Federal.Fiscal.Year"] <- "Federal Fiscal Year"
outcomes_table <- select(outcomes_table, `Federal Fiscal Year`, Ticket, `Verbal Warning`, `Written Warning`, `Misdemeanor Summons`, `No Disposition`, `Uniform Arrest Report`)

outcomes_table$Ticket <- paste0(outcomes_table$Ticket, "%")
outcomes_table$`Misdemeanor Summons` <- paste0(outcomes_table$`Misdemeanor Summons`, "%")
outcomes_table$`No Disposition` <- paste0(outcomes_table$`No Disposition`, "%")
outcomes_table$`Uniform Arrest Report` <- paste0(outcomes_table$`Uniform Arrest Report`, "%")
outcomes_table$`Verbal Warning` <- paste0(outcomes_table$`Verbal Warning`, "%")
outcomes_table$`Written Warning` <- paste0(outcomes_table$`Written Warning`, "%")

write.table(
  outcomes_table,
  file.path(getwd(), "ctrp3-outcome-table", "static", "data", "outcome-table.csv"),  
  sep = ",",
  row.names = F
)         

outcomes_for_total_race_plot <- outcomes_with_totals_calc %>% 
  select(Federal.Fiscal.Year, InterventionDispositionCode, 
         `Total White`, 
         `Total Black`, 
         `Total Asian Pacific`,
         `Total American Indian / Alaska Native`,
         `Total Non-White`,
         `Percent White`, 
         `Percent Black`, 
         `Percent Asian Pacific`,
         `Percent American Indian / Alaska Native`,
         `Percent Non-White`) 

outcomes_for_total_ethnicity_plot <- outcomes_with_totals_calc %>% 
  select(Federal.Fiscal.Year, InterventionDispositionCode, 
         `Total Hispanic`, 
         `Total Non-Hispanic`,
         `Percent Hispanic`, 
         `Percent Non-Hispanic`) 

outcomes_race_avg <- outcomes_for_total_race_plot %>% 
  group_by(InterventionDispositionCode) %>% 
  mutate(`Avg Pct White` = round(mean(`Percent White`), 2), 
         `Avg Pct Black` = round(mean(`Percent Black`), 2), 
         `Avg Pct Asian Pacific` = round(mean(`Percent Asian Pacific`), 2), 
         `Avg Pct American Indian / Alaska Native` = round(mean(`Percent American Indian / Alaska Native`), 2), 
         `Avg Pct Non-White` = round(mean(`Percent Non-White`), 2), 
         `Avg Num White` = round(mean(`Total White`), 2), 
         `Avg Num Black` = round(mean(`Total Black`), 2), 
         `Avg Num Asian Pacific` = round(mean(`Total Asian Pacific`), 2), 
         `Avg Num American Indian / Alaska Native` = round(mean(`Total American Indian / Alaska Native`), 2), 
         `Avg Num Non-White` = round(mean(`Total Non-White`), 2)
  )

outcomes_ethnicity_avg <- outcomes_for_total_ethnicity_plot %>% 
  group_by(InterventionDispositionCode) %>% 
  mutate(`Avg Pct Hispanic` = round(mean(`Percent Hispanic`), 2), 
         `Avg Pct Non-Hispanic` = round(mean(`Percent Non-Hispanic`), 2), 
         `Avg Num Hispanic` = round(mean(`Total Hispanic`), 2), 
         `Avg Num Non-Hispanic` = round(mean(`Total Non-Hispanic`), 2)
  )

outcomes_per_race_intervention_plot <- unique(outcomes_race_avg %>% 
                                                select(InterventionDispositionCode, 
                                                       `Avg Pct White`, `Avg Pct Black`, `Avg Pct Asian Pacific`, `Avg Pct American Indian / Alaska Native`, `Avg Pct Non-White`,
                                                       `Avg Num White`, `Avg Num Black`, `Avg Num Asian Pacific`, `Avg Num American Indian / Alaska Native`, `Avg Num Non-White`))

outcomes_per_race_intervention_plot$`Avg Num White` <- round(outcomes_per_race_intervention_plot$`Avg Num White`, 0)
outcomes_per_race_intervention_plot$`Avg Num Black`  <- round(outcomes_per_race_intervention_plot$`Avg Num Black`, 0)
outcomes_per_race_intervention_plot$`Avg Num Asian Pacific` <- round(outcomes_per_race_intervention_plot$ `Avg Num Asian Pacific`, 0)
outcomes_per_race_intervention_plot$`Avg Num American Indian / Alaska Native`  <- round(outcomes_per_race_intervention_plot$`Avg Num American Indian / Alaska Native`, 0)
outcomes_per_race_intervention_plot$`Avg Num Non-White` <- round(outcomes_per_race_intervention_plot$`Avg Num Non-White`, 0)

outcomes_per_race_intervention_plot <- outcomes_per_race_intervention_plot[outcomes_per_race_intervention_plot$InterventionDispositionCode != "Unknown",]

outcomes_per_race_intervention_plot <- outcomes_per_race_intervention_plot %>% 
  arrange(`Avg Num White`)

write.table(
  outcomes_per_race_intervention_plot,
  file.path(getwd(), "ctrp3-intervention-race-plot", "static", "data", "intervention-race-plot.csv"),  
  sep = ",",
  row.names = F
)

outcomes_per_ethnicity_intervention_plot <- unique(outcomes_ethnicity_avg %>% 
                                                     select(InterventionDispositionCode, 
                                                            `Avg Pct Hispanic`, `Avg Pct Non-Hispanic`, 
                                                            `Avg Num Hispanic`, `Avg Num Non-Hispanic`))

outcomes_per_ethnicity_intervention_plot$`Avg Num Hispanic` <- round(outcomes_per_ethnicity_intervention_plot$`Avg Num Hispanic`, 0)
outcomes_per_ethnicity_intervention_plot$`Avg Num Non-Hispanic`  <- round(outcomes_per_ethnicity_intervention_plot$`Avg Num Non-Hispanic`, 0)

outcomes_per_ethnicity_intervention_plot <- outcomes_per_ethnicity_intervention_plot[outcomes_per_ethnicity_intervention_plot$InterventionDispositionCode != "Unknown",]


outcomes_per_ethnicity_intervention_plot <- outcomes_per_ethnicity_intervention_plot %>% 
  arrange(`Avg Num Hispanic`)

write.table(
  outcomes_per_ethnicity_intervention_plot,
  file.path(getwd(), "ctrp3-intervention-ethnicity-plot", "static", "data", "intervention-ethnicity-plot.csv"),  
  sep = ",",
  row.names = F
)

total_race_table_outcome <- outcomes_per_race_intervention_plot

total_race_table_outcome <- total_race_table_outcome %>% 
  select(InterventionDispositionCode, 
         `Avg Num White`, `Avg Pct White`,
         `Avg Num Non-White`, `Avg Pct Non-White`, 
         `Avg Num Black`, `Avg Pct Black`, 
         `Avg Num Asian Pacific`, `Avg Pct Asian Pacific`, 
         `Avg Num American Indian / Alaska Native`, `Avg Pct American Indian / Alaska Native`) 


names(total_race_table_outcome)[names(total_race_table_outcome) == "InterventionDispositionCode"] <- "Traffic Stop Outcome"
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Num White"] <- "Total White"                           
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Num Black"] <- "Total Black"                             
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Num Asian Pacific"] <- "Total Asian Pacific"            
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Num American Indian / Alaska Native"] <- "Total American Indian / Alaska Native"
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Num Non-White"] <- "Total Non-White"                   
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Pct White"] <- "Percent White (%)"                     
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Pct Black"] <- "Percent Black (%)"                    
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Pct Asian Pacific"] <- "Percent Asian Pacific (%)"     
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Pct American Indian / Alaska Native"] <- "Percent American Indian / Alaska Native (%)" 
names(total_race_table_outcome)[names(total_race_table_outcome) == "Avg Pct Non-White"] <- "Percent Non-White (%)"               

total_ethnicity_table_outcome <- outcomes_per_ethnicity_intervention_plot

total_ethnicity_table_outcome <- total_ethnicity_table_outcome %>% 
  select(InterventionDispositionCode, 
         `Avg Num Hispanic`, `Avg Pct Hispanic`,
         `Avg Num Non-Hispanic`, `Avg Pct Non-Hispanic`)

names(total_ethnicity_table_outcome)[names(total_ethnicity_table_outcome) == "InterventionDispositionCode"] <- "Traffic Stop Outcome"
names(total_ethnicity_table_outcome)[names(total_ethnicity_table_outcome) == "Avg Num Hispanic"] <- "Total Hispanic"                           
names(total_ethnicity_table_outcome)[names(total_ethnicity_table_outcome) == "Avg Num Non-Hispanic"] <- "Total Non-Hispanic"                           
names(total_ethnicity_table_outcome)[names(total_ethnicity_table_outcome) == "Avg Pct Hispanic"] <- "Percent Hispanic (%)"                  
names(total_ethnicity_table_outcome)[names(total_ethnicity_table_outcome) == "Avg Pct Non-Hispanic"] <- "Percent Non-Hispanic (%)"



write.table(
  total_race_table_outcome,
  file.path(getwd(), "ctrp3-race-outcome-table", "static", "data", "race-outcome-table.csv"),  
  sep = ",",
  row.names = F
)

write.table(
  total_ethnicity_table_outcome,
  file.path(getwd(), "ctrp3-ethnicity-outcome-table", "static", "data", "ethnicity-outcome-table.csv"),  
  sep = ",",
  row.names = F
)

reason_for_total_race_plot <- reason_no_code_with_totals_calc %>% 
  select(Federal.Fiscal.Year, StatutoryReasonForStop, 
         `Total White`, 
         `Total Black`, 
         `Total Asian Pacific`,
         `Total American Indian / Alaska Native`,
         `Total Non-White`,
         `Percent White`, 
         `Percent Black`, 
         `Percent Asian Pacific`,
         `Percent American Indian / Alaska Native`,
         `Percent Non-White`) 

reason_for_total_ethnicity_plot <- reason_no_code_with_totals_calc %>% 
  select(Federal.Fiscal.Year, StatutoryReasonForStop, 
         `Total Hispanic`, 
         `Total Non-Hispanic`,
         `Percent Hispanic`, 
         `Percent Non-Hispanic`) 

reason_race_avg <- reason_for_total_race_plot %>% 
  group_by(StatutoryReasonForStop) %>% 
  mutate(`Avg Pct White` = round(mean(`Percent White`), 2), 
         `Avg Pct Black` = round(mean(`Percent Black`), 2), 
         `Avg Pct Asian Pacific` = round(mean(`Percent Asian Pacific`), 2), 
         `Avg Pct American Indian / Alaska Native` = round(mean(`Percent American Indian / Alaska Native`), 2), 
         `Avg Pct Non-White` = round(mean(`Percent Non-White`), 2), 
         `Avg Num White` = round(mean(`Total White`), 2), 
         `Avg Num Black` = round(mean(`Total Black`), 2), 
         `Avg Num Asian Pacific` = round(mean(`Total Asian Pacific`), 2), 
         `Avg Num American Indian / Alaska Native` = round(mean(`Total American Indian / Alaska Native`), 2), 
         `Avg Num Non-White` = round(mean(`Total Non-White`), 2)
  )

reason_ethnicity_avg <- reason_for_total_ethnicity_plot %>% 
  group_by(StatutoryReasonForStop) %>% 
  mutate(`Avg Pct Hispanic` = round(mean(`Percent Hispanic`), 2), 
         `Avg Pct Non-Hispanic` = round(mean(`Percent Non-Hispanic`), 2), 
         `Avg Num Hispanic` = round(mean(`Total Hispanic`), 2), 
         `Avg Num Non-Hispanic` = round(mean(`Total Non-Hispanic`), 2)
  )

reason_per_race_intervention_plot <- unique(reason_race_avg %>% 
                                              select(StatutoryReasonForStop, 
                                                     `Avg Pct White`, `Avg Pct Black`, `Avg Pct Asian Pacific`, `Avg Pct American Indian / Alaska Native`, `Avg Pct Non-White`,
                                                     `Avg Num White`, `Avg Num Black`, `Avg Num Asian Pacific`, `Avg Num American Indian / Alaska Native`, `Avg Num Non-White`))

reason_per_race_intervention_plot$`Avg Num White` <- round(reason_per_race_intervention_plot$`Avg Num White`, 0)
reason_per_race_intervention_plot$`Avg Num Black`  <- round(reason_per_race_intervention_plot$`Avg Num Black`, 0)
reason_per_race_intervention_plot$`Avg Num Asian Pacific` <- round(reason_per_race_intervention_plot$ `Avg Num Asian Pacific`, 0)
reason_per_race_intervention_plot$`Avg Num American Indian / Alaska Native`  <- round(reason_per_race_intervention_plot$`Avg Num American Indian / Alaska Native`, 0)
reason_per_race_intervention_plot$`Avg Num Non-White` <- round(reason_per_race_intervention_plot$`Avg Num Non-White`, 0)


reason_per_race_intervention_plot <- reason_per_race_intervention_plot %>% 
  arrange(`Avg Num White`)


reason_per_ethnicity_intervention_plot <- unique(reason_ethnicity_avg %>% 
                                                   select(StatutoryReasonForStop, 
                                                          `Avg Pct Hispanic`, `Avg Pct Non-Hispanic`, 
                                                          `Avg Num Hispanic`, `Avg Num Non-Hispanic`))

reason_per_ethnicity_intervention_plot$`Avg Num Hispanic` <- round(reason_per_ethnicity_intervention_plot$`Avg Num Hispanic`, 0)
reason_per_ethnicity_intervention_plot$`Avg Num Non-Hispanic`  <- round(reason_per_ethnicity_intervention_plot$`Avg Num Non-Hispanic`, 0)

reason_per_ethnicity_intervention_plot <- reason_per_ethnicity_intervention_plot %>% 
  arrange(`Avg Num Hispanic`)

write.table(
  reason_per_race_intervention_plot,
  file.path(getwd(), "ctrp3-reason-race-plot", "static", "data", "reason-race-plot.csv"),  
  sep = ",",
  row.names = F
)

write.table(
  reason_per_ethnicity_intervention_plot,
  file.path(getwd(), "ctrp3-reason-ethnicity-plot", "static", "data", "reason-ethnicity-plot.csv"),  
  sep = ",",
  row.names = F
)

searches <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode, VehicleSearchedIndicator) %>% 
  group_by(Federal.Fiscal.Year) %>% 
  summarise(`Total White Stops` = sum(SubjectRaceCode == "White"), 
            `Total Black Stops` = sum(SubjectRaceCode == "Black"),
            `Total Asian Pacific Stops` = sum(SubjectRaceCode == "Asian Pacific"),
            `Total American Indian / Alaska Native Stops` = sum(SubjectRaceCode == "American Indian / Alaska Native"), 
            `Total Unknown Race Stops` = sum(SubjectRaceCode == "Unknown"),
            `Total Unknown Ethnicity Stops` = sum(SubjectEthnicityCode == "Unknown"),
            `Total Hispanic Stops` = sum(SubjectEthnicityCode == "Hispanic"),
            `Total Non-Hispanic Stops` = sum(SubjectEthnicityCode == "Non-Hispanic")) %>% 
  mutate(`Total Non-White Stops` =   (`Total Black Stops`)+(`Total Asian Pacific Stops`)+(`Total American Indian / Alaska Native Stops`))

searches_with_RE <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode, VehicleSearchedIndicator) %>% 
  group_by(Federal.Fiscal.Year, VehicleSearchedIndicator) %>%  
  summarise(`Total White Searches` = sum(SubjectRaceCode == "White" & VehicleSearchedIndicator == "True"), 
            `Total Black Searches` = sum(SubjectRaceCode == "Black" & VehicleSearchedIndicator == "True"),
            `Total Asian Pacific Searches` = sum(SubjectRaceCode == "Asian Pacific" & VehicleSearchedIndicator == "True"),
            `Total American Indian / Alaska Native Searches` = sum(SubjectRaceCode == "American Indian / Alaska Native" & VehicleSearchedIndicator == "True"), 
            `Total Hispanic Searches` = sum(SubjectEthnicityCode == "Hispanic" & VehicleSearchedIndicator == "True"), 
            `Total Non-Hispanic Searches` = sum(SubjectEthnicityCode == "Non-Hispanic" & VehicleSearchedIndicator == "True"),
            `Total Unknown Race Searches` = sum(SubjectRaceCode == "Unknown" & VehicleSearchedIndicator == "True"),
            `Total Unknown Ethnicity Searches` = sum(SubjectEthnicityCode == "Unknown" & VehicleSearchedIndicator == "True")) %>% 
  mutate(`Total Non-White Searches` =   (`Total Black Searches`)+(`Total Asian Pacific Searches`)+(`Total American Indian / Alaska Native Searches`))

searches_with_RE <- searches_with_RE[!is.na(searches_with_RE$VehicleSearchedIndicator) & searches_with_RE$VehicleSearchedIndicator != "False" & 
                                       searches_with_RE$VehicleSearchedIndicator != "",]

searches_with_RE$VehicleSearchedIndicator <- NULL

searches_totals <- merge(searches, searches_with_RE, by = "Federal.Fiscal.Year")

no_searches_with_RE <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode, VehicleSearchedIndicator) %>% 
  group_by(Federal.Fiscal.Year, VehicleSearchedIndicator) %>%  
  summarise(`Total White No Searches` = sum(SubjectRaceCode == "White" & VehicleSearchedIndicator == "False"), 
            `Total Black No Searches` = sum(SubjectRaceCode == "Black" & VehicleSearchedIndicator == "False"),
            `Total Asian Pacific No Searches` = sum(SubjectRaceCode == "Asian Pacific" & VehicleSearchedIndicator == "False"),
            `Total American Indian / Alaska Native No Searches` = sum(SubjectRaceCode == "American Indian / Alaska Native" & VehicleSearchedIndicator == "False"), 
            `Total Hispanic No Searches` = sum(SubjectEthnicityCode == "Hispanic" & VehicleSearchedIndicator == "False"), 
            `Total Non-Hispanic No Searches` = sum(SubjectEthnicityCode == "Non-Hispanic" & VehicleSearchedIndicator == "False"),
            `Total Unknown Race No Searches` = sum(SubjectRaceCode == "Unknown" & VehicleSearchedIndicator == "False"),
            `Total Unknown Ethnicity No Searches` = sum(SubjectEthnicityCode == "Unknown" & VehicleSearchedIndicator == "False")) %>% 
  mutate(`Total Non-White No Searches` =   (`Total Black No Searches`)+(`Total Asian Pacific No Searches`)+(`Total American Indian / Alaska Native No Searches`))            

no_searches_with_RE <- no_searches_with_RE[!is.na(no_searches_with_RE$VehicleSearchedIndicator) & no_searches_with_RE$VehicleSearchedIndicator != "True" & no_searches_with_RE$VehicleSearchedIndicator != "",]

no_searches_with_RE$VehicleSearchedIndicator <- NULL

searches_totals <- merge(searches_totals, no_searches_with_RE, by = "Federal.Fiscal.Year")

searches_totals <- searches_totals %>% 
  mutate(`Total Stops With Searches` = (`Total White Searches` + `Total Black Searches` + `Total Asian Pacific Searches` + `Total American Indian / Alaska Native Searches` + `Total Unknown Race Searches`), 
         `Total Stops` = (`Total White Stops` + `Total Black Stops` + `Total Asian Pacific Stops` + `Total American Indian / Alaska Native Stops` + `Total Unknown Race Stops`), 
         `Total Stops No Searches` = (`Total Stops` - `Total Stops With Searches`))

searches_pie_plot <- searches_totals[,c(1,29:31)]

searches_pie_plot <- searches_pie_plot %>% 
  mutate(`Percent Stops With Searches` = round((`Total Stops With Searches` / `Total Stops`)*100,1), 
         `Percent Stops No Searches` = round((`Total Stops No Searches` / `Total Stops`)*100,1)) 

searches_pie_plot <- gather(searches_pie_plot, Topic, Value, `Percent Stops With Searches`:`Percent Stops No Searches`, factor_key=TRUE)

searches_pie_plot <- searches_pie_plot[,c(1,5,6)]

searches_pie_plot <- searches_pie_plot %>% 
  group_by(Topic) %>% 
  summarise(Value = round(mean(Value), 1))

write.table(
  searches_pie_plot,
  file.path(getwd(), "ctrp3-searches-pie", "static", "data", "searches-pie.csv"),  
  sep = ",",
  row.names = F
)

searches_by_race <- searches_totals[,c(1:19, 29, 30)]

searches_by_race <- searches_by_race %>% 
  mutate(`Percent Searched - White` = round((`Total White Searches`/`Total Stops With Searches`)*100, 2), 
         `Percent Searched - Black` = round((`Total Black Searches`/`Total Stops With Searches`)*100, 2),
         `Percent Searched - Asian Pacific` = round((`Total Asian Pacific Searches`/`Total Stops With Searches`)*100, 2),
         `Percent Searched - American Indian / Alaska Native Searches` = round((`Total American Indian / Alaska Native Searches`/`Total Stops With Searches`)*100, 2), 
         `Percent Searched - Unknown Race` = round((`Total Unknown Race Searches`/`Total Stops With Searches`)*100, 2), 
         `Percent Searched - Unknown Ethnicity` = round((`Total Unknown Ethnicity Searches`/`Total Stops With Searches`)*100, 2), 
         `Percent Searched - Hispanic` = round((`Total Hispanic Searches`/`Total Stops With Searches`)*100, 2), 
         `Percent Searched - Non-Hispanic` = round((`Total Non-Hispanic Searches`/`Total Stops With Searches`)*100, 2), 
         `Percent Searched - Non-White` = round((`Total Non-White Searches`/`Total Stops With Searches`)*100, 2), 
         `Percent White Searched` = round((`Total White Searches`/`Total White Stops`)*100, 2), 
         `Percent Black Searched` = round((`Total Black Searches`/`Total Black Stops`)*100, 2), 
         `Percent Asian Pacific Searched` = round((`Total Asian Pacific Searches`/`Total Asian Pacific Stops`)*100, 2), 
         `Percent American Indian / Alaska Native Searched` = round((`Total American Indian / Alaska Native Searches`/`Total American Indian / Alaska Native Stops`)*100, 2), 
         `Percent Unknown Race Searched` = round((`Total Unknown Race Searches`/`Total Unknown Race Stops`)*100, 2), 
         `Percent Unknown Ethnicity Searched` = round((`Total Unknown Ethnicity Searches`/`Total Unknown Ethnicity Stops`)*100, 2), 
         `Percent Hispanic Searched` = round((`Total Hispanic Searches`/`Total Hispanic Stops`)*100, 2), 
         `Percent Non-Hispanic Searched` = round((`Total Non-Hispanic Searches`/`Total Non-Hispanic Stops`)*100, 2), 
         `Percent Non-White Searched` = round((`Total Non-White Searches`/`Total Non-White Stops`)*100, 2)
  )

searches_by_race[is.nan(searches_by_race)] <- 0

write.table(
  searches_by_race,
  file.path(getwd(), "ctrp3-searches-race-plot", "static", "data", "searches.csv"),  
  sep = ",",
  row.names = F
)

write.table(
  searches_by_race,
  file.path(getwd(), "ctrp3-searches-ethnicity-plot", "static", "data", "searches.csv"),  
  sep = ",",
  row.names = F
)

searches_with_contraband <- new_df %>% 
  select(Federal.Fiscal.Year, SubjectRaceCode, SubjectEthnicityCode, VehicleSearchedIndicator, ContrabandIndicator) %>% 
  group_by(ContrabandIndicator, VehicleSearchedIndicator, Federal.Fiscal.Year) %>% 
  summarise(`Total White` = sum(SubjectRaceCode == "White"), 
            `Total Black` = sum(SubjectRaceCode == "Black"),
            `Total Asian Pacific` = sum(SubjectRaceCode == "Asian Pacific"),
            `Total American Indian / Alaska Native` = sum(SubjectRaceCode == "American Indian / Alaska Native"), 
            `Total Hispanic` = sum(SubjectEthnicityCode == "Hispanic"),
            `Total Non-Hispanic` = sum(SubjectEthnicityCode == "Non-Hispanic"), 
            `Total Unknown Race` = sum(SubjectRaceCode == "Unknown"),
            `Total Unknown Ethnicity` = sum(SubjectEthnicityCode == "Unknown")) %>% 
  mutate(`Total Stops Per Search Outcome` = (`Total White` + `Total Black` + `Total Asian Pacific` + `Total American Indian / Alaska Native` + `Total Unknown Race`), 
         `Total Non-White` = (`Total Black`) + (`Total Asian Pacific`) + (`Total American Indian / Alaska Native`))


searches_with_contraband <- searches_with_contraband[!is.na(searches_with_contraband$ContrabandIndicator) & searches_with_contraband$ContrabandIndicator == "True" & searches_with_contraband$VehicleSearchedIndicator == "True",]

searches_with_contraband <- merge(searches_with_contraband, searches_with_RE, by = "Federal.Fiscal.Year")

searches_with_contraband <- searches_with_contraband %>% 
  mutate(`Percent White with Contraband` = round((`Total White`/`Total White Searches`)*100, 2), 
         `Percent Black with Contraband` = round((`Total Black`/`Total Black Searches`)*100, 2),          
         `Percent Asian Pacific with Contraband` = round((`Total Asian Pacific`/`Total Asian Pacific Searches`)*100, 2),  
         `Percent American Indian / Alaska Native with Contraband` = round((`Total American Indian / Alaska Native`/`Total American Indian / Alaska Native Searches`)*100, 2),  
         `Percent Hispanic with Contraband` = round((`Total Hispanic`/`Total Hispanic Searches`)*100, 2),
         `Percent Non-Hispanic with Contraband` = round((`Total Non-Hispanic`/`Total Non-Hispanic Searches`)*100, 2),
         `Percent Unknown Race with Contraband` = round((`Total Unknown Race`/`Total Unknown Race Searches`)*100, 2),
         `Percent Unknown Ethnicity with Contraband` = round((`Total Unknown Ethnicity`/`Total Unknown Ethnicity Searches`)*100, 2),
         `Percent Non-White with Contraband` = round((`Total Non-White`/`Total Non-White Searches`)*100, 2)
  )

searches_with_contraband[is.nan(searches_with_contraband)] <- 0

write.table(
  searches_with_contraband,
  file.path(getwd(), "ctrp3-contraband-race-plot", "static", "data", "contraband.csv"),  
  sep = ",",
  row.names = F
)

write.table(
  searches_with_contraband,
  file.path(getwd(), "ctrp3-contraband-ethnicity-plot", "static", "data", "contraband.csv"),  
  sep = ",",
  row.names = F
)
