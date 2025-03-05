# Clean and Format Raw SPIDER Export for Downstream Analysis

library(tidyverse)
library(lubridate)
source('C:/Lead Systems Epi/LISDW/R Scripts/DateFormatter.R')

prep_spider <- function(df_raw, start_date = '1900-01-01', end_date = '2100-01-01', include_unknown_geo = T, max_status = 3){
  
  # unknown geo = 999. create a conditional filter
  if(include_unknown_geo){
    max_fips = 1000}
  else{
    max_fips = 998}
  
  
  if(any(is.na(df_raw$cstatus))){
    print('THERE ARE BLANK CSTATUS ENTRIED. FIX BEFORE MOVING ON')
    stop()
  }
  else{
    print('No Blank cstatus detected')
  }
  
  df_raw %>%
    # Replace NA cstatus with 9
    #replace_na(list('cstatus' = 9)) %>%
    # Format all date columns as ymd
    mutate(
      across((all_of(c('tfirstexp', 'tlastexp', 'tonset', 'dreport'))), date_formatter)) %>%
    # Create a date column from dreport when populated, tfirstexp when not
    mutate(
      date = case_when(
        is.na(dreport) ~ tfirstexp,
        TRUE ~ dreport),
      # Create an Event and EventDesc from ceventdesc without removing ceventdesc
      Event = str_extract(string = ceventdesc, pattern = '^[:digit:]+(?=[:space:]|_)'),
      EventDesc = str_extract(string = ceventdesc, pattern = '(?<=_|[:space:]{1}).*')
      ) %>%
    filter(
       date >=  start_date, #'2019-01-01', #date
       date <= end_date, #'2021-12-31', # date
      # Use event geography, not exposure - it's much more complete
      ceventfips <= max_fips,
      cstatus <= max_status
    ) %>%
    dplyr::select(dreport, everything()) %>% #date
    rowwise() %>%
    mutate(`Time-3Year` = paste(year(ymd(start_date)), year(ymd(end_date)), sep = '-'),
           ## add variable number of 0s so that all fips codes are 11 digits
           `Geo-County` = case_when(floor(log10(ceventfips) + 1) == 1 ~
                                      paste('5300', ceventfips, '000000', sep = ''),
                                    floor(log10(ceventfips) + 1) == 2 ~
                                      paste('530', ceventfips, '000000', sep = ''),
                                    floor(log10(ceventfips) + 1) == 3 ~
                                      paste('53', ceventfips, '000000', sep = '')))
    }


## Prep SPIDER export from file name instead of from data.frame
prep_spider_from_file <- function(filename, start_date = '1900-01-01', end_date = '2100-01-01', include_unknown_geo = T, max_status = 3){
  
  # unknown geo = 999. create a conditional filter
  if(include_unknown_geo){
    max_fips = 1000}
  else{
    max_fips = 998}
  
  df_raw <- read.csv(filename, colClasses = c('regnr1' = 'character', 'regnr2' = 'character', 'regnr3' = 'character', 'regnr4' = 'character', 'regnr5' = 'character', 'ai11' = 'character', 'ai12' = 'character', 'ai13' = 'character', 'ai14' = 'character', 'ai21' = 'character', 'ai22' = 'character', 'ai23' = 'character', 'ai24' = 'character', 'ai31' = 'character', 'ai32' = 'character', 'ai33' = 'character', 'ai34' = 'character', 'ai41' = 'character', 'ai42' = 'character', 'ai43' = 'character', 'ai44' = 'character', 'distnr1' = 'character', 'distnr2' = 'character', 'distnr3' = 'character', 'distnr4' = 'character', 'g1' = 'character', 'g2' = 'character', 'g3' = 'character', 'g4' = 'character', 'o1' = 'character', 'o2' = 'character', 'o3' = 'character', 'o4' = 'character')) 
  
  if(any(is.na(df_raw$cstatus))){
    print('THERE ARE BLANK CSTATUS ENTRIED. FIX BEFORE MOVING ON')
    stop()
  }
  else{
    print('No Blank cstatus detected')
  }
  
  df_raw %>%
    # Replace NA cstatus with 9
    replace_na(list('cstatus' = 9)) %>%
    # Format all date columns as ymd
    mutate(
      across((all_of(c('tfirstexp', 'tlastexp', 'tonset', 'dreport'))), date_formatter)) %>%
    # Create a date column from dreport when populated, tfirstexp when not
    mutate(
      date = case_when(
        is.na(dreport) ~ tfirstexp,
        TRUE ~ dreport),
      # Create an Event and EventDesc from ceventdesc without removing ceventdesc
      Event = str_extract(string = ceventdesc, pattern = '^[:digit:]+(?=[:space:])'),
      EventDesc = str_extract(string = ceventdesc, pattern = '(?<=[:space:]{1}).*')
    ) %>%
    filter(
      date >=  start_date, #'2019-01-01', #date
      date <= end_date, #'2021-12-31', # date
      # Use event geography, not exposure - it's much more complete
      ceventfips <= max_fips,
      cstatus <= max_status
    ) %>%
    dplyr::select(dreport, everything()) %>% #date
    rowwise() %>%
    mutate(`Time-3Year` = paste(year(ymd(start_date)), year(ymd(end_date)), sep = '-'),
           ## add variable number of 0s so that all fips codes are 11 digits
           `Geo-County` = case_when(floor(log10(ceventfips) + 1) == 1 ~
                                      paste('5300', ceventfips, '000000', sep = ''),
                                    floor(log10(ceventfips) + 1) == 2 ~
                                      paste('530', ceventfips, '000000', sep = ''),
                                    floor(log10(ceventfips) + 1) == 3 ~
                                      paste('53', ceventfips, '000000', sep = '')))
}

# tmp <- prep_spider(read.csv('Data/jan0324.csv'), start_date = '2023-08-22', end_date = '2023-12-31', include_unknown_geo = T, max_status = 9) 
# #  select(dreport, tfirstexp, Event, cstatus, ceventfips)
# tmp2 <- prep_spider_from_file('Data/jan0324.csv', start_date = '2023-08-22', end_date = '2023-12-31', include_unknown_geo = T, max_status = 9)


# prep_tmp <- spider_export %>%
#   # Replace NA cstatus with 9
#   #replace_na(list('cstatus' = 9)) %>%
#   # Format all date columns as ymd
#   mutate(
#     across((all_of(c('tfirstexp', 'tlastexp', 'tonset', 'dreport'))), date_formatter)) %>%
#   # Create a date column from dreport when populated, tfirstexp when not
#   mutate(
#     date = case_when(
#       is.na(dreport) ~ tfirstexp,
#       TRUE ~ dreport),
#     # Create an Event and EventDesc from ceventdesc without removing ceventdesc
#     Event = str_extract(string = ceventdesc, pattern = '^[:digit:]+(?=[:space:]|_)'),
#     EventDesc = str_extract(string = ceventdesc, pattern = '(?<=_|[:space:]{1}).*')
#   )
