# MAKE THE SPATIAL FILE FOR THE TABLEAU DASHBOARD - USE PIE POPULATION ESTIMATES

library(lubridate)
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(zoo)
census_api_key('9cbf1983cbddda3608a9c4196f4b6e7e5ede3209', install = T, overwrite = T)
censu_api_key <- '9cbf1983cbddda3608a9c4196f4b6e7e5ede3209'
# Load prep_spider() function, which does basic data engineering of the raw SPIDER export:
#   formatting date and geography FIPS codes; 
#   filtering dates by input parameters 'start_date' and 'end_date', 
#   filtering out unknown geographies if input parameter 'include_unknown_geos' == FALSE,
#   filtering out cases with status ('userstatus') less than or equal to input parameter 'max_status'
source('R_scripts/prep_spider_export.R')

# Load get_demographics(), which mainly converts numerical IDs for categories to human-interprable labels
source('R_scripts/get_demographics.R')

# Load format_symptoms(), which converts wide data (each field is a symptom) to long data (each field is a type of symptom evidence (e.g.: clinician-reported sypmtoms))
source('R_scripts/symptoms.R')

#################################################################################

# MAKE THE COUNTY PIE POPULATION DATA.FRAME FORMATTED NICELY
# pie_pop_files <- list.files(path = 'Data/PIE/County', pattern = '*.csv', full.names = T)
# 
# pie_pop_list <- lapply(pie_pop_files, read.csv)
# pie_pop <- bind_rows(pie_pop_list)
# 
# # Get totals by county by year - I guess by each race for all age groups
# pie_pop_formatted <- pie_pop %>%
#   group_by(across(all_of(c('CensusCountyCode2020', 'Year', 'RaceMars97')))) %>%
#   summarise(
#     PopulationTotalByRace = sum(Population)) %>%
#   ungroup() %>%
#   group_by(across(all_of(c('Geography', 'Year', 'Age')))) %>%
#   summarise(
#     PopulationTotalByAge = sum(Population)) %>%
#   ungroup() %>%
#   group_by(across(all_of(c('Geography', 'Year', 'Sex')))) %>%
#   summarise(
#     PopulationTotalBySex = sum(
#       
#     )
#   )

pie_pop <- read.csv('Data/PIE/PopPIE_results.csv')

pie_pop_formatted <- pie_pop %>%
  group_by(across(all_of(c('Geography', 'Year')))) %>%
  mutate(
    PopulationTotalOverall = sum(pop)) %>%
  ungroup() %>%
  group_by(across(all_of(c('Geography', 'Year', 'Race.Eth')))) %>%
  mutate(
    PopulationTotalByRace = sum(pop)) %>%
  ungroup() %>%
  group_by(across(all_of(c('Geography', 'Year', 'Age')))) %>%
  mutate(
    PopulationTotalByAge = sum(pop)) %>%
  ungroup() %>%
  group_by(across(all_of(c('Geography', 'Year', 'Sex')))) %>%
  mutate(
    PopulationTotalBySex = sum(pop)) %>%
  ungroup() %>%
  group_by(across(all_of(c('Geography', 'Year', 'Sex', 'Age')))) %>%
  mutate(
    PopulationTotalBySexAndAge = sum(pop)) %>%
  ungroup() %>%
  group_by(across(all_of(c('Geography', 'Year', 'Sex', 'Race.Eth')))) %>%
  mutate(
    PopulationTotalBySexAndRace = sum(pop)) %>%
  ungroup() %>%
  group_by(across(all_of(c('Geography', 'Year', 'Age', 'Race.Eth')))) %>%
  mutate(
    PopulationTotalByAgeAndRace = sum(pop))
  
## WITH THE POPULATIONALTOTAL COLUMNS, YOU CAN SELECT MEASURE(S) OF INTEREST ALONG WITH GEOGRAPHY, THEN CALL DISTINCT()

pop_by_whatever <- function(...){
  pie_pop_formatted %>%
    ungroup() %>%
    select(Geography.Name, Geography, ...) %>%
    distinct()
}

pop_by_age <- pop_by_whatever('Year', 'Age', 'PopulationTotalByAge') %>%
  mutate(
    Upper = as.integer(case_when(
      Age == '[0-4]' ~ '4',
      Age == '[5-9]' ~ '9',
      Age == '[10-14]' ~ '14',
      Age == '[15-19]' ~ '19',
      Age == '[20-24]' ~ '24',
      Age == '[25-29]' ~ '29',
      Age == '[30-34]' ~ '34',
      Age == '[35-39]' ~ '39',
      Age == '[45-49]' ~ '49',
      Age == '[55-59]' ~ '59',
      Age == '[65-69]' ~ '69',
      Age == '[75-79]' ~ '79',
      Age == '[85+]' ~ '999',
      Age == '[40-44]' ~ '44',
      Age == '[50-54]' ~ '54',
      Age == '[60-64]' ~ '64',
      Age == '[70-74]' ~ '74',
      Age == '[80-84]' ~ '84',
      TRUE ~ NA_character_)),
    Lower = as.integer(case_when(
      Age == '[0-4]' ~ '0',
      Age == '[5-9]' ~ '5',
      Age == '[10-14]' ~ '10',
      Age == '[15-19]' ~ '15',
      Age == '[20-24]' ~ '20',
      Age == '[25-29]' ~ '25',
      Age == '[30-34]' ~ '30',
      Age == '[35-39]' ~ '35',
      Age == '[45-49]' ~ '45',
      Age == '[55-59]' ~ '55',
      Age == '[65-69]' ~ '65',
      Age == '[75-79]' ~ '75',
      Age == '[85+]' ~ '85',
      Age == '[40-44]' ~ '40',
      Age == '[50-54]' ~ '50',
      Age == '[60-64]' ~ '60',
      Age == '[70-74]' ~ '70',
      Age == '[80-84]' ~ '80',
      TRUE ~ NA_character_)))

#write.csv(pop_by_age, 'Output/Tabular/PIE_pop_by_age.csv', row.names = F)
  
# READ IN SPIDER FLAT FILE EXPORT
df_raw <- read.csv('Data/nov1324.csv')
na_cstatus <- df_raw %>% filter(is.na(cstatus))
na_dreport <- df_raw %>% filter(is.na(dreport))
df_prepped <- prep_spider(df_raw = df_raw, max_status = 9)

# GET COUNTYGROUP SHAPES
get_pop_by_county_groups <- function(){
  # POPULATION FROM AMERICAN COMMUNITY SURVEY VIA TIDYCENSUS PACKAGE
    population_raw <- get_acs(geography = 'county', variables = 'B01001_001', state = 'WA', geometry = T) 
  
  # format population data to bind with pesticide data
  population <- population_raw %>%
    mutate(GEOID = paste(GEOID,'000000', sep = ''),
           county = str_replace(NAME, ' County, Washington', '')) %>%
    dplyr::select(GEOID, county) 
  
  # multi-county groupings - necessary to comply with DOH small numbers guidance
  # County-groups based on ACH updated 4/23 (https://www.hca.wa.gov/assets/program/ach-map.pdf)
  county_groups <- population %>%
    mutate(county_group = case_when(
    # 1 = 'North Sound' ACH
    county == 'Whatcom' ~ 1,
    county == 'Skagit' ~ 1, 
    county == 'Island' ~ 1,
    county == 'San Juan' ~ 1, 
    county == 'Snohomish' ~ 1, 
    # 2 = 'Olympic' Community of Health
    county == 'Clallam' ~ 2,
    county == 'Jefferson' ~ 2,
    county == 'Kitsap' ~ 2,
    # 3 = 'Cascade Pacific Action Alliance'
    county == 'Cowlitz' ~ 3,
    county == 'Grays Harbor' ~ 3,
    county == 'Lewis' ~ 3,
    county == 'Mason' ~ 3, 
    county == 'Pacific' ~ 3,
    county == 'Thurston' ~ 3,
    county == 'Wahkiakum' ~ 3,
    # 4 = SWACH 'Southwest'
    county == 'Clark' ~ 4, 
    county == 'Klickitat' ~ 4,
    county == 'Skamania' ~ 4,
    # 5 = 'Greater Columbia' ACH Now Called "Greater Health Now"
    county == 'Asotin' ~ 5, 
    county == 'Benton' ~ 5,  
    county == 'Columbia' ~ 5, 
    county == 'Franklin' ~ 5, 
    county == 'Garfield' ~ 5,  
    county == 'Kittitas' ~ 5, 
    county == 'Walla Walla' ~ 5, 
    county == 'Whitman' ~ 5, 
    county == 'Yakima' ~ 5, 
    # 6 = 'North Central' ACH Now Called "Thriving Together NCW"
    county == 'Chelan' ~ 6, 
    county == 'Douglas' ~ 6,
    county == 'Grant' ~ 6, 
    county == 'Okanogan' ~ 6,
    # 7 = 'Better Health Together'
    county == 'Adams' ~ 7, 
    county == 'Ferry' ~ 7, 
    county == 'Lincoln' ~ 7,
    county == 'Pend Oreille' ~ 7, 
    county == 'Spokane' ~ 7, 
    county == 'Stevens' ~ 7,
    # 8 = 'King' Now Called "HealthierHere
    county == 'King' ~ 8, 
    # 9 = Pierce Now Called "Elevate Health"
    county == 'Pierce' ~ 9,
    county == 'Unknown' ~ 10
  )) %>%
    group_by(county_group) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_as_sf()
  
  # Return list of population by county and county_groups spatial object
  return(list(population = population,county_groups = county_groups))
}

#tmp <-get_acs(geography = 'county', variables = 'B01001_001', state = 'WA', geometry = T, key = '9cbf1983cbddda3608a9c4196f4b6e7e5ede3209', year = 2023) 


population <- get_pop_by_county_groups()[[1]] %>%
  as.data.frame() %>%
  select(-geometry)
  
#write.csv(pop_df, 'Output/Tabular/County_FIPS_cw.csv', row.names = F)
county_groups <- get_pop_by_county_groups()[[2]]
x <- st_coordinates(county_groups)
#st_write(county_groups, 'Output/Spatial/ACHCountyGroup_geometry.shp')
#county_groups$county_group
# cases investigated by year

# df_raw_by_year <- df_raw %>%
#   mutate(date = mdy_hm(tfirstexp),
#          year = year(date)) %>%
#   group_by(year) %>%
#   summarise(count = n())
# 
# # QA - missing tfirstexp DPP
# dpp_tfirstexp_missing <- df_raw %>%
#   filter(cstatus <= 3,
#          str_detect(tfirstexp, '[:digit:]', negate = T))
# 
# by_status_2021 <- df_raw %>%
#   mutate(date = mdy_hm(tfirstexp),
#          year = year(date)) %>%
#   filter(year == 2021) %>%
#   group_by(cstatus) %>%
#   summarise(count = n())

###################################################################################
# # do some cleaning/ get a time-slice
# 
df_2012_2023 <- prep_spider(df_raw = df_raw, start_date = '2012-01-01', end_date = '2023-12-31', include_unknown_geo = T, max_status = 3)

#df_2011_2022 <- prep_spider(df_raw = df_raw, start_date = '2011-01-01', end_date = '2022-12-31', include_unknown_geo = T, max_status = 3)


################################################################################
# mutate demographic info and make the county groups

get_demographics <- function(df){
  
  population <- get_pop_by_county_groups()[[1]]
  county_groups <- get_pop_by_county_groups()[[2]]
  
  df_rdy <- df %>% 
    left_join(population, by = c('Geo-County' = 'GEOID')) %>%
    filter(!is.na(county)) %>%
    mutate(occupation = case_when(ccoc2002 >= 0 & ccoc2002 <= 9 ~ 'Non-Occupational',
                                ccoc2002 >= 10 & ccoc2002 <= 430 ~ 'Management',
                                ccoc2002 >= 500 & ccoc2002 <= 950 ~ 'Business and financial operations',
                                  ccoc2002 >= 1000 & ccoc2002 <= 1240 ~ 'Computer and mathematical',
                                  ccoc2002 >= 1300 & ccoc2002 <= 1560 ~ 'Architecture and engineering',
                                  ccoc2002 >= 1600 & ccoc2002 <= 1960 ~ 'Life, physical, and social science',
                                  ccoc2002 >= 2000 & ccoc2002 <= 2060 ~ 'Community and social services',
                                  ccoc2002 >= 2100 & ccoc2002 <= 2150 ~ 'Legal',
                                  ccoc2002 >= 2200 & ccoc2002 <= 2550 ~ 'Education, training, and library',
                                  ccoc2002 >= 2600 & ccoc2002 <= 2960 ~ 'Arts, design, entertainment, sports, and media',
                                  ccoc2002 >= 3000 & ccoc2002 <= 3540 ~ 'Healthcare practitioner and technical',
                                  ccoc2002 >= 3600 & ccoc2002 <= 3650 ~ 'Healthcare support',
                                  ccoc2002 >= 3700 & ccoc2002 <= 3950 ~ 'Protective services',
                                  ccoc2002 >= 4000 & ccoc2002 <= 4160 ~ 'Food preparation and serving-related',
                                  ccoc2002 >= 4200 & ccoc2002 <= 4250 ~ 'Building and grounds cleaning and maintenance',
                                  ccoc2002 >= 4300 & ccoc2002 <= 4650 ~ 'Personal care and service',
                                  ccoc2002 >= 4700 & ccoc2002 <= 4960 ~ 'Sales and related',
                                  ccoc2002 >= 5000 & ccoc2002 <= 5930 ~ 'Office and administrative support',
                                  ccoc2002 >= 6000 & ccoc2002 <= 6130 ~ 'Farming, fishing, and forestry', 
                                  #ccoc2002 == 6050 ~ 'Misc. Agricultural Worker',
                                  ccoc2002 >= 6200 & ccoc2002 <= 6940 ~  'Construction and extraction', 
                                  ccoc2002 >= 7000 & ccoc2002 <= 7620 ~  'Installation, maintenance, and repair',
                                  ccoc2002 >= 7700 & ccoc2002 <= 8960 ~  'Production',
                                  ccoc2002 >= 9000 & ccoc2002 <= 9750 ~  'Transportation and material moving',
                                  ccoc2002 == 9999 ~  'Unknown',
                                  TRUE ~ NA_character_),
           # cstatus is automatically generated by SPIDER - NOTE: rank is not in SPIDER and status is not equal to rank
           status = case_when(cstatus==1 ~ 'Definite',
                              cstatus==2 ~ 'Probable',
                              cstatus==3 ~ 'Possible',
                              cstatus==4 ~ 'Suspicious',
                              cstatus==5 ~ 'Unlikely',
                              cstatus==6 ~ 'Insufficient information',
                              cstatus==7 ~ 'Asymptomatic',
                              cstatus==8 ~ 'Unrelated',
                              cstatus==9 ~ 'Unknown/uncoded',
                              TRUE ~ NA_character_),
           sex = case_when(ccasesex == 'M' ~ 'Male',
                           ccasesex == 'F' ~ 'Female',
                           ccasesex == 'U' ~ 'Unknown'),
           workrel = case_when(cworkrel==1 ~ 'Yes',
                               cworkrel==2 ~ 'Possibly',
                               cworkrel==3 ~ 'No',
                               cworkrel==4 ~ 'Unknown',
                               cworkrel==5 ~ 'N/A',
                               TRUE ~ NA_character_),
           typeexp = case_when(ldrift == TRUE ~ 'Drift',
                               lspray == TRUE ~ 'Spray',
                               lindoorair == TRUE ~ 'Indoor air', 
                               lsurface == TRUE ~ 'Surface', 
                               lcontact == TRUE  ~ 'Contact', 
                               lother == TRUE  ~ 'Other', 
                               lleakspill == TRUE ~ 'Leak or spill', 
                               ltargeted == TRUE ~  'Targeted', 
                               ltypeunk == TRUE ~ 'Unknown',
                               TRUE ~ NA_character_),
           # how will race data work regarding new reporting rules?
           # race = case_when(
           #   ccaserace == 1 ~ 'American Indian, Alaskan Native',
           #   ccaserace == 2 ~ 'Asian or Pacific Islander',
           #   ccaserace == 3 ~ 'Black',
           #   ccaserace == 5 ~ 'White',
           #   ccaserace == 6 ~ 'Mixed Race',
           #   ccaserace == 8 ~ 'Other, Brown',
           #   ccaserace == 9 ~ 'Unknown',
           #   TRUE ~ NA_character_),
           # hisp = case_when(
           #   ccasehisp == 1 ~ 'Yes',
           #   ccasehisp == 2 ~ 'No',
           #   ccasehisp == 9 ~ 'Unknown',
           # ),
           race_ethnicity = case_when(
             # ccasehisp == 1 is "Yes"
             # ccaserace == 1 & ccasehisp != 1 ~ 'Non-Hispanic AIAN',
             # ccaserace == 2 & ccasehisp != 1 ~ 'Non-Hispanic API',
             # ccaserace == 3 & ccasehisp != 1 ~ 'Non-Hispanic Black',
             # ccaserace == 5 & ccasehisp != 1 ~ 'Non-Hispanic White',
             # ccaserace == 6 ~ 'Mixed Race',
             # ccaserace == 1 & ccasehisp == 1 ~ 'Mixed Race', # AIAN,
             # ccaserace == 2 & ccasehisp == 1 ~ 'Mixed Race',# API,
             # ccaserace == 3 & ccasehisp == 1 ~ 'Mixed Race', # Black,
             # ccaserace == 5 & ccasehisp == 1 ~ 'Hispanic',
             # ccaserace >= 8 & ccaserace <= 9 & ccasehisp == 1 ~ 'Hispanic',
             # ccaserace == 8 & ccasehisp != 1 ~ 'Other',
             # TRUE ~ 'Unknown'),
             ccaserace == 1 & ccasehisp != 1 ~ 'Non-Hispanic AIAN',
             ccaserace == 2 & ccasehisp != 1 ~ 'Non-Hispanic API',
             ccaserace == 3 & ccasehisp != 1 ~ 'Non-Hispanic Black',
             ccaserace == 5 & ccasehisp != 1 ~ 'Non-Hispanic White',
             ccaserace == 6 ~ 'Mixed Race',
             ccaserace == 1 & ccasehisp == 1 ~ 'Mixed Race', # AIAN,
             ccaserace == 2 & ccasehisp == 1 ~ 'Mixed Race',# API,
             ccaserace == 3 & ccasehisp == 1 ~ 'Mixed Race', # Black,
             ccaserace == 5 & ccasehisp == 1 ~ 'Hispanic',
             ccaserace >= 8 & ccaserace <= 9 & ccasehisp == 1 ~ 'Hispanic',
             ccaserace == 8 & ccasehisp != 1 ~ 'Other',
             TRUE ~ 'Unknown'),
           crop = case_when(ctarget == 10 ~ 'Landscape or ornamental',
                            ctarget == 20 ~ 'Forest trees',
                            ctarget == 31 ~ 'Veterinary/livestock',
                            ctarget == 32 ~ 'Veterinary/domestic animals',
                            ctarget >= 41 & ctarget <= 43 ~ 'Buildings',
                            ctarget == 50 ~ 'Weeds/undesired plants',
                            ctarget >= 60 & ctarget <= 61 ~ 'Pools',
                            ctarget == 70 ~ 'Soil',
                            ctarget == 80 ~ 'Wood products',
                            ctarget >= 100 & ctarget <= 120 ~ 'Fruit crops',
                            ctarget == 200 ~ 'Beverage crops', 
                            ctarget == 300 ~ 'Flavoring and spice crops', 
                            ctarget >= 400 & ctarget <= 460 ~ 'Miscellaneous vegetables', 
                            ctarget >= 500 & ctarget <= 550 ~ 'Grains, grasses, and fiber crops', 
                            ctarget >= 600 & ctarget <= 601 ~ 'Oils crops and seed treatments',
                            TRUE ~ 'Other'),
           product_class = case_when(class1 == 1 ~ 'Organochlorine compound',
                                     class1 == 2 ~ 'Organophosphorous compound',
                                     class1 == 3 ~ 'N-methyl carbamates',
                                     class1 == 4 ~ 'Pyrethrin',
                                     class1 == 5 ~ 'Pyrethroid',
                                     class1 == 6 ~ 'Dipyridyl compound',
                                     class1 == 7 ~ 'Chlorophenoxy compound',
                                     class1 == 8 ~ 'Triazines',
                                     class1 == 9 ~ 'Thiocarbamates',
                                     class1 == 10 ~ 'Organo-mettalic compounds',
                                     class1 == 11 ~ 'Inorganic compounds',
                                     class1 == 12 ~ 'Coumarins',
                                     class1 == 13 ~ 'Indandiones',
                                     class1 == 14 ~ 'Convulsants',
                                     class1 == 15 ~ 'Microbial',
                                     class1 == 16 ~ 'Dithiocarbamates',
                                     class1 == 17 ~ 'AChE inhibitors (comb. of OC and OP only)',
                                     class1 == 18 ~ 'AChE inhibitors w/ pyrethrin and/or pyrethroid only',
                                     class1 == 19 ~ 'AChE inhibitors w/ pyrethrin and/or pyrethroid + other',
                                     class1 == 20 ~ 'AChE inhibitors w/ OC only',
                                     class1 == 21 ~ 'AChE inhibitors w/ compounds not otherwise listed',
                                     class1 == 22 ~ 'Pyrethrin plus pyrethroid only',
                                     class1 == 23 ~ 'Pyrethrin plus pyrethroid plus other compounds not otherwise specified',
                                     class1 == 24 ~ 'Inorganic plus organometallic compounds only',
                                     class1 == 25 ~ 'OC plus inorganic compounds',
                                     class1 == 26 ~ 'Pyrethrin plus other compounds not otherwise specified',
                                     class1 == 94 ~ 'GMO',
                                     class1 == 95 ~ 'Unidentified cholinesterase inhibitor',
                                     class1 == 96 ~ 'Other',
                                     class1 == 97 ~ 'Multiple (not otherwise specified)',
                                     class1 == 99 ~ 'Unknown',
                                     TRUE ~ NA_character_),
           # functional class (pesticide type)
           func_class = case_when(type1 == 1 ~ 'Insecticide',
                                  type1 == 2 ~ 'Insect Growth Regulator',
                                  type1 == 3 ~ 'Herbicide/Algicide',
                                  type1 == 4 ~ 'Fungicide',
                                  type1 == 5 ~ 'Fumigant',
                                  type1 == 6 ~ 'Rodenticide',
                                  type1 == 7 ~ 'Disinfectant/Broad Spectrum for Water Sanitation',
                                  type1 == 8 ~ 'Insect Repellent',
                                  type1 == 9 ~ 'Antifouling Agent',
                                  type1 >= 10 & type1 <= 14 | type1 == 97 ~ 'Multiple',
                                  type1 == 96 ~ 'Other',
                                  TRUE ~ NA_character_),
           # revised groupings by ACH
           county_group = case_when(
               # 1 = 'North Sound' ACH
               county == 'Whatcom' ~ 1,
               county == 'Skagit' ~ 1, 
               county == 'Island' ~ 1,
               county == 'San Juan' ~ 1, 
               county == 'Snohomish' ~ 1, 
               # 2 = 'Olympic' Community of Health
               county == 'Clallam' ~ 2,
               county == 'Jefferson' ~ 2,
               county == 'Kitsap' ~ 2,
               # 3 = 'Cascade Pacific Action Alliance'
               county == 'Cowlitz' ~ 3,
               county == 'Grays Harbor' ~ 3,
               county == 'Lewis' ~ 3,
               county == 'Mason' ~ 3, 
               county == 'Pacific' ~ 3,
               county == 'Thurston' ~ 3,
               county == 'Wahkiakum' ~ 3,
               # 4 = SWACH 'Southwest'
               county == 'Clark' ~ 4, 
               county == 'Klickitat' ~ 4,
               county == 'Skamania' ~ 4,
               # 5 = 'Greater Columbia' ACH Now Called "Greater Health Now"
               county == 'Asotin' ~ 5, 
               county == 'Benton' ~ 5,  
               county == 'Columbia' ~ 5, 
               county == 'Franklin' ~ 5, 
               county == 'Garfield' ~ 5,  
               county == 'Kittitas' ~ 5, 
               county == 'Walla Walla' ~ 5, 
               county == 'Whitman' ~ 5, 
               county == 'Yakima' ~ 5, 
               # 6 = 'North Central' ACH Now Called "Thriving Together NCW"
               county == 'Chelan' ~ 6, 
               county == 'Douglas' ~ 6,
               county == 'Grant' ~ 6, 
               county == 'Okanogan' ~ 6,
               # 7 = 'Better Health Together'
               county == 'Adams' ~ 7, 
               county == 'Ferry' ~ 7, 
               county == 'Lincoln' ~ 7,
               county == 'Pend Oreille' ~ 7, 
               county == 'Spokane' ~ 7, 
               county == 'Stevens' ~ 7,
               # 8 = 'King' Now Called "HealthierHere
               county == 'King' ~ 8, 
               # 9 = Pierce Now Called "Elevate Health"
               county == 'Pierce' ~ 9,
               county == 'Unknown' ~ 10
             ))
  return(df_rdy)}

df_2012_2023_demo <- get_demographics(df_2012_2023)
#df_2011_2022_demo <- get_demographics(df_2011_2022)
# Check County Group options
unique(df_2012_2023_demo$county_group)

##################################################################################
# merge symptoms
symptoms_tmp <- format_symptoms(read.csv('Data/nov1324_s&s_details.csv'))


# As a function - demographics and symptoms. "export_date" is of format: 3 letter month abbreviation, 2 digit day, 2 digit year
# e.g.: jan0324
get_df_w_symptoms <- function(export_date, start, end, to_sf = F){
  
  # Get county groups spatial data
  county_groups <- get_pop_by_county_groups()$county_groups
  # Create the filenames and read in the .csv data
  main_fn <- paste0('Data/', export_date, '.csv')
  main_df <- read.csv(main_fn)
  symptoms_fn <- paste0('Data/', export_date, '_s&s_details.csv')
  symptoms_df <- format_symptoms(read.csv(symptoms_fn, colClasses = 'character'))
  
  # Prep the main file
  df_prepped <- prep_spider(main_df, start_date = start, end_date = end, include_unknown_geo = T, max_status = 3)
  
  # Get demographics
  df_demo <- get_demographics(df_prepped)
  
  # Add in symptoms 
  df_w_symptoms <- df_demo %>%
    left_join(symptoms_df, by = 'cexpid') %>%
    mutate(year = year(date)) %>%
    replace_na(list(fruit_crops = FALSE,
                    veg_crops = FALSE,
                    grain_crops = FALSE,
                    pyrethroids = FALSE,
                    inorganic_compounds = FALSE,
                    dithiocarbamates = FALSE,
                    organophosphates = FALSE)) %>%
    filter(cexpostate == 'WA', 
           cstatus <= 3,
           !is.na(county_group)) %>%
    group_by(across(all_of(c('year', 'county_group')))) %>%
    # Summarise the logical variables into count variables
    summarise(
      count = n(),
      # expo type
      drift = sum(ldrift),
      spray = sum(lspray),
      indoorair = sum(lindoorair),
      surface = sum(lsurface),
      contact = sum(lcontact),
      targeted = sum(ltargeted),
      leak_spill = sum(lleakspill),
      other = sum(lother),
      type_unknown = sum(ltypeunk),
      dermal = sum(ldermal),
      inhalation = sum(linhale),
      ingestion = sum(lingestion),
      injection = sum(linjection),
      occular = sum(loccular),
      route_unknown = sum(lrouteunk),
      # race/ethnicity - UPDATED
      nonhis_white = sum(race_ethnicity == 'Non-Hispanic White'),
      nonhis_black = sum(race_ethnicity == 'Non-Hispanic Black'),
      nonhis_aapi = sum(race_ethnicity == 'Non-Hispanic API'),
      nonhis_native = sum(race_ethnicity == 'Non-Hispanic AIAN'),
      his_native = sum(race_ethnicity == 'Hispanic AIAN'),
      his_aapi = sum(race_ethnicity == 'Hispanic API'),
      his_black = sum(race_ethnicity == 'Hispanic Black'),
      hispanic = sum(race_ethnicity == 'Hispanic'),
      race_unknown = sum(race_ethnicity == 'Unknown'),
      mixed_race = sum(race_ethnicity == 'Mixed Race'),
      other_race = sum(race_ethnicity == 'Other'),
      # age
      age_under5 = sum(cage >=0 & cage <5),
      age_5_14 = sum(cage >= 5 & cage <= 14),
      age_15_34 = sum(cage >= 15 & cage <= 34),
      age_35_64 = sum(cage >= 35 & cage <= 64),
      age_over65 = sum(cage >= 65),
      # sex
      male = sum(sex == 'Male'),
      female = sum(sex == 'Female'),
      # targets
      forest = sum(crop == 'Forest trees', na.rm = T),
      animals = sum(crop ==  'Veterinary/livestock' | crop == 'Veterinary/domestic animals', na.rm = T),
      buildings = sum(crop == 'Buildings', na.rm = T),
      weeds = sum(crop == 'Weeds/undesired plants', na.rm = T),
      pools = sum(crop == 'Pools', na.rm = T),
      soil = sum(crop == 'Soil', na.rm = T),
      wood = sum(crop == 'Wood products', na.rm = T),
      fruit_crops = sum(crop == 'Fruit crops', na.rm = T),
      veg_crops = sum(crop == 'Miscellaneous vegetables', na.rm = T),
      grain_crops = sum(crop ==  'Grains, grasses, and fiber crops', na.rm = T),
      other_crops = sum(crop %in% c('Oils crops and seed treatments', 'Beverage crops', 'Flavoring and spice crops', 'Other'), na.rm = T),
      # pesticides 
      pyrethroids = sum(product_class == 'Pyrethroid', na.rm = T),
      pyrethrins = sum(product_class == 'Pyrethrin', na.rm = T),
      pyrethrins_and_pyrethroids = sum(product_class %in% c('Pytrethrin plus pyrethroid only' , 'Pyrethrin plus other compounds not otherwise specified', 'Pytrethrin plus pyrethroid plus other compounds not otherwise specified'), na.rm = T),
      inorganic_compounds = sum(product_class == 'Inorganic compounds', na.rm = T),
      dithiocarbamates = sum(product_class == 'Dithiocarbamates', na.rm = T),
      organophosphates = sum(product_class == 'Organophosphorous compound', na.rm = T),
      other_pest_class = sum(product_class == 'Other', na.rm = T),
      mult_pest_class = sum(product_class == 'Multiple (not otherwise specified)', na.rm = T),
      chlorophenoxy = sum(product_class == 'Chlorophenoxy compound', na.rm = T),
      organometalic = sum(product_class == 'Organo-mettalic compounds', na.rm = T),
      dipyridyl = sum(product_class == 'Dipyridyl compound', na.rm = T),
      # functional class
      insecticide = sum(func_class == 'Insecticide', na.rm = T),
      herbicide = sum(func_class == 'Herbicide/Algicide', na.rm = T),
      fungicide = sum(func_class == 'Fungicide', na.rm = T), 
      fumigant = sum(func_class == 'Fumigant', na.rm = T),
      multi_class = sum(func_class == 'Multiple', na.rm = T),
      other_class = sum(func_class == 'Other', na.rm = T),
      # sev
      sev1 = sum(cseverity == 1),
      sev2 = sum(cseverity == 2),
      sev3 = sum(cseverity == 3),
      sev4 = sum(cseverity == 4),
      intentional = sum(cintent == 1),
      unintentional = sum(cintent == 2),
      # symptoms 
      headache = sum(symptoms_reported_by_individual_or_clinician == 'headache', na.rm = T),
      rash = sum(symptoms_reported_by_individual_or_clinician == 'rash', na.rm = T),
      conjdx = sum(symptoms_reported_by_individual_or_clinician == 'conjdx', na.rm = T),
      fatigue = sum(symptoms_reported_by_individual_or_clinician == 'fatigue', na.rm = T), 
      nausea = sum(symptoms_reported_by_individual_or_clinician == 'nausea', na.rm = T),
      # targets
      forest = sum(crop == 'Forest trees', na.rm = T),
      animals = sum(crop ==  'Veterinary/livestock' | crop == 'Veterinary/domestic animals', na.rm = T),
      buildings = sum(crop == 'Buildings', na.rm = T),
      weeds = sum(crop == 'Weeds/undesired plants', na.rm = T),
      pools = sum(crop == 'Pools', na.rm = T),
      soil = sum(crop == 'Soil', na.rm = T),
      wood = sum(crop == 'Wood products', na.rm = T),
      fruit_crops = sum(crop == 'Fruit crops', na.rm = T),
      veg_crops = sum(crop == 'Miscellaneous vegetables', na.rm = T),
      grain_crops = sum(crop ==  'Grains, grasses, and fiber crops', na.rm = T),
      other_crops = sum(crop %in% c('Oils crops and seed treatments', 'Beverage crops', 'Flavoring and spice crops', 'Other'), na.rm = T),
      # equipment
      hipress_grsprayer = sum(cequipment == 17),
      aerial = sum(cequipment == 1),
      handheld_sprayer = sum(cequipment == 10),
      manual_placement = sum(cequipment == 12),
      pressurized_can = sum(cequipment == 3),
      lopress_grsprayer = sum(cequipment == 11),
      backpack = sum(cequipment == 9),
      fogger_bugbomb = sum(cequipment == 16),
      # AG - FARMING, FISHING, AND FORESTRY
      ag_tot = sum(occupation == 'Farming, fishing, and forestry'),
      # OCCUPATION
      worker = sum(workrel == 'Yes'),
      # WEARING PPE
      ppe = sum(cppeany[cppeany == 1]),
      # LABEL FOLLOWED
      label_followed = sum(clabeluse == 1),
      label_notfollowed = sum(clabeluse == 2)) %>%
    # GROUP IT ON UP - 3 YEAR INTERVALS
    mutate(
      time_3year = case_when(
        year <= 2014 ~ '2012 - 2014',
        year >2014 & year <= 2017 ~ '2015 - 2017',
        year >2017 & year <= 2020 ~ '2018 - 2020',
        year >2020 & year <= 2023 ~ '2021 - 2023',
        TRUE ~ NA_character_)) %>%
    distinct()
  
  if(to_sf){
    shp_fn <- paste0('Output/Spatial/by_year_and_ach_wide_',year(ymd(start)), '_', year(ymd(end)),'_', today(), 'spiderexport.shp')
    print(paste('saving', shp_fn))
    
    sf_out <- df_w_symptoms %>%
      left_join(county_groups) %>%
      st_as_sf() %>%
      st_write(shp_fn)
    
    return(list(df = df_w_symptoms, spatial = sf_out))
  }
  else{
    return(df_w_symptoms)
  }
  
}
df_w_symptoms_2012_2023_sf <- get_df_w_symptoms(export_date = 'nov1324', start = '2012-01-01', end = '2023-12-31', to_sf = T)
df <- df_w_symptoms_2012_2023_sf$df
unique(df$time_3year)

write.csv(df, 'Output/Tabular/by_year_and_ach_wide_2012_2023.csv', row.names = F)
# QA NA check

nas <- filter_all(df_summary, any_vars(is.na(.)))
nrow(nas)

# # GROUP IT ON UP - 3 YEAR INTERVALS
# df_by_year_and_countygroup <- df_w_symptoms_2012_2023_sf$spatial %>%
#   mutate(            time_3year = case_when(year <= 2014 ~ '2012 - 2014',
#                                             year >2014 & year <= 2017 ~ '2015 - 2017',
#                                             year >2017 & year <= 2020 ~ '2018 - 2020',
#                                             year >2020 & year <= 2023 ~ '2021 - 2023',
#                                             TRUE ~ NA_character_)) %>%
#   distinct()

## QA - NA TIME 3-YEAR COLUMN
na_time_3year <- df_by_year_and_countygroup %>%
  filter(
    is.na(time_3year)
  )
st_write(df_by, paste0('Output/Spatial/by_year_and_ach_wide_',today(), 'spiderexport.shp'))

#################################################################################
# COUNT BY COUNTY TABLE 

count_by_county = df_2012_2023_demo %>%
  mutate(Year = year(date)) %>%
  filter(
    cexpostate == 'WA',
    cstatus <= 3) %>%
  rename(County = county) %>%
  group_by(across(all_of(c('Year' ,'County' )))) %>%
  summarise(
    Count = n(),
    Drift = sum(ldrift == TRUE) ) %>%
  ungroup() %>%
  # Add in Skamania, which has 0 total cases
  bind_rows(data.frame(County = 'Skamania', Year = 2012, Count = 0)) %>%
  # Fill in county-time_3year groups with 0 cases
  complete(County, Year, fill = list(Count = 0, Drift = 0)) %>%
  arrange(desc(Count)) #%>%

write.csv(count_by_county, paste0('Output/Tabular/Count_by_county_all', today(), 'spiderexport.csv'), row.names = F)

count_by_county = df_2012_2023_demo %>%
  mutate(year = year(date),
         time_3year = case_when(
           year <= 2014 ~ '2012 - 2014',
           year >2014 & year <= 2017 ~ '2015 - 2017',
                                                   year >2017 & year <= 2020 ~ '2018 - 2020',
                                                   year >2020 & year <= 2023 ~ '2021 - 2023',
                                                   TRUE ~ NA_character_)) %>%
  filter(cexpostate == 'WA',
         cstatus <= 3,
         !is.na(county_group)) %>%
  group_by(across(all_of(c('time_3year' ,'county' )))) %>%
  summarise(
    count = n(),
    drift = sum(ldrift == TRUE) ) %>%
  ungroup() %>%
  # Add in Skamania, which has 0 total cases
  bind_rows(data.frame(county = 'Skamania', time_3year = '2018 - 2020', count = 0)) %>%
  # Fill in county-time_3year groups with 0 cases
  complete(county, `time_3year`, fill = list(count = 0, drift = 0)) %>%
  arrange(desc(count)) #%>%
  #group_modify( ~head(., 5L))

sum(count_by_county$Count)
sum(count_by_county$Drift)  
write.csv(count_by_county, paste0('Output/Tabular/Count_by_county_all_3yearintervals_', today(), 'spiderexport.csv'), row.names = F)

####################################################################################
## SCRATCH PAPER

multiple_types <- df_prepped %>%
  filter(sum(ltypeunk, ldrift, lindoorair, lspray, ltargeted, lleakspill, lsurface, lcontact) > 1)


quantile(df$drift, )
