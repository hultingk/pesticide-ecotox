## loading libraries
librarian::shelf(tidyverse, googledrive, googlesheets4, janitor)

# 2021-2025 honeybee data from ecotox
honeybee_update <- read_sheet("https://docs.google.com/spreadsheets/d/1mxHcmwkHm0b1OCJI59zivL-1fVXRRsUsB1xzwvksTdk/edit?gid=470693985#gid=470693985")

# cleaning column names
honeybee_update <- clean_names(honeybee_update)

honeybee_update %>% # not many new topical/dermal observations, mostly ingested 
  count(exposure_type)


# filtering only for active ingredients 
honeybee_update <- honeybee_update %>%
  filter(endpoint %in% c("LD50")) %>% # filtering by endpoint  
  filter(conc_1_type_author == "Active ingredient") %>% ## Filter for only active ingredient -- we don't know we is lethal in formulations, CHECK what total is
  filter(observed_duration_days < 5) %>% # want observations of 1-4 days
  mutate(exposure_type = dplyr::case_when(
    exposure_type %in% c("Dermal", "Spray, hand", "Topical, general") ~ "Topical",
    exposure_type %in% c("Diet, unspecified", "Food", "Drinking water") ~ "Consumed/injected",
    exposure_type %in% c("Environmental, unspecified", "In Vitro") ~ "Other"
  ))
# only 6 new observations after filtering!

## importing chemical data 
chemical_info <- read_sheet("https://docs.google.com/spreadsheets/d/1_v6aEuPvog9GBXyR8BIb7CkOwYDevuPtSuoRQw3LXFM/edit?gid=1160525203#gid=1160525203")
# removing columns
chemical_info <- chemical_info %>%
  dplyr::select(!c("chemical_name", "n"))

# adding chemical info that is in the chemical spreadsheet
honeybee_update <- honeybee_update %>%
  left_join(chemical_info, by = "cas_number") # joining by CAS number

# all the same units, unifying
honeybee_update <- honeybee_update %>%
  mutate(observed_response_units = dplyr::case_when(
    observed_response_units %in% c("AI ug/org", "ug/org") ~ "ug/org"
  ))

honeybee_update %>%
  count(pesticide_class, pesticide_name, organism_lifestage)
# 6 observations total
# 4 larval, 2 adult






