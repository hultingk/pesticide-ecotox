# AUTHORS: Katherine Hulting and Ashley Darst
# DATE: 2025
# INPUT: ECOTOX data (downloaded May 9, 2025)
# OUTPUT: Lepidopteran LD50 summary tables

## loading libraries
librarian::shelf(tidyverse, googledrive, googlesheets4, janitor)

# Upload data ----
# IMPORTING DATA FROM LEP EXPORT -- exported only lep data with mortality measurements, exported as two files due to file size limitations
lep_general_mortality <- read_sheet("https://docs.google.com/spreadsheets/d/1CuuLMViB8obWlAGZihlWlUvcpKP6DpWJbYnGrFfaJM0/edit?gid=1293937341#gid=1293937341")
lep_other_mortality <- read_sheet("https://docs.google.com/spreadsheets/d/1G14T3yslAiXOknL1aJ9drzb1wsTffJHewW8s0QWOW1A/edit?gid=1323975835#gid=1323975835")

## importing chemical data for common names and whether it is in USGS dataset
chemical_info <- read_sheet("https://docs.google.com/spreadsheets/d/1_v6aEuPvog9GBXyR8BIb7CkOwYDevuPtSuoRQw3LXFM/edit?gid=1160525203#gid=1160525203")

# IMPORTING bodyweights - determined these from papers after filtering 
bodyweight <- read_sheet("https://docs.google.com/spreadsheets/d/1xy7qhTDR19MdyVRc2AjtiHVvoKhHGTu8qLxHfAGA_e4/edit?gid=1738994693#gid=1738994693")

# Data cleaning ----
##### LEP DATA ####
# combining two files
lep_data <- rbind(
  lep_general_mortality,
  lep_other_mortality
)

# cleaning column names
lep_data <- clean_names(lep_data)


# filtering what we need - only keeping LD50 and active ingredients
lep_data <- lep_data %>%
  separate(species_scientific_name, into = c("genus", "species"), sep = "^\\S*\\K\\s+") %>% # separating into genus and species 
  filter(endpoint %in% c("LD50")) %>% # filtering by endpoint  
  filter(conc_1_type_author == "Active ingredient") ## Filter for only active ingredient -- we don't know we is lethal in formulations, CHECK what total is


## Rename chemicals -- two different DDTs, called both DDT
lep_data <- lep_data %>%
  mutate(chemical_name_common = recode(chemical_name, "(1E)-N-[(6-Chloro-3-pyridinyl)methyl]-N'-cyano-N-methylethanimidamide" = "acetamiprid", "(1E)-N-[(6-Chloro-3-pyridinyl)methyl]-N-ethyl-N'-methyl-2-nitro-1,1-ethenediamine
" = "nitenpyram", "(2E)-1-[(6-Chloro-3-pyridinyl)methyl]-N-nitro-2-imidazolidinimine" = "imidacloprid", "(4R,4aR,5R,7S,9S,10S,10aR,11S,12S)-2-Amino-1,4,4a,5,9,10-hexahydro-12-(hydroxymethyl)-5,9:7,10a-dimethano-10aH-[1,3]dioxocino[6,5-d]pyrimidine-4,7,10,11,12-pentol" = "tetrodotoxin", "(5alpha,6alpha)-7,8-Didehydro-4,5-epoxy-17-methylmorphinan-3,6-diol, Sulfate (2:1)" = "morphine sulfate", "1,1'-(2,2,2-Trichloroethylidene)bis[4-chlorobenzene]" = "DDT", "1-Chloro-2-(2,2,2-trichloro-1-(4-chlorophenyl)ethyl)benzene" = "DDT", "1-[[2-[2-Chloro-4-(4-chlorophenoxyl)phenyl]-4-methyl-1,3-dioxolan-2-yl]methyl]-1H-1,2,4-triazole" = "difenoconazole", "3-[(2-Chloro-5-thiazolyl)methyl]tetrahydro-5-methyl-N-nitro-4H-1,3,5-oxadiazin-4-imine" = "thiamethoxam", "3-[Benzoyl(methyl)amino]-N-[2-bromo-4-(1,1,1,2,3,3,3-heptafluoropropan-2-yl)-6-(trifluoromethyl)phenyl]-2-fluorobenzamide" = "broflanilide", "N''-Methyl-N-nitro-N'-[(tetrahydro-3-furanyl)methyl]guanidine" = "dinotefuran", "N-[[[2,5-Dichloro-4-(1,1,2,3,3,3-hexafluoropropoxy)phenyl]amino]carbonyl]-2,6-difluorobenzamide" = "lufenuron", "N-[[[3,5-Dichloro-4-(1,1,2,2-tetrafluoroethoxy)phenyl]amino]carbonyl]-2,6-difluorobenzamide" = "hexaflumuron", "N2-[1,1-Dimethyl-2-(methylsulfonyl)ethyl]-3-iodo-N1-[2-methyl-4-[1,2,2,2-tetrafluoro-1-(trifluoromethyl)ethyl]phenyl]-1,2-benzenedicarboxamide" = "flubendiamide", "Potassium cyanide (K(CN))" = "potassium cyanide", "[C(E)]-N-[(2-Chloro-5-thiazolyl)methyl]-N'-methyl-N''-nitroguanidine" = "clothianidin", "[N(Z)]-N-[3-[(6-Chloro-3-pyridinyl)methyl]-2-thiazolidinylidene]cyanamide" = "thiacloprid", "[N-[2-[(Dithiocarboxy)amino]ethyl]carbamodithioato(2-)-kappaS,kappaS']manganese mixt. with [N-[2-[(dithiocarboxy)amino]ethyl]carbamodithioato(2-)-kappaS,kappaS']zinc" = "mixture"))

## Filtering for pesticides and no mixtures
lep_data <- lep_data %>%
  filter(chemical_name_common != "mixture" & chemical_name_common != "morphine sulfate") # removed 2 observations

# removing columns
chemical_info <- chemical_info %>%
  dplyr::select(!c("chemical_name", "n"))

# join to pesticide info, fix pesticide classifications
lep_data <- lep_data %>%
  left_join(chemical_info, by = "cas_number") %>% # joining by CAS number
  mutate(pesticide_class = dplyr::case_when(
    pesticide_class %in% c("benzoylurea", "benzoylureas") ~ "benzoylurea",
    pesticide_class %in% c("botanical", "essential oil", "natural toxin", "monoterpene", "natural") ~ "natural",
    pesticide_class %in% c("pyrrole", "pyrroles") ~ "pyrrole",
    pesticide_class %in% c("pyrethrins and pyrethroids", "pyrethroid", "pyrethroid ether") ~ "pyrethroid",
    .default = pesticide_class
  )) %>%
  filter(!pesticide %in% c("no", "NA", "commercial mixture", "pesticide metabolite"))

# organize into topical vs not topical
# Spray, foliar seems dietary to me, but I can't find the one paper online
lep_data <- lep_data %>%
  mutate(exposure_type = dplyr::case_when(
    exposure_type %in% c("Dermal", "Dipped or soaked", "Direct application",
                         "Immersion", "Spray, foliar", "Spray, hand", "Spray, unspecified",
                         "Surface area dose", "Topical, general") ~ "Topical",
    exposure_type %in% c("Diet, unspecified", "Food", "Injection, unspecified", "Subcutaneous") ~ "Consumed/injected",
    exposure_type %in% c("Environmental, unspecified", "Fumigation", "Multiple routes between application groups",
                         "Multiple routes within environmental exposures") ~ "Other"
  ))

#### FILTERING ####
lep_data_sub <- lep_data %>%
  filter(observed_duration_days < 5) %>% # want observations of 1-4 days
  filter(organism_lifestage %in% c("Instar", "Larva", "Neonate")) %>% # filtering for larva
  filter(endpoint == "LD50") # only keeping LD50


###### standardize between units ######
lep_data_sub$observed_response_units <- str_trim(lep_data_sub$observed_response_units)
lep_data_sub <- lep_data_sub %>% # filtering unwanted units
  filter(!observed_response_units %in% c("ppm", "AI %", "%", "ug/eu", "ppm diet", "AI", "% diet", "AI % diet",
                                         "ug/cm2 lf", "ug/cm2", "AI ug/cm2 diet", "ng", "ug", "mg h/L",
                                         "mg/cm2", "ug/cm2", "ug/cm2 org", "g/ha")) 

# getting rid of "AI" and "diet" in some units -- all are active ingredients, if it is diet that will be reflected in the exposure route
lep_data_sub <- lep_data_sub %>%
  mutate(observed_response_units = if_else(str_detect(lep_data_sub$observed_response_units, "AI"), str_sub(lep_data_sub$observed_response_units, start = 4), observed_response_units)) %>%
  mutate(observed_response_units = dplyr::case_when(
    observed_response_units %in% c("mg/L", "mg/L diet", "mg/L diet") ~ "mg/L",
    observed_response_units %in% c("ai g/L") ~ "g/L",
    observed_response_units %in% c("ug/g diet") ~ "ug/g",
    observed_response_units %in% c("mg/kg diet") ~ "mg/kg",
    observed_response_units %in% c("ug/ml diet") ~ "ug/ml",
    .default = observed_response_units
  ))

# creating new units and responses columns to not overwrite original
lep_data_sub <- lep_data_sub %>%
  mutate(observed_response_units_converted = observed_response_units,
         observed_response_mean_converted = observed_response_mean,
         observed_response_min_converted = observed_response_min,
         observed_response_max_converted = observed_response_max)

### LIQUID UNITS: COVERTING UNITS TO mg/L 
for (i in 1:nrow(lep_data_sub)) {
  if (lep_data_sub$observed_response_units_converted[i] %in% c("mg/ml", "g/L")) {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 1000
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 1000
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 1000
    lep_data_sub$observed_response_units_converted[i] = "mg/L"
  }
  else if (lep_data_sub$observed_response_units_converted[i] == "ug/ml") {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 1
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 1
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 1
    lep_data_sub$observed_response_units_converted[i] = "mg/L"
  }
  else if (lep_data_sub$observed_response_units_converted[i] %in% c("ug/L", "ng/ml", "ul/L")) {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 1/1000
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 1/1000
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 1/1000
    lep_data_sub$observed_response_units_converted[i] = "mg/L"
  }
  else if (lep_data_sub$observed_response_units_converted[i] == "g/ml") {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 1000000
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 1000000
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 1000000
    lep_data_sub$observed_response_units_converted[i] = "mg/L"
  }
  else if (lep_data_sub$observed_response_units_converted[i] == "g/378 L") {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 2.64550265
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 2.64550265
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 2.64550265
    lep_data_sub$observed_response_units_converted[i] = "mg/L"
  }
}

#### WEIGHT UNITS: CONVERTING TO ug/g (not considering body weight here, resulting units will be ug/org)
for (i in 1:nrow(lep_data_sub)) {
  if (lep_data_sub$observed_response_units_converted[i] %in% c("mg/kg")) {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 1
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 1
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 1
    lep_data_sub$observed_response_units_converted[i] = "ug/g"
  }
  else if (lep_data_sub$observed_response_units_converted[i] %in% c("mg/g wet wt diet")) {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 0.001
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 0.001
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 0.001
    lep_data_sub$observed_response_units_converted[i] = "ug/g wet wt diet"
  }
  else if (lep_data_sub$observed_response_units_converted[i] %in% c("ng/mg bdwt")) {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 1
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 1
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 1
    lep_data_sub$observed_response_units_converted[i] = "ug/g bdwt"
  }
  else if (lep_data_sub$observed_response_units_converted[i] %in% c("ug/mg bdwt")) {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 1000
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 1000
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 1000
    lep_data_sub$observed_response_units_converted[i] = "ug/g bdwt"
  }
  else if (lep_data_sub$observed_response_units_converted[i] %in% c("ng/org")) {
    lep_data_sub$observed_response_mean_converted[i] = lep_data_sub$observed_response_mean_converted[i] * 1/1000
    lep_data_sub$observed_response_min_converted[i] = lep_data_sub$observed_response_min_converted[i] * 1/1000
    lep_data_sub$observed_response_max_converted[i] = lep_data_sub$observed_response_max_converted[i] * 1/1000
    lep_data_sub$observed_response_units_converted[i] = "ug/org"
  }
}

lep_data_sub <- lep_data_sub %>% # removing units that weren't removed above for some reason
  filter(!observed_response_units_converted %in% c("ppm", "g/ha", "ug/cm2"))

### MOST COMMON will probably by topical, LD50, ug/org 
endpoint_summary <- lep_data_sub %>%
  group_by(observed_response_units_converted, exposure_type, endpoint) %>%
  summarize(n = n())

# Preparing for merge
bodyweight <- bodyweight %>%
  filter(organism_lifestage != "Adult") %>%
  select(c("citation", "genus", "species", "organism_age_mean", "organism_age_units", "average_org_weight_g"))

# Merge bodyweights with lep data
lep_data_sub <- left_join(lep_data_sub, bodyweight)

# Convert ug/g org to ug/org 
lep_data_sub <- lep_data_sub %>%
  mutate(mean_response_ug_org = if_else(observed_response_units_converted %in% c("ug/g", "ug/g bdwt", "ug/g org"), observed_response_mean_converted*average_org_weight_g, observed_response_mean_converted),
         min_response_ug_org = if_else(observed_response_units_converted %in% c("ug/g", "ug/g bdwt", "ug/g org"), observed_response_min_converted*average_org_weight_g, observed_response_min_converted),
         max_response_ug_org = if_else(observed_response_units_converted %in% c("ug/g", "ug/g bdwt", "ug/g org"), observed_response_max_converted*average_org_weight_g, observed_response_max_converted),
         converted_units = if_else(observed_response_units_converted %in% c("ug/g", "ug/g bdwt", "ug/g org"), "ug/org", observed_response_units_converted))

# joining pest info
lep_data_sub <- lep_data_sub %>%
  mutate(genus_species = paste(genus, species, sep = " ")) %>%
  mutate(genus_species = case_when(
    genus_species %in% c("Heliothis virescens", "Helicoverpa virescens") ~ "Chloridea virescens",
    genus_species %in% c("Heliothis armigera") ~ "Helicoverpa armigera",
    genus_species %in% c("Anagasta kuehniella") ~ "Ephestia kuehniella",
    .default = genus_species
  )) %>%
  select(!c("genus", "species"))

# subsetting - focusing on topical for now
lep_topical <- lep_data_sub %>%
  filter(!is.na(mean_response_ug_org)) %>%
  filter(exposure_type == "Topical" & converted_units == "ug/org")

##### FINAL CSV, ALL INSTARS ####
# making compound names consistent with USGS names
lep_topical$pesticide_name <- toupper(lep_topical$pesticide_name)
lep_topical <- lep_topical %>%
  mutate(pesticide_name = dplyr::case_when(
    pesticide_name %in% c("ALPHA-CYPERMETHRIN") ~ "ALPHA CYPERMETHRIN",
    pesticide_name %in% c("BETA-CYPERMETHRIN") ~ "CYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    #pesticide_name %in% c("CHLORPYRIFOS OXYGEN ANALOG") ~ "CHLORPYRIFOS", ### I think this is valid -- mechanisms of chlorpyrifos toxicity is probably through it's oxygen analog https://pubchem.ncbi.nlm.nih.gov/compound/Chlorpyrifos#section=Biological-Half-Life 
    pesticide_name %in% c("CIS-CYPERMETHRIN") ~ "CYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    pesticide_name %in% c("FLUVALINATE-TAU") ~ "FLUVALINATE TAU", 
    pesticide_name %in% c("TRANS-CYPERMETHRIN") ~ "CYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    pesticide_name %in% c("ZETA-CYPERMETHRIN") ~ "ZETA-CYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    .default = pesticide_name
  ))

# cypermethrin notes
## zeta-cypermethrin and cypermethrin have the same CAS number in Maggie's spreadsheet, but different LD50s in her data
## this same CAS number can be used for beta-cypermethrin, but our beta-cypermethrin in our data has a different one
## there is also a second different CAS number for zeta-cypermethrin
## Cis and Trans cypermethrin have CAS numbers that don't really show up anywhere, and looking at the paper they come from I'm not really sure how ECOTOX got them
## perhaps calculate alpha seperately, calculate zeta using the 2 zeta CAS numbers, and calculate cypermethrin aggregating everything

# CHLORPYRIFOS-METHYL not the same as CHLORPYRIFOS - can't be combined
# ISOFENPHOS only used before 1995 -- banned, still in USGS list but doesn't show up in Maggie's data


lep_topical_final <- lep_topical %>%
  dplyr::select(cas_number, chemical_name, pesticide_name, pesticide_class, USGS, genus_species
              , organism_lifestage, organism_age_mean, observed_duration_days, exposure_type, effect, endpoint,
                mean_response_ug_org, min_response_ug_org, max_response_ug_org, converted_units, 
                author, reference_number, title, source, publication_year) %>%
  rename(instar = organism_age_mean)

write.csv(lep_topical_final, file = "lep_topical_LD50.csv", row.names = F)

#### SUMMARY TABLE, ALL INSTARS ####
summary_table <- lep_topical %>%
  group_by(pesticide_class, pesticide_name, converted_units) %>%
  reframe(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
          median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
          sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
          n = n(), # number of studies
          n_species = length(unique(genus_species)),
          group = "lep") %>% # number of species tested
  filter(pesticide_name != "CYPERMETHRIN") # calculating cypermethrin seperately as a combo of all cypermethrins

## calculating cypermethrin seperately as a combo of all cypermethrins
cypermethrin_ld50 <- lep_topical %>%
  mutate(cypermethrin = if_else(pesticide_name %in% c("ALPHA CYPERMETHRIN", "ZETA-CYPERMETHRIN", "CYPERMETHRIN"), "CYPERMETHRIN", NA)) %>%
  group_by(pesticide_class, cypermethrin, converted_units) %>%
  reframe(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
          median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
          sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
          n = n(), # number of studies
          n_species = length(unique(genus_species)),
          group = "lep") %>% # number of species tested
  filter(cypermethrin == "CYPERMETHRIN") %>%
  rename(pesticide_name = cypermethrin)

# adding cypermethrin to summary table
summary_table <- rbind(
  summary_table, cypermethrin_ld50
)

write.csv(summary_table, file = "summary_table_topical_LD50.csv", row.names = F)

# Table with both oral and topical values ----

# Get all in the same units
lep_topical_oral <- lep_data_sub %>%
  filter(!is.na(mean_response_ug_org)) %>%
  filter(exposure_type != "Other" & converted_units == "ug/org")

# Fix pesticide names
lep_topical_oral$pesticide_name <- toupper(lep_topical_oral$pesticide_name)
lep_topical_oral <- lep_topical_oral %>%
  mutate(pesticide_name = dplyr::case_when(
    pesticide_name %in% c("ALPHA-CYPERMETHRIN") ~ "ALPHA CYPERMETHRIN",
    pesticide_name %in% c("BETA-CYPERMETHRIN") ~ "CYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    #pesticide_name %in% c("CHLORPYRIFOS OXYGEN ANALOG") ~ "CHLORPYRIFOS", ### I think this is valid -- mechanisms of chlorpyrifos toxicity is probably through it's oxygen analog https://pubchem.ncbi.nlm.nih.gov/compound/Chlorpyrifos#section=Biological-Half-Life 
    pesticide_name %in% c("CIS-CYPERMETHRIN") ~ "CYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    pesticide_name %in% c("FLUVALINATE-TAU") ~ "FLUVALINATE TAU", 
    pesticide_name %in% c("TRANS-CYPERMETHRIN") ~ "CYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    pesticide_name %in% c("ZETA-CYPERMETHRIN") ~ "ZETA-CYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    .default = pesticide_name
  ))

# Export csv
lep_topical_oral_final <- lep_topical_oral %>%
  dplyr::select(cas_number, chemical_name, pesticide_name, pesticide_class, USGS, genus_species, 
                organism_lifestage, organism_age_mean, observed_duration_days, exposure_type, effect, endpoint,
                mean_response_ug_org, min_response_ug_org, max_response_ug_org, converted_units, 
                author, reference_number, title, source, publication_year) %>%
  rename(instar = organism_age_mean)

write.csv(lep_topical_oral_final, file = "lep_topical_oral_LD50.csv", row.names = F)

# Make summary table with oral and topical
summary_table_all <- lep_topical_oral %>%
  group_by(pesticide_class, pesticide_name, converted_units) %>%
  reframe(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
          median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
          sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
          n = n(), # number of studies
          n_species = length(unique(genus_species)),
          group = "lep") %>% # number of species tested
  filter(pesticide_name != "CYPERMETHRIN") # calculating cypermethrin seperately as a combo of all cypermethrins

## calculating cypermethrin seperately as a combo of all cypermethrins
cypermethrin_ld50_all <- lep_topical_oral %>%
  mutate(cypermethrin = if_else(pesticide_name %in% c("ALPHA CYPERMETHRIN", "ZETA-CYPERMETHRIN", "CYPERMETHRIN"), "CYPERMETHRIN", NA)) %>%
  group_by(pesticide_class, cypermethrin, converted_units) %>%
  reframe(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
          median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
          sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
          n = n(), # number of studies
          n_species = length(unique(genus_species)),
          group = "lep") %>% # number of species tested
  filter(cypermethrin == "CYPERMETHRIN") %>%
  rename(pesticide_name = cypermethrin)

# adding cypermethrin to summary table
summary_table_all <- rbind(
  summary_table_all, cypermethrin_ld50_all
)


write.csv(summary_table_all, file = "summary_table_topical_oral_LD50.csv", row.names = F)

# Separate by oral and topical
summary_table_separate <- lep_topical_oral %>%
  group_by(pesticide_class, pesticide_name, converted_units, exposure_type) %>%
  reframe(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
          median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
          sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
          n = n(), # number of studies
          n_species = length(unique(genus_species)),
          group = "lep") %>% # number of species tested
  filter(pesticide_name != "CYPERMETHRIN") # calculating cypermethrin seperately as a combo of all cypermethrins

cypermethrin_ld50_separate <- lep_topical_oral %>%
  mutate(cypermethrin = if_else(pesticide_name %in% c("ALPHA CYPERMETHRIN", "ZETA-CYPERMETHRIN", "CYPERMETHRIN"), "CYPERMETHRIN", NA)) %>%
  group_by(pesticide_class, cypermethrin, converted_units, exposure_type) %>%
  reframe(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
          median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
          sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
          n = n(), # number of studies
          n_species = length(unique(genus_species)),
          group = "lep") %>% # number of species tested
  filter(cypermethrin == "CYPERMETHRIN") %>%
  rename(pesticide_name = cypermethrin)

# adding cypermethrin to summary table
summary_table_separate <- rbind(
  summary_table_separate, cypermethrin_ld50_separate
)

write.csv(summary_table_separate, file = "summary_table_topical_oral_sep_LD50.csv", row.names = F)

