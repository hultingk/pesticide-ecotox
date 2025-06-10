##### Script for cleaning/combining ecotox terrestrial insect/spider data ####

## loading libraries
librarian::shelf(tidyverse, googledrive, googlesheets4, janitor)

## importing data from shared google folder (queried May 6, 2025)
#googledrive::drive_auth()
#ecotox_1 <- read_sheet("https://docs.google.com/spreadsheets/d/1aUQUue9Jh3M0EhE9F72n_RMqHUKV5I18cygncvjhAoE/edit?gid=183310004#gid=183310004")
#ecotox_2 <- read_sheet("https://docs.google.com/spreadsheets/d/1yrPc7CypDj51oUEMUklh9kZq3uEJSsRglgZ-WfSr7L4/edit?gid=1038203519#gid=1038203519")
#ecotox_3 <- read_sheet("https://docs.google.com/spreadsheets/d/1FCU-4u4aXIjjcDGi056-IKPykbJS7Cr4cIYi0QL3LWs/edit?gid=1659416391#gid=1659416391")
#ecotox_4 <- read_sheet("https://docs.google.com/spreadsheets/d/1Iy4h-W9C_MpbtYu4KGozghRh9jj1YDyQmbPC8tWTVLM/edit?gid=1044083294#gid=1044083294")
#ecotox_5 <- read_sheet("https://docs.google.com/spreadsheets/d/1SMavZEdQR6vnFCePB1UBd1rCJq5RX7Vvd4NsUTpNvko/edit?gid=767826702#gid=767826702")
#ecotox_6 <- read_sheet("https://docs.google.com/spreadsheets/d/1sJYamV-_o2GUSO8MwRleXv8sBaHkEgVKpOJqN8us7qE/edit?gid=1223586958#gid=1223586958")

# IMPORTING DATA FROM LEP EXPORT -- exported only lep data with 
lep_general_mortality <- read_sheet("https://docs.google.com/spreadsheets/d/1CuuLMViB8obWlAGZihlWlUvcpKP6DpWJbYnGrFfaJM0/edit?gid=1293937341#gid=1293937341")
lep_other_mortality <- read_sheet("https://docs.google.com/spreadsheets/d/1G14T3yslAiXOknL1aJ9drzb1wsTffJHewW8s0QWOW1A/edit?gid=1323975835#gid=1323975835")

# IMPORTING USGS pesticides
usgs <- read_sheet("https://docs.google.com/spreadsheets/d/1Mg-_Yb_jGcqwn8psEeNgs2sHzMIaN7CEvsRoftttJh4/edit?gid=0#gid=0")

##### OLD CODE: JUMP TO LEP DATA FOR NEW EXPORTS ####
# combining into one dataframe
#ecotox <- rbind(
#  ecotox_1,
#  ecotox_2,
#  ecotox_3,
#  ecotox_4,
#  ecotox_5,
#  ecotox_6
#)

# looking at distinct rows -- there are some duplicate rows -- leaving these in for now 
#distinct(ecotox)

# Clean column names
#ecotox <- clean_names(ecotox)

#species <- ecotox %>%
#  count(species_scientific_name) # lots of species!

#### TO DO ####
# 1. Filter for LD50 or LC50 where the effect is mortality
# 2. Look at observed response units
# 3. Filter for lepidotera (might have to manually go through and split species name into genus/species column, filter for lepidoptera genera)
# 4. Assign chemical name to class of pesticide/other toxin
# 5. Note gaps for Leps, note variation in addition to mean values
#########


# Filter for lepidopterans
## I want this done first for personal reasons... my meta analysis maybe!
## Split into genus column -- NAs generated due to family level data
## Subfamilies are all ants
#ecotox <- ecotox %>%
#  separate(species_scientific_name, into = c("genus", "species"), sep = "^\\S*\\K\\s+")


#genus <- ecotox %>%
#  count(genus)



## Not including Insecta -- mostly population level effect + some development and some avoidance effects
#ecotox_lep <- ecotox %>%
#  filter(genus %in% c("Acrolepia", "Adoxophyes", "Agrotis", "Alabama", "Amyelois", "Anagasta", "Anarsia", "Antheraea", "Anticarsia", 
#                      "Aroga", "Athetis", "Autographa", "Bombyx", "Cadra", "Catopsilia", "Cerconota", "Chilo", "Choristoneura", 
#                      "Chrysodeixis", "Cnaphalocrocis", "Cochylis", "Coleotechnites", "Conopomorpha", "Corcyra", "Crocidolomia", 
#                      "Cydia", "Danaus", "Diatraea", "Dioryctria", "Earias", "Egira", "Elasmopalpus", "Emmalocera", "Ephestia", 
#                      "Epiphyas", "Epirrita", "Estigmene", "Etiella", "Eublemma", "Galleria", "Glyphodes", "Grapholita", "Helicoverpa", 
#                      "Heliothis", "Hellula", "Homoeosoma", "Homona", "Hydria", "Hypena", "Issoria", "Itame", "Keiferia", "Lacanobia", "Lampides", 
#                      "Leguminivora", "Lepidoptera", "Leucinodes", "Leucoptera", "Lobesia", "Lymantria", "Mamestra", "Manduca", "Maruca", 
#                      "Mythimna", "Neoleucinodes", "Noctuidae", "Ostrinia", "Pandemis", "Pectinophora", "Peridroma", "Phauda", "Phthorimaea", 
#                      "Phyllocnistis", "Phyllonorycter", "Pieris", "Planotortrix", "Platynota", "Plutella", "Pseudohypatopa", "Pyralidae", 
 #                     "Rhopobota", "Samia", "Sceliodes", "Scirpophaga", "Sesamia", "Sparganothis", "Spilarctia", "Spodoptera", "Striacosta", 
 #                     "Tineola", "Tortricidae", "Trichoplusia", "Tuta", "Vanessa", "Zeuzera"))


# Filter for response
## Check types of endpoints -- NA endpoint is concerning, LOEL/NOEL may be useful...
#ecotox_lep %>%
#  count(endpoint) %>%
#  print(n = nrow(.))

## Check types of effects for LD/LC50 -- all mortality, this is good
#ecotox_lep %>%
#  filter(endpoint %in% c("LD50", "LC50")) %>%
#  count(effect) %>%
#  print(n = nrow(.))

## filtering for chosen response measured -- narrows it down a lot
#ecotox_lep50 <- ecotox_lep %>%
#  filter(endpoint %in% c("LD50", "LC50")) %>%
#  filter(effect %in% c("Mortality"))

## Filter for only active ingredient -- we don't know we is lethal in formulations, CHECK what total is
#ecotox_lep50 <- ecotox_lep50 %>%
#  filter(conc_1_type_author == "Active ingredient")


# Filter for pesticides
#chemical <- ecotox_lep50 %>%
#  count(chemical_name)

## Rename chemicals -- two different DDTs, called both DDT
#ecotox_lep50 <- ecotox_lep50 %>%
#  mutate(chemical_name_common = recode(chemical_name, "(1E)-N-[(6-Chloro-3-pyridinyl)methyl]-N'-cyano-N-methylethanimidamide" = "acetamiprid", "(1E)-N-[(6-Chloro-3-pyridinyl)methyl]-N-ethyl-N'-methyl-2-nitro-1,1-ethenediamine
#" = "nitenpyram", "(2E)-1-[(6-Chloro-3-pyridinyl)methyl]-N-nitro-2-imidazolidinimine" = "imidacloprid", "(4R,4aR,5R,7S,9S,10S,10aR,11S,12S)-2-Amino-1,4,4a,5,9,10-hexahydro-12-(hydroxymethyl)-5,9:7,10a-dimethano-10aH-[1,3]dioxocino[6,5-d]pyrimidine-4,7,10,11,12-pentol" = "tetrodotoxin", "(5alpha,6alpha)-7,8-Didehydro-4,5-epoxy-17-methylmorphinan-3,6-diol, Sulfate (2:1)" = "morphine sulfate", "1,1'-(2,2,2-Trichloroethylidene)bis[4-chlorobenzene]" = "DDT", "1-Chloro-2-(2,2,2-trichloro-1-(4-chlorophenyl)ethyl)benzene" = "DDT", "1-[[2-[2-Chloro-4-(4-chlorophenoxyl)phenyl]-4-methyl-1,3-dioxolan-2-yl]methyl]-1H-1,2,4-triazole" = "difenoconazole", "3-[(2-Chloro-5-thiazolyl)methyl]tetrahydro-5-methyl-N-nitro-4H-1,3,5-oxadiazin-4-imine" = "thiamethoxam", "3-[Benzoyl(methyl)amino]-N-[2-bromo-4-(1,1,1,2,3,3,3-heptafluoropropan-2-yl)-6-(trifluoromethyl)phenyl]-2-fluorobenzamide" = "broflanilide", "N''-Methyl-N-nitro-N'-[(tetrahydro-3-furanyl)methyl]guanidine" = "dinotefuran", "N-[[[2,5-Dichloro-4-(1,1,2,3,3,3-hexafluoropropoxy)phenyl]amino]carbonyl]-2,6-difluorobenzamide" = "lufenuron", "N-[[[3,5-Dichloro-4-(1,1,2,2-tetrafluoroethoxy)phenyl]amino]carbonyl]-2,6-difluorobenzamide" = "hexaflumuron", "N2-[1,1-Dimethyl-2-(methylsulfonyl)ethyl]-3-iodo-N1-[2-methyl-4-[1,2,2,2-tetrafluoro-1-(trifluoromethyl)ethyl]phenyl]-1,2-benzenedicarboxamide" = "flubendiamide", "Potassium cyanide (K(CN))" = "potassium cyanide", "[C(E)]-N-[(2-Chloro-5-thiazolyl)methyl]-N'-methyl-N''-nitroguanidine" = "clothianidin", "[N(Z)]-N-[3-[(6-Chloro-3-pyridinyl)methyl]-2-thiazolidinylidene]cyanamide" = "thiacloprid", "[N-[2-[(Dithiocarboxy)amino]ethyl]carbamodithioato(2-)-kappaS,kappaS']manganese mixt. with [N-[2-[(dithiocarboxy)amino]ethyl]carbamodithioato(2-)-kappaS,kappaS']zinc" = "mixture"))

## Filtering for pesticides and no mixtures
#ecotox_lep50 <- ecotox_lep50 %>%
#  filter(chemical_name_common != "mixture" & chemical_name_common != "morphine sulfate")

## MISSING LD50 values from these papers... for example, Zheng et al.
## LESS values than in the Hall synthesis from 2021
## Hall synthesis has stricter standards than us (potentially)
  # a. Larval dose (topical studies) or diet concentration (dietary studies) units were not provided or could not be deciphered.
  # b. No ‘susceptible’ (i.e., non-resistant) lab populations were tested.
  # c. Single active ingredient compounds were not tested.
  # d. An appropriate solvent control was not utilized.
  # e. Insecticide solvent carrier for toxicity bioassays not provided.
  # f. Published in a language other than English.




##### LEP DATA ####
# combining
lep_data <- rbind(
  lep_general_mortality,
  lep_other_mortality
)

# cleaning column names
lep_data <- clean_names(lep_data)


# filtering what we need
lep_data <- lep_data %>%
  separate(species_scientific_name, into = c("genus", "species"), sep = "^\\S*\\K\\s+") %>% # separating into genus and species 
  filter(endpoint %in% c("LD50", "LC50")) %>% # filtering by endpoint  
  filter(conc_1_type_author == "Active ingredient") ## Filter for only active ingredient -- we don't know we is lethal in formulations, CHECK what total is


## Rename chemicals -- two different DDTs, called both DDT
lep_data <- lep_data %>%
  mutate(chemical_name_common = recode(chemical_name, "(1E)-N-[(6-Chloro-3-pyridinyl)methyl]-N'-cyano-N-methylethanimidamide" = "acetamiprid", "(1E)-N-[(6-Chloro-3-pyridinyl)methyl]-N-ethyl-N'-methyl-2-nitro-1,1-ethenediamine
" = "nitenpyram", "(2E)-1-[(6-Chloro-3-pyridinyl)methyl]-N-nitro-2-imidazolidinimine" = "imidacloprid", "(4R,4aR,5R,7S,9S,10S,10aR,11S,12S)-2-Amino-1,4,4a,5,9,10-hexahydro-12-(hydroxymethyl)-5,9:7,10a-dimethano-10aH-[1,3]dioxocino[6,5-d]pyrimidine-4,7,10,11,12-pentol" = "tetrodotoxin", "(5alpha,6alpha)-7,8-Didehydro-4,5-epoxy-17-methylmorphinan-3,6-diol, Sulfate (2:1)" = "morphine sulfate", "1,1'-(2,2,2-Trichloroethylidene)bis[4-chlorobenzene]" = "DDT", "1-Chloro-2-(2,2,2-trichloro-1-(4-chlorophenyl)ethyl)benzene" = "DDT", "1-[[2-[2-Chloro-4-(4-chlorophenoxyl)phenyl]-4-methyl-1,3-dioxolan-2-yl]methyl]-1H-1,2,4-triazole" = "difenoconazole", "3-[(2-Chloro-5-thiazolyl)methyl]tetrahydro-5-methyl-N-nitro-4H-1,3,5-oxadiazin-4-imine" = "thiamethoxam", "3-[Benzoyl(methyl)amino]-N-[2-bromo-4-(1,1,1,2,3,3,3-heptafluoropropan-2-yl)-6-(trifluoromethyl)phenyl]-2-fluorobenzamide" = "broflanilide", "N''-Methyl-N-nitro-N'-[(tetrahydro-3-furanyl)methyl]guanidine" = "dinotefuran", "N-[[[2,5-Dichloro-4-(1,1,2,3,3,3-hexafluoropropoxy)phenyl]amino]carbonyl]-2,6-difluorobenzamide" = "lufenuron", "N-[[[3,5-Dichloro-4-(1,1,2,2-tetrafluoroethoxy)phenyl]amino]carbonyl]-2,6-difluorobenzamide" = "hexaflumuron", "N2-[1,1-Dimethyl-2-(methylsulfonyl)ethyl]-3-iodo-N1-[2-methyl-4-[1,2,2,2-tetrafluoro-1-(trifluoromethyl)ethyl]phenyl]-1,2-benzenedicarboxamide" = "flubendiamide", "Potassium cyanide (K(CN))" = "potassium cyanide", "[C(E)]-N-[(2-Chloro-5-thiazolyl)methyl]-N'-methyl-N''-nitroguanidine" = "clothianidin", "[N(Z)]-N-[3-[(6-Chloro-3-pyridinyl)methyl]-2-thiazolidinylidene]cyanamide" = "thiacloprid", "[N-[2-[(Dithiocarboxy)amino]ethyl]carbamodithioato(2-)-kappaS,kappaS']manganese mixt. with [N-[2-[(dithiocarboxy)amino]ethyl]carbamodithioato(2-)-kappaS,kappaS']zinc" = "mixture"))

## Filtering for pesticides and no mixtures
lep_data <- lep_data %>%
  filter(chemical_name_common != "mixture" & chemical_name_common != "morphine sulfate") # removed 2 observations

# counting chemical names to export
#chemicals <- lep_data %>%
#  count(chemical_name, cas_number) 

# exporting chemical names to classify
#write.csv(chemicals, "chemicals.csv", row.names = FALSE)
#drive_upload(media = file.path("chemicals.csv"), overwrite = T,
#             path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1JgKWBip-mrmoVKkrpcHjF6z9LZqa_W5b"))

## importing chemical data 
chemical_info <- read_sheet("https://docs.google.com/spreadsheets/d/1_v6aEuPvog9GBXyR8BIb7CkOwYDevuPtSuoRQw3LXFM/edit?gid=1160525203#gid=1160525203")

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
## TO DO: convert the units that use bodyweight to ug/org
summary <- lep_data_sub %>%
  group_by(observed_response_units_converted, exposure_type, endpoint) %>%
  summarize(n = n())

# IMPORTING bodyweights
bodyweight <- read_sheet("https://docs.google.com/spreadsheets/d/1xy7qhTDR19MdyVRc2AjtiHVvoKhHGTu8qLxHfAGA_e4/edit?gid=1738994693#gid=1738994693")

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


# Make summary csv
final_table <- lep_data_sub %>%
  filter(!is.na(mean_response_ug_org)) %>%
  filter(exposure_type == "Topical" & converted_units == "ug/org") %>%
  group_by(pesticide_class, pesticide_name, converted_units) %>%
  summarize(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
            median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
            sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
            n = n(), # number of studies
            n_species = length(unique(species_common_name))) # number of species tested

# exporting
#write.csv(final_table, file = "Lep_LD50_topical_summary.csv", row.names = F)

# summary plot
LD50_topical_plot <- lep_data_sub %>%
  filter(!is.na(mean_response_ug_org)) %>%
  filter(exposure_type == "Topical" & converted_units == "ug/org") %>%
  mutate(pesticide_class = fct_reorder(pesticide_class, log(mean_response_ug_org), mean, .na_rm = T)) %>%
  ggplot(aes(x = pesticide_class, y = log(mean_response_ug_org), col = pesticide_class)) +
  geom_jitter() +
  stat_summary(fun.data = "mean_cl_boot", col = "black") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60,  hjust=1))
LD50_topical_plot

# exporting
#pdf(file = "summary_plot_topicalLD50.pdf", width = 10, height = 7)
#LD50_topical_plot
#dev.off()

###################################
# filtering out subset of pesticide classes and response units
# lep_data_sub <- lep_data %>%
#   filter(pesticide_class %in% c("pyrethroid", "organophosphate", "neonicotinoid", 
#                                 "benzoylurea", "diacylhydrazine", "carbamate", "anthranilic diamide")) %>%
#   filter(effect_measurement == "Mortality") %>%
#   filter(endpoint == "LD50") %>%
#   filter(observed_response_units %in% c("ug/org", "ug/g", "ug/g bdwt")) %>%
#   filter(exposure_type %in% c("Topical, general", "Dermal"))

# log transformed overview
# lep_data_sub %>%
#   ggplot() +
#   geom_boxplot(aes(pesticide_class, log(conc_1_mean_author), fill = pesticide_class)) +
#   facet_grid(observed_response_units~exposure_type) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 60,  hjust=1))


## making table of means for major classes broken up by exposure type, endpoint, and units
# lep_data %>%
#   filter(pesticide_class %in% c("pyrethroid", "organophosphate", "neonicotinoid", 
#                                 "benzoylurea", "diacylhydrazine", "carbamate", "anthranilic diamide")) %>%
#   filter(effect_measurement == "Mortality") %>%
#   group_by(pesticide_class, exposure_type, endpoint, observed_response_units) %>%
#   summarise(n = n(),
#             mean = mean(conc_1_mean_author),
#             median = median(conc_1_mean_author),
#             sd = sd(conc_1_mean_author),
#             min = min(conc_1_mean_author), 
#             max = max(conc_1_mean_author))
  
