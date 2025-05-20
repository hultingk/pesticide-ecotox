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

lep_data <- lep_data %>%
  left_join(chemical_info, by = "cas_number") %>% # joining by CAS number
  mutate(pesticide_class = dplyr::case_when(
    pesticide_class %in% c("benzoylurea", "benzoylureas") ~ "benzoylurea",
    pesticide_class %in% c("botanical", "essential oil", "natural toxin", "monoterpene", "natural") ~ "natural",
    pesticide_class %in% c("pyrrole", "pyrroles") ~ "pyrrole",
    pesticide_class %in% c("pyrethrins and pyrethroids", "pyrethroid", "pyrethroid ether") ~ "pyrethroid",
    .default = pesticide_class
  )) %>%
  filter(!pesticide %in% c("no", "NA"))
 



