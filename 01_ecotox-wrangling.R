##### Script for cleaning/combining ecotox terrestrial insect/spider data ####

## loading libraries
librarian::shelf(tidyverse, googledrive, googlesheets4)
library(janitor)
 
## importing data from shared google folder (queried May 6, 2025)
googledrive::drive_auth()
ecotox_1 <- read_sheet("https://docs.google.com/spreadsheets/d/1aUQUue9Jh3M0EhE9F72n_RMqHUKV5I18cygncvjhAoE/edit?gid=183310004#gid=183310004")
ecotox_2 <- read_sheet("https://docs.google.com/spreadsheets/d/1yrPc7CypDj51oUEMUklh9kZq3uEJSsRglgZ-WfSr7L4/edit?gid=1038203519#gid=1038203519")
ecotox_3 <- read_sheet("https://docs.google.com/spreadsheets/d/1FCU-4u4aXIjjcDGi056-IKPykbJS7Cr4cIYi0QL3LWs/edit?gid=1659416391#gid=1659416391")
ecotox_4 <- read_sheet("https://docs.google.com/spreadsheets/d/1Iy4h-W9C_MpbtYu4KGozghRh9jj1YDyQmbPC8tWTVLM/edit?gid=1044083294#gid=1044083294")
ecotox_5 <- read_sheet("https://docs.google.com/spreadsheets/d/1SMavZEdQR6vnFCePB1UBd1rCJq5RX7Vvd4NsUTpNvko/edit?gid=767826702#gid=767826702")
ecotox_6 <- read_sheet("https://docs.google.com/spreadsheets/d/1sJYamV-_o2GUSO8MwRleXv8sBaHkEgVKpOJqN8us7qE/edit?gid=1223586958#gid=1223586958")


# combining into one dataframe
ecotox <- rbind(
  ecotox_1,
  ecotox_2,
  ecotox_3,
  ecotox_4,
  ecotox_5,
  ecotox_6
)

# looking at distinct rows -- there are some duplicate rows -- leaving these in for now 
distinct(ecotox)

# Clean column names
ecotox <- clean_names(ecotox)

species <- ecotox %>%
  count(species_scientific_name) # lots of species!

#### TO DO ####
# 1. Filter for LD50 or LC50 where the effect is mortality
# 2. Look at observed response units
# 3. Filter for lepidotera (might have to manually go through and split species name into genus/species column, filter for lepidoptera genera)
# 4. Assign chemical name to class of pesticide/other toxin
# 5. Note gaps for Leps, note variation in addition to mean values
#########

# Filter for lepidopterans
## I want this done first for personal reasons... my meta analysis maybe!
## Split into genus column -- assuming NAs generated are due to family level data, but ned to check
# 119 at Insecta level...
ecotox <- ecotox %>%
  separate(species_scientific_name, into = c("genus", "species"), sep = "^\\S*\\K\\s+")

genus <- ecotox %>%
  count(genus)

# A-L done
# Not including Insecta
ecotox %>%
  filter(Endpoint %in% c("Acrolepia", "Adoxophyes", "Agrotis", "Alabama", "Amyelois", "Anagasta", "Anarsia", "Antheraea", "Anticarsia", "Aroga", "Athetis", "Autographa", "Bombyx", "Cadra", "Catopsilia", "Cerconota", "Chilo", "Choristoneura", "Chrysodeixis", "Cnaphalocrocis", "Cochylis", "Coleotechnites", "Conopomorpha", "Corcyra", "Crocidolomia", "Cydia", "Danaus", "Diatraea", "Dioryctria", "Earias", "Egira", "Elasmopalpus", "Emmalocera", "Ephestia", "Epiphyas", "Epirrita", "Estigmene", "Etiella", "Eublemma", "Galleria", "Glyphodes", "Grapholita", "Helicoverpa", "Heliothis", "Hellula", "Homoeosoma", "Homona", "Hydria", "Hypena", "Issoria", "Itame", "Keiferia", "Lacanobia", "Lampides", "Leguminivora", "Lepidoptera", "Leucinodes", "Leucoptera", "Lobesia", "Lymantria", ""))

# Check types of endpoints -- NA endpoint is concerning, LOEL/NOEL may be useful...
ecotox %>%
  count(endpoint) %>%
  print(n = nrow(.))

## filtering for chosen response measured -- narrows it down a lot
ecotox <- ecotox %>%
  filter(endpoint %in% c("LD50", "LC50")) %>%
  filter(effect %in% c("Mortality"))

