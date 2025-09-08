library(tidyverse)

maggie_ld50 <- read.csv("uniqueCompoundsMD.csv")
joined_ld50 <- read.csv("joinedLepLD50s.csv")


joined_ld50 %>%
  filter(is.na(State)) %>%
  count(Compound)


joined_ld50 %>%
  mutate(Compound = dplyr::case_when(
    Compound %in% c("ALPHA-CYPERMETHRIN") ~ "ALPHACYPERMETHRIN",
    Compound %in% c("BETA-CYPERMETHRIN") ~ "", ### Check to see if this can be combined with CYPERMETHRIN
    Compound %in% c("CHLORPYRIFOS OXYGEN ANALOG") ~ "CHLORPYRIFOS", ### I think this is valid -- mechanisms of chlorpyrifos toxicity is probably through it's oxygen analog https://pubchem.ncbi.nlm.nih.gov/compound/Chlorpyrifos#section=Biological-Half-Life 
    #Compound %in% c("CHLORPYRIFOS-METHYL") ~ "", ### Check to see if this can be combined with CHLORPYRIFOS
    Compound %in% c("CIS-CYPERMETHRIN") ~ "", ### Check to see if this can be combined with CYPERMETHRIN
    Compound %in% c("FLUVALINATE-TAU") ~ "FLUVALINATETAU", 
    # ISOFENPHOS only used before 1995 -- banned, still in USGS list but doesn't show up in Maggie's data
    Compound %in% c("TRANS-CYPERMETHRIN") ~ "", ### Check to see if this can be combined with CYPERMETHRIN
    Compound %in% c("ZETA-CYPERMETHRIN") ~ "ZETACYPERMETHRIN", ### Check to see if this can be combined with CYPERMETHRIN
    .default = Compound
  ))

# CHLORPYRIFOS-METHYL not the same as CHLORPYRIFOS