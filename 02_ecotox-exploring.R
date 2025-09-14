
## loading libraries
librarian::shelf(tidyverse, googledrive, googlesheets4, janitor)

# reading in cleaned data
lep_ecotox_sub <- read_sheet("https://docs.google.com/spreadsheets/d/1mMqVLlty3R8tWYtbKJbOjxsdSXNuuNVD9lBxel7cuJE/edit?gid=1880228016#gid=1880228016",
                             na = c("NULL", "NA"))
honeybee <- read_sheet("https://docs.google.com/spreadsheets/d/18K0bCEA8DyBNoodxUxoMyQ7dmxZAr2ekqAH1EL9sB38/edit?gid=457109213#gid=457109213",
                       na = c("NA"))
species <- read_sheet("https://docs.google.com/spreadsheets/d/1lhgrIbgQ_yOYYx8Tr23YHXiVmLsejNX-7YxtIoOepEA/edit?gid=2132720897#gid=2132720897")

# joining pest info
lep_ecotox_sub <- lep_ecotox_sub %>%
  left_join(species, by = c("genus", "species")) %>%
  mutate(genus_species = paste(genus, species, sep = " "))

# combining if there are multiple names for one species
lep_ecotox_sub <- lep_ecotox_sub %>%
  mutate(genus_species = case_when(
    genus_species %in% c("Heliothis virescens", "Helicoverpa virescens") ~ "Chloridea virescens",
    genus_species %in% c("Heliothis armigera") ~ "Helicoverpa armigera",
    genus_species %in% c("Anagasta kuehniella") ~ "Ephestia kuehniella",
    .default = genus_species
  )) %>%
  select(!c("genus", "species"))



# subsetting - focusing on topical for now
lep_topical <- lep_ecotox_sub %>%
  filter(!is.na(mean_response_ug_org)) %>%
  filter(exposure_type == "Topical" & converted_units == "ug/org")

# Total number of species and AI (including non-USGS)
# lep_topical %>%
#   summarize(n_pesticides = length(unique(pesticide_name)),
#             n_species = length(unique(genus_species)))
# 71 pesticides tested, 32 species

# Total number of species and AI (ONLY USGS)
# lep_topical %>%
#   filter(USGS == "yes") %>%
#   summarize(n_pesticides = length(unique(pesticide_name)),
#             n_species = length(unique(genus_species)))
# 48 USGS pesticides tested, 30 species


#### Varience within instar ####
lep_topical$organism_age_mean <- as.factor(lep_topical$organism_age_mean)
# lep_topical %>%
#   count(organism_age_mean)

# lep_instar_plot <- lep_topical %>%
#   ggplot(aes(x = organism_age_mean, y = log(mean_response_ug_org), col = organism_age_mean)) +
#   geom_jitter(size = 2, alpha = 0.7) +
#   stat_summary(fun.data = "mean_cl_boot", col = "black") +
#   theme_minimal(base_size = 16) +
#   #theme(axis.text.x = element_text(angle = 60,  hjust=1)) +
#   xlab("Instar") +
#   ylab("Log(LD50), Topical") +
#   guides(color=guide_legend(title="Instar"))
# lep_instar_plot

#ggsave("lep_instar_plot.png", width = 8, height = 6, dpi = 300)



# how much variation is there in bodyweight for each instar?
# lep_topical %>%
#   ggplot(aes(organism_age_mean, average_org_weight_g)) +
#   geom_jitter() +
#   theme_minimal() 
# 3rd and 4th instar have fairly comperable bodyweights

# is bodyweight related to LD50 for 3rd and 4th instar
# body_weight_plot <- lep_topical %>%
#   #filter(genus_species == "Helicoverpa armigera") %>%
#   filter(organism_age_mean %in% c("3", "4")) %>%
#   ggplot(aes(average_org_weight_g, log(mean_response_ug_org), color = pesticide_class)) +
#   geom_smooth(method = "lm") +
#   geom_point() +
#   theme_minimal(base_size = 16) +
#   xlab("Average body weight") +
#   ylab("Log(LD50), Topical") +
#   guides(color=guide_legend(title="Pesticide Class"))
# body_weight_plot
# to some extent but not super consistently
#ggsave("body_weight_plot.png", width = 8, height = 6, dpi = 500)


# most data is from instar 3 or 4 - filtering
lep_topical_3_4 <- lep_topical %>%
  filter(organism_age_mean %in% c("3", "4"))

# looking at varience in LD50 by pesticide class and instar
# comparison_instar_plot <- lep_topical_3_4 %>%
#   ggplot(aes(x = pesticide_class, y = log(mean_response_ug_org), col = organism_age_mean)) +
#   geom_jitter(position = position_jitterdodge(jitter.width = 1), alpha = 0.7) +
#   stat_summary(aes(fill = organism_age_mean), fun.data = "mean_cl_boot", col = "black", position = position_jitterdodge()) +
#   theme_minimal(base_size = 16) +
#   theme(axis.text.x = element_text(angle = 60,  hjust=1)) +
#   xlab("Pesticide class") +
#   ylab("Log(LD50), Topical") +
#   guides(color=guide_legend(title="Instar"),
#          fill = guide_legend(title="Instar"))
# comparison_instar_plot
#ggsave("comparison_instar_plot.png", width = 8, height = 6, dpi = 450)


#### variance between species ####
# lep_topical_3_4 %>% # how many studies per species?
#   count(genus_species) %>%
#   arrange(desc(n))
# lep_topical_3_4 %>%
#   count(genus_species) %>%
#   arrange(desc(n))
# # for the most studied species, how much variation in LD50? 
# species_comparison_plot <- lep_topical_3_4 %>%
#   filter(genus_species %in% c("Helicoverpa armigera", "Chilo suppressalis", "Chloridea virescens",
#                               "Chrysodeixis includens", "Spodoptera litura")) %>%
#   ggplot(aes(pesticide_class, log(mean_response_ug_org), color = genus_species)) +
#   geom_jitter(alpha = 0.2, position = position_jitterdodge()) +
#   stat_summary(aes(fill = genus_species), fun.data = "mean_cl_boot", position = position_dodge(0.2), size = 0.7) +
#   theme_minimal(base_size = 16) +
#   theme(axis.text.x = element_text(angle = 60,  hjust=1)) +
#   xlab("Pesticide class") +
#   ylab("Log(LD50), Topical") +
#   guides(color=guide_legend(title="Species"),
#          fill = guide_legend(title="Species"))
# species_comparison_plot
# #ggsave("species_comparison_plot.png", width = 8, height = 6, dpi = 550)
# 
# # creating summary table with just instar 3 and 4
# summary_table <- lep_topical_3_4 %>%
#   group_by(pesticide_class, pesticide_name, converted_units) %>%
#   summarize(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
#             median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
#             sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
#             n = n(), # number of studies
#             n_species = length(unique(genus_species))) # number of species tested


#### COMPARING to honeybees ####
# converting to ug/org instead of ng/org
# honeybee <- honeybee %>%
#   mutate(mean_ld50 = mean_ld50/1000,
#          median_ld50 = median_ld50/1000,
#          sd_ld50 = sd_ld50/1000)
# 
# # range of values is similar, generally honeybees have lower LD50s
# honeybee %>%
#   filter(Exposure_Type == "Dermal") %>%
#   filter(PestFamily_AdjClass != "Insect Growth Regulator") %>%
#   ggplot(aes(PestFamily_AdjClass, median_ld50)) +
#   geom_point(alpha = 0.5) +
#   theme_minimal(base_size = 12) +  
#   stat_summary(fun.data = "mean_cl_boot", col = "black") +
#   theme(axis.text.x = element_text(angle = 60,  hjust=1))
# 
# summary_table %>%
#   ggplot(aes(pesticide_class, median_LD50)) +
#   geom_point(alpha = 0.5) +
#   theme_minimal(base_size = 12) +  
#   stat_summary(fun.data = "mean_cl_boot", col = "black") +
#   theme(axis.text.x = element_text(angle = 60,  hjust=1))
# 
# # Merge summary table and honeybee
# honeybee2 <- honeybee %>%
#   filter(Exposure_Type == "Dermal")
# honeybee2$PestFamily_AdjClass <- tolower(honeybee2$PestFamily_AdjClass)
# honeybee2 <- rename(honeybee2, pesticide_class = PestFamily_AdjClass) 
# honeybee2 <- rename(honeybee2, median_LD50 = median_ld50) 
# honeybee2$insect <- "honeybee"
# summary_table2 <- summary_table
# summary_table2$insect <- "lep"
# summary_table2 <- summary_table2 %>%
#   ungroup %>%
#   select(pesticide_class, insect, median_LD50)
# honeybee2 <- honeybee2 %>%
#   select(pesticide_class, insect, median_LD50)
# honeybee_lep <- full_join(honeybee2, summary_table2)
# 
# honeybee_lep %>%
#   mutate(pesticide_class = fct_reorder(pesticide_class, log(median_LD50), mean, .na_rm = T)) %>%
#   ggplot(aes(pesticide_class, log(median_LD50), col = insect)) +
#   geom_jitter(position = position_jitterdodge(jitter.width = 0.5), alpha = 0.7) +
#   stat_summary(aes(fill = insect), fun.data = "mean_cl_boot", position = position_jitterdodge(dodge.width = 0.5)) +
#   theme_minimal(base_size = 12) +  
#   theme(axis.text.x = element_text(angle = 60,  hjust=1))
# 
# # distributions of values of major pesticide classes -- log transformed
# honeybee %>%
#   filter(Exposure_Type == "Dermal") %>%
#   filter(PestFamily_AdjClass %in% c("Carbamate", "Neonicotinoid", "Organophosphate", "Pyrethroid")) %>%
#   ggplot(aes(log(median_ld50), fill = PestFamily_AdjClass)) +
#   geom_histogram()
# 
# summary_table %>%
#   filter(pesticide_class %in% c("carbamate", "neonicotinoid", "organophosphate", "pyrethroid")) %>%
#   ggplot(aes(log(median_LD50), fill = pesticide_class)) +
#   geom_histogram()


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
  dplyr::select(cas_number, chemical_name, pesticide_name, pesticide_class, USGS, genus_species, 
                pest, organism_lifestage, organism_age_mean, observed_duration_days, exposure_type, effect, endpoint,
                mean_response_ug_org, min_response_ug_org, max_response_ug_org, converted_units, 
               author, reference_number, title, source.x, publication_year) %>%
  rename(instar = organism_age_mean, source = source.x)

write.csv(lep_topical_final, file = "lep_topical_LD50.csv", row.names = F)


#### SUMMARY TABLE, ALL INSTARS ####
# creating summary table with just instar 3 and 4
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

# honeybee2 <- honeybee2 %>%
#   mutate(converted_units = "ug/org") %>%
#   rename(pesticide_name = Active.Ingredient,
#          mean_LD50 = mean_ld50,
#          sd_LD50 = sd_ld50,
#          group = insect)
# honeybee_table <- honeybee2 %>%
#   select(pesticide_class, pesticide_name, group, mean_LD50, median_LD50, sd_LD50, converted_units, n)
# lep_table <- summary_table %>%
#   select(pesticide_class, pesticide_name, group, mean_LD50, median_LD50, sd_LD50, converted_units, n)
# 
# 
# joined_pest_data <- rbind(
#   lep_table, honeybee_table
# )
# 
# write.csv(joined_pest_data, file = "joined_summary_table_topical_LD50.csv", row.names = F)

## spot checking large LD50s
# highest LD50s
# 2329.000000, from ref # 119631, resistant strain
# 1734.600000, from ref # 105786, not resistant strain
# 501.000000, from ref # 154628, resistant strain
# 330.000000, from ref # 119631, resistant strain
# 262.300000, from ref # 108410, not resistant strain
# 216.300000, from ref # 179964, not resistant strain
# 202.125000, from ref # 104714, not resistant strain
# 198.100889, from ref # 104714, not resistant strain

# bottom LD50s
# 0.0000100, from ref # 117673, not resistant strain
# 0.0000240, from ref # 121481, not resistant strain? can't access paper
# 0.0000400, from ref # 181653, not resistant strain
# 0.0000700, from ref # 181653, not resistant strain


# Table with both oral and topical values ----

# Get all in the same units
lep_topical_oral <- lep_ecotox_sub %>%
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
                pest, organism_lifestage, organism_age_mean, observed_duration_days, exposure_type, effect, endpoint,
                mean_response_ug_org, min_response_ug_org, max_response_ug_org, converted_units, 
                author, reference_number, title, source.x, publication_year) %>%
  rename(instar = organism_age_mean, source = source.x)

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

