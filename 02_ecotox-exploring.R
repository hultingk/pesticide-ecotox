
## loading libraries
librarian::shelf(tidyverse, googledrive, googlesheets4, janitor)

# reading in cleaned data
lep_ecotox_sub <- read_sheet("https://docs.google.com/spreadsheets/d/1XUD52KFV-5ZFFGvt3kA40iTI7XMUrK8sqSEDIao9ZLw/edit?gid=1322518390#gid=1322518390",
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
lep_topical %>%
  summarize(n_pesticides = length(unique(pesticide_name)),
            n_species = length(unique(genus_species)))
# 71 pesticides tested, 32 species

# Total number of species and AI (ONLY USGS)
lep_topical %>%
  filter(USGS == "yes") %>%
  summarize(n_pesticides = length(unique(pesticide_name)),
            n_species = length(unique(genus_species)))
# 48 USGS pesticides tested, 30 species


#### Varience within instar ####
lep_topical$organism_age_mean <- as.factor(lep_topical$organism_age_mean)
lep_topical %>%
  count(organism_age_mean)

lep_topical %>%
  ggplot(aes(x = organism_age_mean, y = log(mean_response_ug_org), col = organism_age_mean)) +
  geom_jitter() +
  stat_summary(fun.data = "mean_cl_boot", col = "black") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60,  hjust=1))

# how much variation is there in bodyweight for each instar?
lep_topical %>%
  ggplot(aes(organism_age_mean, average_org_weight_g)) +
  geom_jitter() +
  theme_minimal() 
# 3rd and 4th instar have fairly comperable bodyweights

# is bodyweight related to LD50 for 3rd and 4th instar
lep_topical %>%
  filter(organism_age_mean %in% c("3", "4")) %>%
  ggplot(aes(average_org_weight_g, log(mean_response_ug_org), color = pesticide_class)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_minimal()
# to some extent but not super consistently


# most data is from instar 3 or 4 - filtering
lep_topical_3_4 <- lep_topical %>%
  filter(organism_age_mean %in% c("3", "4"))

# looking at varience in LD50 by pesticide class and instar
lep_topical_3_4 %>%
  ggplot(aes(x = pesticide_class, y = log(mean_response_ug_org), col = organism_age_mean, fill = organism_age_mean)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 1)) +
  stat_summary(fun.data = "mean_cl_boot", col = "black", position = position_jitterdodge()) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60,  hjust=1))
  

#### variance between species ####
lep_topical_3_4 %>% # how many studies per species?
  count(genus_species) %>%
  arrange(desc(n))

# for the most studied species, how much variation in LD50? 
lep_topical_3_4 %>%
  filter(genus_species == "Helicoverpa armigera") %>%
  ggplot(aes(pesticide_name, log(mean_response_ug_org), color = pesticide_class)) +
  geom_point(alpha = 0.5) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60,  hjust=1))

# creating summary table with just instar 3 and 4
summary_table <- lep_topical_3_4 %>%
  group_by(pesticide_class, pesticide_name, converted_units) %>%
  summarize(mean_LD50 = mean(mean_response_ug_org, na.rm = T),  # mean value
            median_LD50 = median(mean_response_ug_org, na.rm = T), # median value
            sd_LD50 = sd(mean_response_ug_org, na.rm = T), # sd 
            n = n(), # number of studies
            n_species = length(unique(genus_species))) # number of species tested


#### COMPARING to honeybees ####
# converting to ug/org instead of ng/org
honeybee <- honeybee %>%
  mutate(mean_ld50 = mean_ld50/1000,
         median_ld50 = median_ld50/1000,
         sd_ld50 = sd_ld50/1000)

# range of values is similar, generally honeybees have lower LD50s
honeybee %>%
  ggplot(aes(PestFamily_AdjClass, median_ld50)) +
  geom_point(alpha = 0.5) +
  theme_minimal(base_size = 12) +  
  stat_summary(fun.data = "mean_cl_boot", col = "black") +
  theme(axis.text.x = element_text(angle = 60,  hjust=1))

summary_table %>%
  ggplot(aes(pesticide_class, median_LD50)) +
  geom_point(alpha = 0.5) +
  theme_minimal(base_size = 12) +  
  stat_summary(fun.data = "mean_cl_boot", col = "black") +
  theme(axis.text.x = element_text(angle = 60,  hjust=1))



##### Pest vs nonpest #####
lep_topical_3_4 %>%
  count(pest) ## way more pests, only 5 observations of nonpest 


