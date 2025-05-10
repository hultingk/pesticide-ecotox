# Butterfly decline synthesis group
# Hall data exploration
# Ashley Darst
# May 10 2025

# Load libraries
librarian::shelf(tidyverse, googledrive, googlesheets4, janitor)

# Load data
hall <- read_sheet("https://docs.google.com/spreadsheets/d/1aBMvGOmBhMURHcB78SmxV_G_L9_oLTXp5_pkvmLJcCY/edit?gid=102880429#gid=102880429")

# Convert list columns to numeric (weird)
hall$LD50_ug <- as.double(hall$LD50_ug)
hall$LD50_ugg <- as.double(hall$LD50_ugg)
hall$LC50_ugg <- as.double(hall$LC50_ugg)
hall$LC50_ugcm2 <- as.double(hall$LC50_ugcm2)
hall$LC50_ugcm3 <- as.double(hall$LC50_ugcm3)

# Topical ----

## LD50 ug/larva ----
hall_top <-  hall %>%
  filter(Exposure == "Topical") %>%
  drop_na(LD50_ug)

hall_top %>%
  group_by(Class) %>%
  summarize(n = n(),
            mean = mean(LD50_ug),
            median = median(LD50_ug),
            sd = sd(LD50_ug))

# Keep plot colors consistent despite reordering
cols <- c("Diamide" = "#F8766D", "Pyrethroid" = "#7CAE00", Neonicotinoid = "#C77CFF", OP = "#00BFC4")

# Hall removed the outlier
hall_top %>%
  mutate(Class = fct_reorder(Class, LD50_ug, mean)) %>%
  ggplot(aes(x = Class, y = LD50_ug, col = Class)) +
  geom_jitter() +
  stat_summary(fun.data = mean_cl_boot, col = "black") +
  scale_color_manual(values = cols)

# Butterfly vs moth coverage -- need more butterflies
hall_top %>%
  group_by(Type) %>%
  summarize(n = n())

hall_top %>%
  mutate(Class = fct_reorder(Class, LD50_ug, mean)) %>%
  ggplot(aes(x = Class, y = LD50_ug, col = Class)) +
  geom_jitter(alpha = 0.6, aes(shape = Type)) +
  stat_summary(fun.data = mean_cl_boot, col = "black", aes(shape = Type), position = position_dodge(0.5)) +
  scale_color_manual(values = cols)

# Remove outlier
hall_top %>%
  filter(LD50_ug < 100) %>%
  mutate(Class = fct_reorder(Class, LD50_ug, mean)) %>%
  ggplot(aes(x = Class, y = LD50_ug, col = Class)) +
  geom_jitter(alpha = 0.6, aes(shape = Type)) +
  stat_summary(fun.data = mean_cl_boot, col = "black", aes(shape = Type), position = position_dodge(0.5)) +
  scale_color_manual(values = cols)

## LD50 ug/g larva ----
hall_top_g <-  hall %>%
  filter(Exposure == "Topical") %>%
  drop_na(LD50_ugg)

# Need more neonics and diamides
hall_top_g %>%
  group_by(Class) %>%
  summarize(n = n(),
            mean = mean(LD50_ugg),
            median = median(LD50_ugg),
            sd = sd(LD50_ugg))

# Order of neonic and OP changes -- due to outlier or way of calculating LD50?
hall_top_g %>%
  mutate(Class = fct_reorder(Class, LD50_ugg, mean)) %>%
  ggplot(aes(x = Class, y = LD50_ugg, col = Class)) +
  geom_jitter() +
  stat_summary(fun.data = mean_cl_boot, col = "black") +
  scale_color_manual(values = cols)


# Dietary ----

## LD50 ug/g ----
hall_diet_g <-  hall %>%
  filter(Exposure == "Dietary") %>%
  drop_na(LC50_ugg)

hall_diet_g %>%
  group_by(Class) %>%
  summarize(n = n(),
            mean = mean(LC50_ugg),
            median = median(LC50_ugg),
            sd = sd(LC50_ugg))

hall_diet_g %>%
  mutate(Class = fct_reorder(Class, LC50_ugg, mean)) %>%
  ggplot(aes(x = Class, y = LC50_ugg, col = Class)) +
  geom_jitter() +
  stat_summary(fun.data = mean_cl_boot, col = "black") +
  scale_color_manual(values = cols)

## LD50 ug/cm2 ----
hall_diet_cm2 <-  hall %>%
  filter(Exposure == "Dietary") %>%
  drop_na(LC50_ugcm2)

hall_diet_cm2 %>%
  group_by(Class) %>%
  summarize(n = n(),
            mean = mean(LC50_ugcm2),
            median = median(LC50_ugcm2),
            sd = sd(LC50_ugcm2))

hall_diet_cm2 %>%
  mutate(Class = fct_reorder(Class, LC50_ugcm2, mean)) %>%
  ggplot(aes(x = Class, y = LC50_ugcm2, col = Class)) +
  geom_jitter() +
  stat_summary(fun.data = mean_cl_boot, col = "black") +
  scale_color_manual(values = cols)

# Notes ----
# Need more diamides and neonics (and obviously non-insecticides)
# Diamide consistently the worst, neonic not as bad
# Need to convert into LD50 ug/larva probably, all need to be in the same unit

