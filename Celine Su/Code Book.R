
library(tidyverse)
library(broom)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(tidyr)

library(ggbeeswarm)
library(ggrepel)

## New Github repo!

# - Measuring Consistency

# Data

## Basic Stats
basic_2023_24 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_23_24.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2023)

basic_2022_23 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_22_23.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2022)

basic_2021_22 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_21_22.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2021)

basic_2020_21 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_20_21.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2020)

basic_2019_20 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_19_20.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2019)

basic_2018_19 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_18_19.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2018)

basic_2017_18 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_17_18.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2017)

basic_2016_17 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Basic%20Stats/basic_stats_16_17.csv") |> 
  rename(EVG = EV...13, PPG = PP...14, SHG = SH...15, EVA = EV...17, PPA = PP...18, SHA = SH...19) |>
  mutate(start_year = 2016)

basic_df <- rbind(basic_2016_17, basic_2017_18, basic_2018_19, basic_2019_20, 
                  basic_2020_21, basic_2021_22, basic_2022_23, basic_2023_24)

basic_df <- basic_df |> 
  filter(GP >= 20)

long_basic_df <- basic_df |> 
  pivot_wider(id_cols = c(Player, PlayerID), 
              values_from = c(GP, G, A, PTS, EVG, PPG, SHG, EVA, PPA, SHA, `+/-`, TOI, `S%`, PS), 
              names_from = start_year) |> 
  unique()

long_basic_df <- long_basic_df %>% 
  unnest() %>% 
  replace(. == "NULL", NA)


long_basic_df <- long_basic_df |> 
  arrange(Player, PlayerID)


## Advanced Stats 
advanced_2023_24 <- read_csv("~/Desktop/Penguins Capstone Project/Advanced Stats/advanced_2023_24.csv") |>  
  mutate(start_year = 2023)

advanced_2022_23 <- read_csv("~/Desktop/Penguins Capstone Project/Advanced Stats/advanced_2022_23.csv") |> 
  mutate(start_year = 2022)

advanced_2021_22 <- read_csv("~/Desktop/Penguins Capstone Project/Advanced Stats/advanced_2021_22.csv") |> 
  mutate(start_year = 2021)

advanced_2020_21 <- read_csv("~/Desktop/Penguins Capstone Project/Advanced Stats/advanced_2020_21.csv") |> 
  mutate(start_year = 2020)

advanced_2019_20 <- read_csv("~/Desktop/Penguins Capstone Project/Advanced Stats/advanced_2019_20.csv") |> 
  mutate(start_year = 2019)

advanced_2018_19 <- read_csv("~/Desktop/Penguins Capstone Project/Advanced Stats/advanced_2018_19.csv") |> 
  mutate(start_year = 2018)

advanced_2017_18 <- read_csv("~/Desktop/Penguins Capstone Project/Advanced Stats/advanced_2017_18.csv") |> 
  mutate(start_year = 2017)

advanced_2016_17 <- read_csv("~/Desktop/Penguins Capstone Project/Advanced Stats/advanced_2016_17.csv") |> 
  mutate(start_year = 2016)


advanced_df <- rbind(advanced_2016_17, advanced_2017_18, advanced_2018_19, advanced_2019_20, 
                     advanced_2020_21, advanced_2021_22, advanced_2022_23, advanced_2023_24)

advanced_df <- advanced_df |> filter(GP >= 20, Pos != "G") |> unique()

long_adv_df <- advanced_df |> 
  pivot_wider(id_cols = c(Player, PlayerID), 
              values_from = c(GP, CF, CA, FF, FA, `oiSH%`, `oiSV%`, PDO, TK, GV, `CF%`, `FF%`), 
              names_from = start_year) |> 
  unique()

long_adv_df <- long_adv_df %>% 
  unnest() %>% 
  replace(. == "NULL", NA)

long_adv_df <- long_adv_df |> arrange(Player, PlayerID)




## Miscellaneous Stats
misc_2023_24 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_23_24.csv") |> 
  mutate(start_year = 2023)

misc_2022_23 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_22_23.csv") |>
  mutate(start_year = 2022)

misc_2021_22 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_21_22.csv") |> 
  mutate(start_year = 2021)

misc_2020_21 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_20_21.csv") |> 
  mutate(start_year = 2020)

misc_2019_20 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_19_20.csv") |> 
  mutate(start_year = 2019)

misc_2018_19 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_18_19.csv") |> 
  mutate(start_year = 2018)

misc_2017_18 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_17_18.csv") |> 
  mutate(start_year = 2017)

misc_2016_17 <- read_csv("https://raw.githubusercontent.com/CelineS138/Penguins-Capstone-Project/main/Data/Misc%20Stats/misc_stats_16_17.csv") |> 
  mutate(start_year = 2016)


misc_df <- rbind(misc_2023_24, misc_2022_23, misc_2021_22, misc_2020_21, misc_2019_20,
                 misc_2018_19, misc_2017_18, misc_2016_17)

misc_df <- misc_df |> 
  filter(Pos != "G", GP >= 20)

misc_df_pos <- misc_df |> 
  mutate(Pos = str_extract(Pos, "[A-Z]"))

num_seasons <- misc_df |> 
  group_by(Player, PlayerID) |> 
  count() |> 
  ungroup() |> 
  mutate(num_seasons = n) |> 
  unique()

player_pos <- misc_df_pos |> 
  group_by(Player) |> 
  summarise(Pos = ifelse(Pos %in% c("C", "L", "R", "W", "F"), "F", "D")) |> 
  unique()

long_misc_df <- misc_df |> 
  pivot_wider(id_cols = c(Player, PlayerID), 
              values_from = c(`G/GP`, `A/GP`, `PTS/GP`, `GC/GP`, `S/GP`, `+/-`, `OPS`, `DPS`, `PS`), 
              names_from = start_year)

long_misc_df <- long_misc_df %>% 
  unnest() %>% 
  replace(. == "NULL", NA)


## Skater Time on Ice Stats (toi)

toi_2023_24 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_23_24.csv") |> 
  mutate(start_year = 2023)

toi_2022_23 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_22_23.csv") |>
  mutate(start_year = 2022)

toi_2021_22 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_21_22.csv")|> 
  mutate(start_year = 2021)

toi_2020_21 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_20_21.csv") |> 
  mutate(start_year = 2020)

toi_2019_20 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_19_20.csv") |> 
  mutate(start_year = 2019)

toi_2018_19 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_18_19.csv") |> 
  mutate(start_year = 2018)

toi_2017_18 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_17_18.csv") |> 
  mutate(start_year = 2017)

toi_2016_17 <- read_csv("~/Desktop/Penguins Capstone Project/TOI Stats/toi_stats_16_17.csv") |> 
  mutate(start_year = 2016)

toi_df <- rbind(toi_2016_17, toi_2017_18, toi_2018_19, toi_2019_20, 
                toi_2020_21, toi_2021_22, toi_2022_23, toi_2023_24)

toi_df <- toi_df |> 
  select(-`...7`, -`...12`, -`...17`)

toi_df <- toi_df |> 
  filter(Pos != "G", GP >= 20, PlayerID != "ahose02")

long_toi_df <- toi_df |> 
  pivot_wider(id_cols = c(Player, PlayerID), values_from = c(`TOI (EV)`, `CF% Rel (EV)`, `GF/60 (EV)`, `GA/60 (EV)`, `TOI (PP)`, `CF% Rel (PP)`, `GF/60 (PP)`, `GA/60 (PP)`, `TOI (SH)`, `CF% Rel (SH)`, `GF/60 (SH)`, `GA/60 (SH)`), names_from = start_year) |> 
  unique()

long_toi_df <- long_toi_df %>% 
  unnest() %>%
  replace(. == "NULL", NA)

long_toi_df <- long_toi_df |> arrange(Player, PlayerID)



# Combined Data

#complete_data <- cbind(basic_df, advanced_df, toi_df, misc_df)

complete_df3 <- cbind(long_basic_df, long_adv_df, long_toi_df, long_misc_df)

complete_df <- complete_df3[ , !duplicated(colnames(complete_df3))]

complete_df <- cbind(complete_df, player_pos, num_seasons)
complete_df <- complete_df[ , !duplicated(colnames(complete_df))]; complete_df <- complete_df[, -276]

complete_df_selected <- complete_df |> 
  filter(num_seasons >= 5)


# Offense

## Forward data 
forwards <- complete_df_selected |> 
  filter(Pos == "F") |> 
  select(Player, PlayerID, starts_with("EVG_"), starts_with("EVA_"), starts_with("EVP_"), starts_with("S%_"), 
         starts_with("TOI_"), starts_with("GP_"))


## Per 60 metric data

forwards_60 <- forwards |>
  select(Player, PlayerID, starts_with("EVG_"), starts_with("EVA_"), starts_with("EVP_"), starts_with("S%_"), 
         starts_with("TOI_"), starts_with("GP_")) |> 
  group_by(Player, PlayerID) |> 
  mutate(`EVG/60_2023` = (EVG_2023 / TOI_2023) * 60,
         `EVG/60_2022` = (EVG_2022 / TOI_2022) * 60,
         `EVG/60_2021` = (EVG_2021 / TOI_2021) * 60,
         `EVG/60_2020` = (EVG_2020 / TOI_2020) * 60,
         `EVG/60_2019` = (EVG_2019 / TOI_2019) * 60,
         `EVG/60_2018` = (EVG_2018 / TOI_2018) * 60,
         `EVG/60_2017` = (EVG_2017 / TOI_2017) * 60,
         `EVG/60_2016` = (EVG_2016 / TOI_2016) * 60,
         
         `EVA/60_2023` = (EVA_2023 / TOI_2023) * 60,
         `EVA/60_2022` = (EVA_2022 / TOI_2022) * 60,
         `EVA/60_2021` = (EVA_2021 / TOI_2021) * 60,
         `EVA/60_2020` = (EVA_2020 / TOI_2020) * 60,
         `EVA/60_2019` = (EVA_2019 / TOI_2019) * 60,
         `EVA/60_2018` = (EVA_2018 / TOI_2018) * 60,
         `EVA/60_2017` = (EVA_2017 / TOI_2017) * 60,
         `EVA/60_2016` = (EVA_2016 / TOI_2016) * 60,
         
         `S%_2023` = `S%_2023`,
         `S%_2022` = `S%_2022`,
         `S%_2021` = `S%_2021`,
         `S%_2020` = `S%_2020`,
         `S%_2019` = `S%_2019`,
         `S%_2018` = `S%_2018`,
         `S%_2017` = `S%_2017`,
         `S%_2016` = `S%_2016`,
  ) |> 
  ungroup()

forwards_stats <- forwards_60 |> 
  mutate(mean_EVG_2023 = mean(`EVG/60_2023`, na.rm = TRUE),
         mean_EVG_2022 = mean(`EVG/60_2022`, na.rm = TRUE),
         mean_EVG_2021 = mean(`EVG/60_2021`, na.rm = TRUE),
         mean_EVG_2020 = mean(`EVG/60_2020`, na.rm = TRUE),
         mean_EVG_2019 = mean(`EVG/60_2019`, na.rm = TRUE),
         mean_EVG_2018 = mean(`EVG/60_2018`, na.rm = TRUE),
         mean_EVG_2017 = mean(`EVG/60_2017`, na.rm = TRUE),
         mean_EVG_2016 = mean(`EVG/60_2016`, na.rm = TRUE),
         
         mean_EVA_2023 = mean(`EVA/60_2023`, na.rm = TRUE),
         mean_EVA_2022 = mean(`EVA/60_2022`, na.rm = TRUE),
         mean_EVA_2021 = mean(`EVA/60_2021`, na.rm = TRUE),
         mean_EVA_2020 = mean(`EVA/60_2020`, na.rm = TRUE),
         mean_EVA_2019 = mean(`EVA/60_2019`, na.rm = TRUE),
         mean_EVA_2018 = mean(`EVA/60_2018`, na.rm = TRUE),
         mean_EVA_2017 = mean(`EVA/60_2017`, na.rm = TRUE),
         mean_EVA_2016 = mean(`EVA/60_2016`, na.rm = TRUE),
         
         `mean_S%_2023` = mean(`S%_2023`, na.rm = TRUE),
         `mean_S%_2022` = mean(`S%_2022`, na.rm = TRUE),
         `mean_S%_2021` = mean(`S%_2021`, na.rm = TRUE),
         `mean_S%_2020` = mean(`S%_2020`, na.rm = TRUE),
         `mean_S%_2019` = mean(`S%_2019`, na.rm = TRUE),
         `mean_S%_2018` = mean(`S%_2018`, na.rm = TRUE),
         `mean_S%_2017` = mean(`S%_2017`, na.rm = TRUE),
         `mean_S%_2016` = mean(`S%_2016`, na.rm = TRUE),
         
         sd_EVG_2023 = sd(`EVG/60_2023`, na.rm = TRUE),
         sd_EVG_2022 = sd(`EVG/60_2022`, na.rm = TRUE),
         sd_EVG_2021 = sd(`EVG/60_2021`, na.rm = TRUE),
         sd_EVG_2020 = sd(`EVG/60_2020`, na.rm = TRUE),
         sd_EVG_2019 = sd(`EVG/60_2019`, na.rm = TRUE),
         sd_EVG_2018 = sd(`EVG/60_2018`, na.rm = TRUE),
         sd_EVG_2017 = sd(`EVG/60_2017`, na.rm = TRUE),
         sd_EVG_2016 = sd(`EVG/60_2016`, na.rm = TRUE),
         
         sd_EVA_2023 = sd(`EVA/60_2023`, na.rm = TRUE),
         sd_EVA_2022 = sd(`EVA/60_2022`, na.rm = TRUE),
         sd_EVA_2021 = sd(`EVA/60_2021`, na.rm = TRUE),
         sd_EVA_2020 = sd(`EVA/60_2020`, na.rm = TRUE),
         sd_EVA_2019 = sd(`EVA/60_2019`, na.rm = TRUE),
         sd_EVA_2018 = sd(`EVA/60_2018`, na.rm = TRUE),
         sd_EVA_2017 = sd(`EVA/60_2017`, na.rm = TRUE),
         sd_EVA_2016 = sd(`EVA/60_2016`, na.rm = TRUE),
         
         `sd_S%_2023` = sd(`S%_2023`, na.rm = TRUE),
         `sd_S%_2022` = sd(`S%_2022`, na.rm = TRUE),
         `sd_S%_2021` = sd(`S%_2021`, na.rm = TRUE),
         `sd_S%_2020` = sd(`S%_2020`, na.rm = TRUE),
         `sd_S%_2019` = sd(`S%_2019`, na.rm = TRUE),
         `sd_S%_2018` = sd(`S%_2018`, na.rm = TRUE),
         `sd_S%_2017` = sd(`S%_2017`, na.rm = TRUE),
         `sd_S%_2016` = sd(`S%_2016`, na.rm = TRUE)
  )


forwards_zscore <- forwards_stats |> 
  group_by(Player, PlayerID) |>
  summarize(EVG_zscore_2023 = (`EVG/60_2023` - `mean_EVG_2023`) / `sd_EVG_2023`,
            EVG_zscore_2022 = (`EVG/60_2022` - `mean_EVG_2022`) / `sd_EVG_2022`,
            EVG_zscore_2021 = (`EVG/60_2021` - `mean_EVG_2021`) / `sd_EVG_2021`,
            EVG_zscore_2020 = (`EVG/60_2020` - `mean_EVG_2020`) / `sd_EVG_2020`,
            EVG_zscore_2019 = (`EVG/60_2019` - `mean_EVG_2019`) / `sd_EVG_2019`,
            EVG_zscore_2018 = (`EVG/60_2018` - `mean_EVG_2018`) / `sd_EVG_2018`,
            EVG_zscore_2017 = (`EVG/60_2017` - `mean_EVG_2017`) / `sd_EVG_2017`,
            EVG_zscore_2016 = (`EVG/60_2016` - `mean_EVG_2016`) / `sd_EVG_2016`,
            
            EVA_zscore_2023 = (`EVA/60_2023` - `mean_EVA_2023`) / `sd_EVA_2023`,
            EVA_zscore_2022 = (`EVA/60_2022` - `mean_EVA_2022`) / `sd_EVA_2022`,
            EVA_zscore_2021 = (`EVA/60_2021` - `mean_EVA_2021`) / `sd_EVA_2021`,
            EVA_zscore_2020 = (`EVA/60_2020` - `mean_EVA_2020`) / `sd_EVA_2020`,
            EVA_zscore_2019 = (`EVA/60_2019` - `mean_EVA_2019`) / `sd_EVA_2019`,
            EVA_zscore_2018 = (`EVA/60_2018` - `mean_EVA_2018`) / `sd_EVA_2018`,
            EVA_zscore_2017 = (`EVA/60_2017` - `mean_EVA_2017`) / `sd_EVA_2017`,
            EVA_zscore_2016 = (`EVA/60_2016` - `mean_EVA_2016`) / `sd_EVA_2016`,
            
            `S%_zscore_2023` = (`S%_2023` - `mean_S%_2023`) / `sd_S%_2023`,
            `S%_zscore_2022` = (`S%_2022` - `mean_S%_2022`) / `sd_S%_2022`,
            `S%_zscore_2021` = (`S%_2021` - `mean_S%_2021`) / `sd_S%_2021`,
            `S%_zscore_2020` = (`S%_2020` - `mean_S%_2020`) / `sd_S%_2020`,
            `S%_zscore_2019` = (`S%_2019` - `mean_S%_2019`) / `sd_S%_2019`,
            `S%_zscore_2018` = (`S%_2018` - `mean_S%_2018`) / `sd_S%_2018`,
            `S%_zscore_2017` = (`S%_2017` - `mean_S%_2017`) / `sd_S%_2017`,
            `S%_zscore_2016` = (`S%_2016` - `mean_S%_2016`) / `sd_S%_2016`
  )

forwards_z_long <- forwards_zscore |> 
  select(-PlayerID) |> 
  pivot_longer(-c(Player),
               names_to = c(".value", "Year"), 
               names_sep = "_zscore_")



## combined z-scores & PCA

forwards_z_clean <- forwards_z_long |> 
  ungroup() |> 
  select(EVG, EVA, `S%`) |> 
  na.omit()


forwards_pca <- princomp(forwards_z_clean)

#summary(forwards_pca)

## get the loading matrix
loadings_forwards <- forwards_pca$loadings

## Function to normalize loadings
normalize_loadings <- function(loadings) {
  # Calculate the sum of the absolute values of the loadings for each component
  col_sums <- colSums(abs(loadings), na.rm = TRUE)
  
  # Divide each loading by the sum of the absolute values of its component
  normalized_loadings <- sweep(loadings, 2, col_sums, FUN = "/")
  
  return(normalized_loadings)
}

## normalize the loadings
nor_loadings_forwards <- normalize_loadings(loadings_forwards)

print(nor_loadings_forwards)


## combined z-scores

forwards_cbz <- forwards_z_long |> 
  group_by(Player, Year) |> 
  mutate(total_zscore = sum(abs(nor_loadings_forwards[1, 1]) * EVG, abs(nor_loadings_forwards[2, 1]) * EVA,
                            nor_loadings_forwards[3, 1] * `S%`)) |> 
  ungroup() |> 
  group_by(Player) |> 
  mutate(mean_zscore = mean(total_zscore, na.rm = TRUE))


wmc_forwards <- forwards_60 |>
  mutate(
    EVG_wmc = (
      if_else(!is.na(GP_2016) & !is.na(GP_2017) & !is.na(`EVG/60_2016`) & !is.na(`EVG/60_2017`), (GP_2016 + GP_2017) * abs(`EVG/60_2016` - `EVG/60_2017`), 0) +
        if_else(!is.na(GP_2017) & !is.na(GP_2018) & !is.na(`EVG/60_2017`) & !is.na(`EVG/60_2018`), (GP_2017 + GP_2018) * abs(`EVG/60_2017` - `EVG/60_2018`), 0) +
        if_else(!is.na(GP_2018) & !is.na(GP_2019) & !is.na(`EVG/60_2018`) & !is.na(`EVG/60_2019`), (GP_2018 + GP_2019) * abs(`EVG/60_2018` - `EVG/60_2019`), 0) +
        if_else(!is.na(GP_2019) & !is.na(GP_2020) & !is.na(`EVG/60_2019`) & !is.na(`EVG/60_2020`), (GP_2019 + GP_2020) * abs(`EVG/60_2019` - `EVG/60_2020`), 0) +
        if_else(!is.na(GP_2020) & !is.na(GP_2021) & !is.na(`EVG/60_2020`) & !is.na(`EVG/60_2021`), (GP_2020 + GP_2021) * abs(`EVG/60_2020` - `EVG/60_2021`), 0) +
        if_else(!is.na(GP_2021) & !is.na(GP_2022) & !is.na(`EVG/60_2021`) & !is.na(`EVG/60_2022`), (GP_2021 + GP_2022) * abs(`EVG/60_2021` - `EVG/60_2022`), 0) +
        if_else(!is.na(GP_2022) & !is.na(GP_2023) & !is.na(`EVG/60_2022`) & !is.na(`EVG/60_2023`), (GP_2022 + GP_2023) * abs(`EVG/60_2022` - `EVG/60_2023`), 0)
    ) / 
      (
        sum(if_else(!is.na(GP_2016) & !is.na(GP_2017), GP_2016 + GP_2017, 0), 
            if_else(!is.na(GP_2017) & !is.na(GP_2018), GP_2017 + GP_2018, 0), 
            if_else(!is.na(GP_2018) & !is.na(GP_2019), GP_2018 + GP_2019, 0), 
            if_else(!is.na(GP_2019) & !is.na(GP_2020), GP_2019 + GP_2020, 0), 
            if_else(!is.na(GP_2020) & !is.na(GP_2021), GP_2020 + GP_2021, 0), 
            if_else(!is.na(GP_2021) & !is.na(GP_2022), GP_2021 + GP_2022, 0), 
            if_else(!is.na(GP_2022) & !is.na(GP_2023), GP_2022 + GP_2023, 0))
      ),
    
    EVA_wmc = (
      if_else(!is.na(GP_2016) & !is.na(GP_2017) & !is.na(`EVA/60_2016`) & !is.na(`EVA/60_2017`), (GP_2016 + GP_2017) * abs(`EVA/60_2016` - `EVA/60_2017`), 0) +
        if_else(!is.na(GP_2017) & !is.na(GP_2018) & !is.na(`EVA/60_2017`) & !is.na(`EVA/60_2018`), (GP_2017 + GP_2018) * abs(`EVA/60_2017` - `EVA/60_2018`), 0) +
        if_else(!is.na(GP_2018) & !is.na(GP_2019) & !is.na(`EVA/60_2018`) & !is.na(`EVA/60_2019`), (GP_2018 + GP_2019) * abs(`EVA/60_2018` - `EVA/60_2019`), 0) +
        if_else(!is.na(GP_2019) & !is.na(GP_2020) & !is.na(`EVA/60_2019`) & !is.na(`EVA/60_2020`), (GP_2019 + GP_2020) * abs(`EVA/60_2019` - `EVA/60_2020`), 0) +
        if_else(!is.na(GP_2020) & !is.na(GP_2021) & !is.na(`EVA/60_2020`) & !is.na(`EVA/60_2021`), (GP_2020 + GP_2021) * abs(`EVA/60_2020` - `EVA/60_2021`), 0) +
        if_else(!is.na(GP_2021) & !is.na(GP_2022) & !is.na(`EVA/60_2021`) & !is.na(`EVA/60_2022`), (GP_2021 + GP_2022) * abs(`EVA/60_2021` - `EVA/60_2022`), 0) +
        if_else(!is.na(GP_2022) & !is.na(GP_2023) & !is.na(`EVA/60_2022`) & !is.na(`EVA/60_2023`), (GP_2022 + GP_2023) * abs(`EVA/60_2022` - `EVA/60_2023`), 0)
    ) / 
      (
        sum(if_else(!is.na(GP_2016) & !is.na(GP_2017), GP_2016 + GP_2017, 0), 
            if_else(!is.na(GP_2017) & !is.na(GP_2018), GP_2017 + GP_2018, 0), 
            if_else(!is.na(GP_2018) & !is.na(GP_2019), GP_2018 + GP_2019, 0), 
            if_else(!is.na(GP_2019) & !is.na(GP_2020), GP_2019 + GP_2020, 0), 
            if_else(!is.na(GP_2020) & !is.na(GP_2021), GP_2020 + GP_2021, 0), 
            if_else(!is.na(GP_2021) & !is.na(GP_2022), GP_2021 + GP_2022, 0), 
            if_else(!is.na(GP_2022) & !is.na(GP_2023), GP_2022 + GP_2023, 0))
      ),
    
    `S%_wmc` = (
      if_else(!is.na(GP_2016) & !is.na(GP_2017) & !is.na(`S%_2016`) & !is.na(`S%_2017`), (GP_2016 + GP_2017) * abs(`S%_2016` - `S%_2017`), 0) +
        if_else(!is.na(GP_2017) & !is.na(GP_2018) & !is.na(`S%_2017`) & !is.na(`S%_2018`), (GP_2017 + GP_2018) * abs(`S%_2017` - `S%_2018`), 0) +
        if_else(!is.na(GP_2018) & !is.na(GP_2019) & !is.na(`S%_2018`) & !is.na(`S%_2019`), (GP_2018 + GP_2019) * abs(`S%_2018` - `S%_2019`), 0) +
        if_else(!is.na(GP_2019) & !is.na(GP_2020) & !is.na(`S%_2019`) & !is.na(`S%_2020`), (GP_2019 + GP_2020) * abs(`S%_2019` - `S%_2020`), 0) +
        if_else(!is.na(GP_2020) & !is.na(GP_2021) & !is.na(`S%_2020`) & !is.na(`S%_2021`), (GP_2020 + GP_2021) * abs(`S%_2020` - `S%_2021`), 0) +
        if_else(!is.na(GP_2021) & !is.na(GP_2022) & !is.na(`S%_2021`) & !is.na(`S%_2022`), (GP_2021 + GP_2022) * abs(`S%_2021` - `S%_2022`), 0) +
        if_else(!is.na(GP_2022) & !is.na(GP_2023) & !is.na(`S%_2022`) & !is.na(`S%_2023`), (GP_2022 + GP_2023) * abs(`S%_2022` - `S%_2023`), 0)
    ) / 
      (
        sum(if_else(!is.na(GP_2016) & !is.na(GP_2017), GP_2016 + GP_2017, 0), 
            if_else(!is.na(GP_2017) & !is.na(GP_2018), GP_2017 + GP_2018, 0), 
            if_else(!is.na(GP_2018) & !is.na(GP_2019), GP_2018 + GP_2019, 0), 
            if_else(!is.na(GP_2019) & !is.na(GP_2020), GP_2019 + GP_2020, 0), 
            if_else(!is.na(GP_2020) & !is.na(GP_2021), GP_2020 + GP_2021, 0), 
            if_else(!is.na(GP_2021) & !is.na(GP_2022), GP_2021 + GP_2022, 0), 
            if_else(!is.na(GP_2022) & !is.na(GP_2023), GP_2022 + GP_2023, 0))
      )
  ) |> 
  select(Player, PlayerID, EVG_wmc, EVA_wmc, `S%_wmc`)





