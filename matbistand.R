# Add a column to statsys_ten to identify aid targeting agriculture, fishing and food securing.
# Saving final dataset as df_food in output folder

# Load packages and data --------------------------------------------------

library(noradstats)
library(tidyverse)
library(janitor)
library(here)

# noradstats::get_aiddata("statsys_ten.csv")
df_raw <- read_aiddata("statsys_ten.csv")
df_raw <- clean_names(df_raw)

# Including a column "Food" to the original database-------------------------------------------------------------------------

df_food <- df_raw |>
  mutate(
    food = case_when(
      dac_main_sector_code_name %in% c("311 - Agriculture", "313 - Fishing") |
        dac_sub_sector_code_name %in% c(
          "71 - Food security policy and administrative management",
          "72 - Household food security programmes ",
          "73 - Food safety and quality",
          "10 - Food assistance"
        ) |
        agreement_partner == "GCDT - Global Crop Diversity Trust" ~ "Long-term earmarked aid to agriculture, fishing and food security",
      type_of_assistance == "Core contributions to multilat" &
        agreement_partner %in% c(
          "CGIAR - Consultative Group on International Agricultural Research",
          "FAO - Food and Agricultural Organization of the United Nations",
          "IFAD - International Fund for Agricultural Development"
        ) ~ "Multilateral long-term aid to agriculture, fishing and food security",
      
      dac_sub_sector_code_name == "40 - Emergency food assistance" ~ "Earmarked emergency aid to agriculture, fishing and food security",
      type_of_assistance == "Core contributions to multilat" &
        agreement_partner == "WFP - World Food Programme" ~ "Multilateral emergency aid to agriculture, fishing and food security",
      TRUE ~ "Not relevant"
    )
  )


# Testing that the totals are ok.
df_food |>
  filter(type_of_flow == "ODA",
         type_of_agreement != "Rammeavtale",
         year >= 2015) |> 
  filter(food != "Not relevant") |> 
  group_by(year) |> 
  summarise(nok_mill = sum(disbursed_mill_nok))


# Saving updated database -------------------------------------------------

writexl::write_xlsx(df_food, here("output", "df_food.xlsx"))
