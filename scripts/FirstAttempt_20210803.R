# Inspect CDC Data
# Gabriel Odom and Anny Rodriguez
# 2021-08-03

library(readxl)
library(lubridate)
library(tidyverse)

# Data source:
# https://healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9


######  Apply to All Data  ####################################################

###  Vector of Filenames  ###
allFilePaths_char <- paste0("data_CDC_raw/", list.files("data_CDC_raw/"))
names(allFilePaths_char) <- paste0(
  "date_",
  str_extract(
    allFilePaths_char, pattern = "[:digit:]{8}"
  )
)


###  Columns of Interest  ###
colsOfInterest <- c(
  # Demographics
  "County",
  "State Abbreviation",
  "Population",
  # Cases, Testing, and Deaths
  "Cases - last 7 days",
  "Cumulative cases",
  "Viral (RT-PCR) lab test positivity rate - last 7 days (may be an underestimate due to delayed reporting)",
  "Total RT-PCR diagnostic tests - last 7 days (may be an underestimate due to delayed reporting)",
  # Hospitalizations
  "Confirmed COVID-19 admissions - last 7 days",
  "Suspected COVID-19 admissions - last 7 days",
  "% inpatient beds occupied by COVID-19 patient",
  "Total inpatient beds among hospitals reporting - last 7 days",
  "% ventilators in use by COVID-19 patient",
  "Total ventilators among hospitals reporting - last 7 days",
  "Deaths - last 7 days",
  "Cumulative deaths",
  # Vaccinations
  "People with at least 1 dose",
  "People who are fully vaccinated",
  "People with at least 1 dose - ages 65+",
  "People who are fully vaccinated - ages 65+",
  "People who are fully vaccinated as % of population - ages 65+",
  "People with at least 1 dose - 12-17",
  "People who are fully vaccinated - 12-17",
  "People who are fully vaccinated as % of population - 12-17"
)

# Misspellings
otherNames_char <- c(
  # Only for 17 December
  "Viral (RT-PCR) lab test positivity rate - last 7 days (may exhibit anomalies due to delayed reporting)",
  # effective June 6
  "People with at least 1 dose - ages 12-17",
  "People who are fully vaccinated - ages 12-17",
  "People who are fully vaccinated as % of population - ages 12-17"
)



t0 <- Sys.time()
allData_ls <- 
  map(
    .x = allFilePaths_char,
    .f = ~{
      read_excel(
        path = .x, 
        sheet = "Counties",
        skip = 1
      ) %>% 
        select(
          any_of(c(colsOfInterest, otherNames_char))
        ) %>% 
        filter(`State Abbreviation` == "FL")
    }
  )
t1 <- Sys.time()
t1 - t0
# 1.025038 min

allData_df <- bind_rows(allData_ls, .id = "DataSet")



######  Clean Up Columns  #####################################################
cleanData_df <- 
  allData_df %>% 
  ###  Duplicated Viral PCR Testing  ###
  rename(
    `Viral (RT-PCR) lab test positivity rate - last 7 days` = 
      `Viral (RT-PCR) lab test positivity rate - last 7 days (may be an underestimate due to delayed reporting)`
  ) %>% 
  mutate(
    `Viral (RT-PCR) lab test positivity rate - last 7 days` = case_when(
      # If the old column name doesn't have missing data, move it to the new
      #   column
      !is.na(`Viral (RT-PCR) lab test positivity rate - last 7 days (may exhibit anomalies due to delayed reporting)`) ~
        `Viral (RT-PCR) lab test positivity rate - last 7 days (may exhibit anomalies due to delayed reporting)`,
      # If the new new column is not missing data, keep it
      !is.na(`Viral (RT-PCR) lab test positivity rate - last 7 days`) ~
        `Viral (RT-PCR) lab test positivity rate - last 7 days`
    )
  ) %>% 
  select(
    -`Viral (RT-PCR) lab test positivity rate - last 7 days (may exhibit anomalies due to delayed reporting)`
  ) %>% 
  
  ###  Duplicated Vaccination 12-17  ###
  mutate(
    `People who are fully vaccinated - 12-17` = case_when(
      # If the old column name doesn't have missing data, move it to the new
      #   column
      !is.na(`People who are fully vaccinated - ages 12-17`) ~
        `People who are fully vaccinated - ages 12-17`,
      # If the new new column is not missing data, keep it
      !is.na(`People who are fully vaccinated - 12-17`) ~
        `People who are fully vaccinated - 12-17`
    )
  ) %>% 
  select(
    -`People who are fully vaccinated - ages 12-17`
  ) %>%
  # TO DO: COPY THIS FIX FOR THE OTHER TWO "ages" FEATURES.
  
  ###  Fix Dates  ###
  mutate(
    Date_char = str_remove(DataSet, "date_")
  ) %>% 
  mutate(
    Date = ymd(Date_char)
  ) %>% 
  select(-DataSet, -Date_char) %>% 
  select(Date, everything())


write_csv(
  cleanData_df, "cleaned_CDC_COVID_data_20210729.csv"
)

# Restart here



######  Data for Plots  #######################################################


###  Setup  ###
library(lubridate)
library(ggpubr)
library(tidyverse)

cleanData_df <- read_csv(
  "cleaned_CDC_COVID_data_20210729.csv",
  # read_csv() uses the first 1000 rows only, but we know already that we have
  #   a bunch of missing data for the first few months
  guess_max = 10000
) %>% 
  mutate(County = str_remove(County, pattern = " County, FL"))

# # population (required for vaccine proportions); source:
# # https://worldpopulationreview.com/us-counties/states/fl
# populations_df <-
# 	read_csv(file = "./county_population_FL_2021.csv") %>% 
# 	mutate(County = str_remove(CTYNAME, pattern = " County")) %>% 
# 	select(County, pop2021)

# UPDATE 2021-08-02, per Prof. Trepka, use this instead:
populations_df <-
  read_csv("county_pop_clean_20210802.csv") %>% 
  # Issues with matching names for counties 13, 55, 56, 63; 
  mutate(
    County = case_when(
      County == "Desoto" ~ "DeSoto",
      County == "Saint Johns" ~ "St. Johns",
      County == "Saint Lucie" ~ "St. Lucie",
      TRUE ~ County
    )
  )

# Cleaning script: "scripts/clean_provisional_population_20210802.R"
# Get the following values for Miami-Dade: 2,918,507 (total) & 2523478 (12+)

cleanData2_df <- 
  cleanData_df %>% 
  left_join(populations_df, by = "County") %>% 
  group_by(County) %>% 
  mutate(
    `Proportion Vaccinated, Total` = 
      `People who are fully vaccinated` / TotalPopulation,
    `Proportion Vaccinated, 65+` = 
      `People who are fully vaccinated - ages 65+` / Population65Up
  )



######  Plots  ################################################################
###  Region Subset  ###
county_char <- "Miami-Dade"
county_char <- c("Miami-Dade", "Broward", "Palm Beach")
county_char <- c("Escambia", "Santa Rosa")


###  Cases  ###
cases_gg <- 
  ggplot(
    data = cleanData_df %>% 
      filter(County %in% county_char) %>% 
      filter(Date >= ymd("20210101")) %>% 
      mutate(aveCases = `Cases - last 7 days` / 7)
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  aes(
    x = Date, y = aveCases,
    group = County, colour = County
  ) +
  labs(
    title = "COVID-19 Cases",
    x = "Average No. Cases per Day: 7-Day Rolling Window"
  ) + 
  geom_point() +
  geom_smooth(se = FALSE)

cases_gg
# This is showing 20k positive cases per day in MDC; this should be divided by
#   7.


###  Proportion Positive  ###
# This value is problematic. It does not match what we have seen in the FLDoH
#   data.
# UPDATE 2021-08-02: Mary Jo reminded us that FLDoH removes cases that had tested
#   positive before. The CDC does not. This may be an issue
propPos_gg <- 
  ggplot(
    data = cleanData_df %>% 
      filter(County %in% county_char) %>% 
      filter(Date >= ymd("20210101")) %>% 
      rename(
        `Proportion of Positive Tests - last 7 days` = 
          `Viral (RT-PCR) lab test positivity rate - last 7 days` 
      )
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  aes(
    x = Date,
    y = `Proportion of Positive Tests - last 7 days`,
    group = County,
    colour = County
  ) +
  labs(
    title = "Proportion of Positive COVID-19 Tests"
  ) + 
  geom_point() +
  geom_smooth(se = FALSE)

propPos_gg



###  Hospitalisations  ###
hosp_gg <- 
  ggplot(
    data = cleanData_df %>% 
      filter(County %in% county_char) %>% 
      filter(Date >= ymd("20210101")) %>% 
      mutate(
        `COVID-19 Patients - last 7 days` = 
          `% inpatient beds occupied by COVID-19 patient` *
          `Total inpatient beds among hospitals reporting - last 7 days`
      )
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  aes(
    x = Date,
    y = `COVID-19 Patients - last 7 days`,
    group = County,
    colour = County
  ) +
  labs(
    title = "Count of COVID-19 Hospitalisations"
  ) + 
  geom_point() +
  geom_smooth(se = FALSE)

hosp_gg


###  Vaccinations  ###
vaxx_gg <- 
  ggplot(
    data = cleanData2_df %>% 
      filter(County %in% county_char) %>% 
      filter(Date >= ymd("20210401")) 
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  aes(
    x = Date,
    y = `Proportion Vaccinated, Total`,
    group = County,
    colour = County
  ) +
  labs(
    title = "Fully-Vaccinated Proportion"
  ) + 
  scale_y_continuous(limits = c(0, 0.7)) +
  geom_point() +
  geom_smooth(se = FALSE)

vaxx_gg


ggarrange(
  cases_gg, propPos_gg, hosp_gg, vaxx_gg,
  ncol = 2, nrow = 2,
  common.legend = TRUE, legend = "bottom"
)

