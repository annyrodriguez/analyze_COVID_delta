# Animating the COVID Graphs
# Anny Rodriguez
# 2021-10-05

library(lubridate)
library(ggpubr)
library(plotly)
library(gganimate)
library(dplyr)
library(gifski)
library(ggplot2)
library(ggthemes)
library(transformr)
library(tidyverse)

# Colourblind friendly colours:
cbbPalette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#000000", "CC70A7"
)

###  Region Subset  ###
# county_char <- "Miami-Dade"
county_char <- c("Miami-Dade", "Broward", "Palm Beach")
# county_char <- c("Escambia", "Santa Rosa")

# We wrangle the CDC data in "scripts/wrangle_CDC_20210803.R"



######  Data for Plots  #######################################################

###  Setup  ###
cleanData_df <- read_csv(
  "data_clean/cleaned_CDC_COVID_data_20211018.csv",
  # read_csv() uses the first 1000 rows only, but we know already that we have
  #   a bunch of missing data for the first few months
  guess_max = 10000
) %>% 
  mutate(County = str_remove(County, pattern = " County, FL"))

populations_df <-
  read_csv("data_clean/county_pop_clean_20210804.csv") %>% 
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
      `People who are fully vaccinated` / `Total Population`,
    `Proportion Vaccinated of Eligible Population` = 
      `People who are fully vaccinated` / `Population 12+`,
    `Proportion Vaccinated, 65+` = 
      `People who are fully vaccinated - ages 65+` / `Population 65+`,
    `Proportion Vaccinated, 12-17` = 
      `People who are fully vaccinated - 12-17` / `Population 12-17`
  )

### New dataset with all the filters and mutations ###

cleanData3_df <-
  cleanData2_df %>%
  filter(County %in% county_char) %>%
  filter(Date >= ymd("20210101")) %>%
  mutate(aveCases = `Cases - last 7 days` / 7) %>%
  rename(
    `Proportion of Positive Tests - last 7 days` =
      `Viral (RT-PCR) lab test positivity rate - last 7 days`) %>%
  mutate(
    `COVID-19 Patients - last 7 days` =
      `% inpatient beds occupied by COVID-19 patient` *
      `Total inpatient beds among hospitals reporting - last 7 days`
    )
  
  

######  Summary Plots  ########################################################

###  Cases  ###
cases_gg <- 
  ggplot(
    data = cleanData2_df %>% 
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
    x = "Average No. Cases per Day: 7-Day Rolling Window",
    y = "Count of COVID-19 Cases"
  ) + 
  scale_colour_manual(values = cbbPalette) + 
  geom_line()

cases_gg

### gganimate code ###
cases_ggAnimate <- 
  cases_gg + 
  transition_reveal(Date) + 
  ease_aes("linear") +
  ggtitle('COVID-19 Cases',
          subtitle = 'Date: {frame_along}')

animate(cases_ggAnimate)

anim_save("1018cases_ggAnimate.gif", cases_ggAnimate)

### plotly code ###

cases_ggPlotly <-
  plot_ly(
    data = cleanData3_df,
    x = ~Date,
    y = ~aveCases,
    text = ~Date,
    hoverinfo = "text") %>%
  layout(xaxis = list(type = "Date")) %>%
  add_markers(color = ~County,
              frame = ~Date >= ymd("20210101"),
              ids = ~Date)

cases_ggPlotly  %>%
   animation_opts(fram = 1000, easing = "linear", redraw = FALSE)




# This is showing 20k positive cases per day in MDC; this should be divided by
#   7.


###  Proportion Positive  ###
# This value is problematic. It does not match what we have seen in the FLDoH
#   data.
# UPDATE 2021-08-02: Mary Jo reminded us that FLDoH removes cases that had tested
#   positive before. The CDC does not. This may be an issue
propPos_gg <- 
  ggplot(
    data = cleanData2_df %>% 
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
  scale_colour_manual(values = cbbPalette) + 
  geom_line()

propPos_gg

### Animation ###
propPos_ggAnimate <- 
  propPos_gg + 
  transition_reveal(Date) + 
  ease_aes("linear") +
  ggtitle('Proportion of Positive COVID-19 Tests',
          subtitle = 'Date: {frame_along}')


animate(propPos_ggAnimate)

anim_save("propPos_ggAnimate1011.gif", propPos_ggAnimate)


###  Hospitalisations  ###
hosp_gg <- 
  ggplot(
    data = cleanData2_df %>% 
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
  scale_colour_manual(values = cbbPalette) + 
  geom_line()

hosp_gg

### Animation ###
hosp_ggAnimate <- 
  hosp_gg + 
  transition_reveal(Date) + 
  ease_aes("linear") +
  ggtitle('Count of COVID-19 Hospitalisations',
          subtitle = 'Date: {frame_along}')


animate(hosp_ggAnimate)

anim_save("1018hosp_ggAnimate.gif", hosp_ggAnimate)



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
  scale_y_continuous(limits = c(0, 0.8)) +
  scale_colour_manual(values = cbbPalette) + 
  geom_line()

vaxx_gg

### Animations ###

vaxx_ggAnimate <- 
  vaxx_gg + 
  transition_reveal(Date) + 
  ease_aes("linear") +
  ggtitle('Fully-Vaccinated Proportion',
          subtitle = 'Date: {frame_along}')


animate(vaxx_ggAnimate)

anim_save("vaxx_ggAnimate1011.gif", vaxx_ggAnimate)

### GG Arrange & Save ###
ggarrange(
  cases_gg, propPos_gg, hosp_gg, vaxx_gg,
  ncol = 2, nrow = 2,
  common.legend = TRUE, legend = "bottom"
)

ggsave(
  "figures/sfl_summary_20211004.pdf",
  # 16 x 9 widescreen format, but sized to potentially fit on a screen
  width = 12, height = 6.75, units = "in"
)



######  Vaccination-Specific Plots  ###########################################

###  All Vaxxed out of Total  ###
vaxTotal_gg <- 
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
  scale_y_continuous(limits = c(0, 1), breaks = 0:10/10) +
  scale_colour_manual(values = cbbPalette) + 
  geom_line()

vaxTotal_gg

### Animations ###

vaxTotal_ggAnimate <- 
  vaxTotal_gg + 
  transition_reveal(Date) + 
  ease_aes("linear") +
  ggtitle('Fully-Vaccinated Proportion',
          subtitle = 'Date: {frame_along}')


animate(vaxTotal_ggAnimate)

anim_save("vaxTotal_ggAnimate1011.gif", vaxTotal_ggAnimate)



###  All Vaxxed out of Eligible  ###
vaxEligible_gg <- 
  ggplot(
    data = cleanData2_df %>% 
      filter(County %in% county_char) %>% 
      filter(Date >= ymd("20210401")) 
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  aes(
    x = Date,
    y = `Proportion Vaccinated of Eligible Population`,
    group = County,
    colour = County
  ) +
  labs(
    title = "Eligible Vaccinated Proportion"
  ) + 
  scale_y_continuous(limits = c(0, 1), breaks = 0:10/10) +
  scale_colour_manual(values = cbbPalette) + 
  geom_line()

vaxEligible_gg


### Animations ###

vaxEligible_ggAnimate <- 
  vaxEligible_gg + 
  transition_reveal(Date) + 
  ease_aes("linear") +
  ggtitle('Eligible Vaccinated Proportion',
          subtitle = 'Date: {frame_along}')


animate(vaxEligible_ggAnimate)

anim_save(filename = "vaxEligible_ggAnimate1011.gif", animation =  vaxEligible_ggAnimate, path = NULL)


###  65+ Vaxxed out of 65+ Population  ###
vax65Plus_gg <- 
  ggplot(
    data = cleanData2_df %>% 
      filter(County %in% county_char) %>% 
      filter(Date >= ymd("20210401")) 
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  aes(
    x = Date,
    y = `Proportion Vaccinated, 65+`,
    group = County,
    colour = County
  ) +
  labs(
    title = "65+ Vaccinated Proportion"
  ) + 
  scale_y_continuous(limits = c(0, 1), breaks = 0:10/10) +
  scale_colour_manual(values = cbbPalette) + 
  geom_line()

vax65Plus_gg


### Animations ###

vax65Plus_ggAnimate <- 
  vax65Plus_gg + 
  transition_reveal(Date) + 
  ease_aes("linear") +
  ggtitle('65+ Vaccinated Proportion',
          subtitle = 'Date: {frame_along}')


animate(vax65Plus_ggAnimate)

anim_save(filename = "vax65Plus_ggAnimate1011.gif", animation = vax65Plus_ggAnimate, path = NULL)


###  12-17 Vaxxed out of 12-17 Population  ###
vax1217Pop_gg <- 
  ggplot(
    data = cleanData2_df %>% 
      filter(County %in% county_char) %>% 
      filter(Date >= ymd("20210401")) 
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  aes(
    x = Date,
    y = `Proportion Vaccinated, 12-17`,
    group = County,
    colour = County
  ) +
  labs(
    title = "12-17 Vaccinated Proportion"
  ) + 
  scale_y_continuous(limits = c(0, 1), breaks = 0:10/10) +
  scale_colour_manual(values = cbbPalette) + 
  geom_line()

vax1217Pop_gg


### Animations ###

vax1217Pop_ggAnimate <- 
  vax1217Pop_gg + 
  transition_reveal(Date) + 
  ease_aes("linear") +
  ggtitle('12-17 Vaccinated Proportion',
          subtitle = 'Date: {frame_along}')


animate(vax1217Pop_ggAnimate)

anim_save("vax1217Pop_ggAnimate1011.gif", vax1217Pop_ggAnimate)


###  Arrange Graph Grid  ###
ggarrange(
  vaxTotal_gg, vaxEligible_gg, vax65Plus_gg, vax1217Pop_gg,
  ncol = 2, nrow = 2,
  common.legend = TRUE, legend = "bottom"
)

ggsave(
  "figures/sfl_svaccinations_20211004.pdf",
  # 16 x 9 widescreen format, but sized to potentially fit on a screen
  width = 12, height = 6.75, units = "in"
)