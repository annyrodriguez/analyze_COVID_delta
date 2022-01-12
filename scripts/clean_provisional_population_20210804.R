# Wrangle Provisional FL Population
# Gabriel Odom and Anny Rodriguez
# 2021-08-02; updated 2022-01-12

# Mary Jo gave us an .xlsx file with the population by county and age group. 
#   It's a mess. Source:
#   http://www.flhealthcharts.com/FLQUERY_New/Population/Count

library(readxl)
library(tidyverse)

###  Read the Raw Data  ###
populations_df <- read_excel(
	"./county_population_20210515.xlsx",
	sheet = "Sheet1",
	skip = 3
) %>% 
	select(Year, starts_with("2021")) %>% 
	slice(-2)


###  Fix Column Names  ###
myColNames_char <-
	populations_df %>% 
	slice(1) %>% 
	unlist() %>% 
	unname()
myColNames_char[1] <- "County"
myColNames_char[ which(myColNames_char == "10-14") ] <- 
	c(
		as.character(10:14), "Total:10-14"
	)
colnames(populations_df) <- myColNames_char
populations2_df <- 
	populations_df %>% 
	slice(-1) %>% 
	select(-`Total:10-14`)


###  Pivot Data and Summarise  ###
populations3_df <- 
	populations2_df %>% 
	pivot_longer(
		cols = `<1`:Total,
		names_to = "Ages", 
		values_to = "Population"
	) %>% 
	mutate(
		Population = str_remove_all(Population, pattern = ",")
	) %>% 
	mutate(Population = as.integer(Population))


###  Create 15-19 Age Groups  ###
pops15_19_df <- 
	populations3_df %>%
	filter(Ages == "15-19") %>%
	group_by(County) %>% 
	summarise(
		`15` = Population / 5,
		`16` = Population / 5,
		`17` = Population / 5,
		`18` = Population / 5,
		`19` = Population / 5
	) %>% 
	pivot_longer(
		cols = `15`:`19`,
		names_to = "Ages", 
		values_to = "Population"
	)

# NOTE: the age groups are no longer in ascending order
populations4_df <- 
	populations3_df %>% 
	filter(Ages != "15-19") %>% 
	bind_rows(pops15_19_df) %>% 
	arrange(County)


###  Add Groupings  ###
popsWithGroups_df <- 
	populations4_df %>% 
	mutate(
		`Age Groups` = case_when(
			Ages == "Total" ~ "Total",
			Ages %in% c("<1", "1-4") ~ "0-4",
			Ages %in% c("5-9", "10", "11") ~ "5-11",
			Ages %in% as.character(12:17) ~ "12-17",
			Ages %in% c("65-74", "75-84", "85+") ~ "65+",
			TRUE ~ "18-64"
		)
	) %>% 
	mutate(
		AllAges     = `Age Groups` == "Total",
		# This requires us to break up 15:19; done on 2021-08-04
		`Ages5-11`  = `Age Groups` == "5-11",
		`Ages12-17` = `Age Groups` == "12-17",
		`Ages5+`    = `Age Groups` %in% c("5-11", "12-17", "18-64", "65+"),
		`Ages12+`   = `Age Groups` %in% c("12-17", "18-64", "65+"),
		`Ages65+`   = `Age Groups` == "65+"
	) %>% 
	select(-`Age Groups`)


###  Subset and Join  ###
totalPop_df <- 
	popsWithGroups_df %>% 
	filter(AllAges) %>% 
	select(County, `Total Population` = Population)

pop511_df <- 
	popsWithGroups_df %>% 
	filter(`Ages5-11`) %>% 
	group_by(County) %>% 
	summarise(`Population 5-11` = sum(Population)) %>% 
	select(County, `Population 5-11`)

pop1217_df <- 
	popsWithGroups_df %>% 
	filter(`Ages12-17`) %>% 
	group_by(County) %>% 
	summarise(`Population 12-17` = sum(Population)) %>% 
	select(County, `Population 12-17`)

pop5Up_df <- 
	popsWithGroups_df %>% 
	filter(`Ages5+`) %>% 
	group_by(County) %>% 
	summarise(`Population 5+` = sum(Population)) %>% 
	select(County, `Population 5+`)

pop12Up_df <- 
	popsWithGroups_df %>% 
	filter(`Ages12+`) %>% 
	group_by(County) %>% 
	summarise(`Population 12+` = sum(Population)) %>% 
	select(County, `Population 12+`)

pop65Up_df <- 
	popsWithGroups_df %>% 
	filter(`Ages65+`) %>% 
	group_by(County) %>% 
	summarise(`Population 65+` = sum(Population)) %>% 
	select(County, `Population 65+`)

populationSummary_df <- 
	pop511_df %>% 
	left_join(pop1217_df, by = "County") %>% 
	left_join(pop5Up_df, by = "County") %>% 
	left_join(pop12Up_df, by = "County") %>% 
	left_join(pop65Up_df, by = "County") %>% 
	left_join(totalPop_df, by = "County")


###  Save Results  ###
write_csv(
	populationSummary_df,
	file = "data_clean/county_pop_clean_20220112.csv"
)
