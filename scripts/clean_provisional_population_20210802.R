# Wrangle Provisional FL Population
# Gabriel Odom
# 2021-08-02

# Mary Jo gave us an .xlsx file with the population by county and age group. 
#   It's a mess. Source:
#   http://www.flhealthcharts.com/FLQUERY_New/Population/Count

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
	mutate(Population = as.integer(Population)) %>% 
	mutate(
		`Age Groups` = case_when(
			Ages == "Total" ~ "Total",
			Ages %in% c("<1", "1-4", "5-9", "10", "11") ~ "0-11",
			Ages %in% c("65-74", "75-84", "85+") ~ "65+",
			TRUE ~ "12-64"
		)
	) %>% 
	mutate(
		AllAges = case_when(
			`Age Groups` == "Total" ~ TRUE,
			TRUE ~ FALSE
		),
		`Ages12+` = case_when(
			`Age Groups` %in% c("12-64", "65+") ~ TRUE,
			TRUE ~ FALSE
		),
		`Ages65+` = case_when(
			`Age Groups` == "65+" ~ TRUE,
			TRUE ~ FALSE
		)
	) %>% 
	select(-`Age Groups`)


###  Subset and Join  ###
totalPop_df <- 
	populations3_df %>% 
	filter(AllAges) %>% 
	select(County, TotalPopulation = Population)

pop12Up_df <- 
	populations3_df %>% 
	filter(`Ages12+`) %>% 
	group_by(County) %>% 
	summarise(Population12Up = sum(Population)) %>% 
	select(County, Population12Up)

pop65Up_df <- 
	populations3_df %>% 
	filter(`Ages65+`) %>% 
	group_by(County) %>% 
	summarise(Population65Up = sum(Population)) %>% 
	select(County, Population65Up)

populationSummary_df <- 
	pop12Up_df %>% 
	left_join(pop65Up_df, by = "County") %>% 
	left_join(totalPop_df, by = "County")

write_csv(populationSummary_df, file = "county_pop_clean_20210802.csv")
