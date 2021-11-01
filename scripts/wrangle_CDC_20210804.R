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
	
	# ###  Duplicated Vaccination 12-17: One Dose  ###
	# # "People with at least 1 dose - ages 12-17"
	# mutate(
	# 	`People with at least 1 dose - 12-17` = case_when(
	# 		# If the old column name doesn't have missing data, move it to the new
	# 		#   column
	# 		!is.na(`People with at least 1 dose - ages 12-17`) ~
	# 			`People with at least 1 dose - ages 12-17`,
	# 		# If the new new column is not missing data, keep it
	# 		!is.na(`People with at least 1 dose - 12-17`) ~
	# 			`People with at least 1 dose - 12-17`
	# 	)
	# ) %>% 
	# select(
	# 	-`People with at least 1 dose - ages 12-17`
	# ) %>%
	
	###  Duplicated Vaccination 12-17: Fully Vaccinated  ###
	# "People who are fully vaccinated - ages 12-17"
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
	
	###  Duplicated Vaccination 12-17: Fully Vaccinated %  ###
	# "People who are fully vaccinated as % of population - ages 12-17"
	mutate(
		`People who are fully vaccinated as % of population - 12-17` = case_when(
			# If the old column name doesn't have missing data, move it to the new
			#   column
			!is.na(`People who are fully vaccinated as % of population - ages 12-17`) ~
				`People who are fully vaccinated as % of population - ages 12-17`,
			# If the new new column is not missing data, keep it
			!is.na(`People who are fully vaccinated as % of population - 12-17`) ~
				`People who are fully vaccinated as % of population - 12-17`
		)
	) %>% 
	select(
		-`People who are fully vaccinated as % of population - ages 12-17`
	) %>%
	
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
	cleanData_df, "data_clean/cleaned_CDC_COVID_data_20211018.csv"
)

# Restart here


