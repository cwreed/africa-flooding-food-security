library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(plm)

# Data files:
data_dir <- "../../Data/"
adm2_file <- paste(data_dir, "flood_ipc_seasonal_adm2_timeseries.gpkg", sep = "")

# Read in data
data_adm2 <- st_read(adm2_file)

# Create filtered datasets to mirror Granger results:

panel_data_adm2 <- data_adm2 %>%
	group_by(FIDcalc) %>%
	arrange(FIDcalc, datetime_start) %>%
	mutate(time_index = as.numeric(as.factor(datetime_start))) %>%
	pdata.frame(index = c('FIDcalc', 'time_index'))

granger_data_adm2 <- data_adm2 %>%
	group_by(FIDcalc) %>%
	arrange(FIDcalc, datetime_start) %>%
	mutate(time_index = as.numeric(as.factor(datetime_start))) %>%
	filter((sum(no_flood_events != 0, na.rm = T) >= 4) & (sum(mean_ipc_diff != 0, na.rm = T) >= 5)) %>%
	pdata.frame(index = c('FIDcalc', 'time_index'))

rm(data_adm2)

dfs <- c(panel_data_adm2, granger_data_adm2)

# Begin static panel modeling specification pipeline
# Based on workflow outlined in "Practical Guides to Panel Data Modeling: A Step-by-Step Analysis Using Stata (Park, 2011)

ggplot(data = panel_data_adm2, aes(x = total_flood_dur_diff, y = mean_ipc_diff)) +
	geom_point(aes(color = FIDcalc)) +
	theme(legend.position = 'none')
	
	
hypo_formula <- "mean_ipc_diff ~ plm::lag(no_floods_diff, 0:4) +
  plm::lag(total_flood_area_diff, 0:4) + 
  plm::lag(total_flood_dur_diff, 0:4)"
  
panel_specification <- function(df, .formula) {
	assign("df", df, pos=.GlobalEnv)
	assign(".formula", .formula, pos=.GlobalEnv)
	# Test that the data can actually be pooled:
	tryCatch({
		chow_test <- pooltest(as.formula(.formula), data = df, model = "pooling")
		if (chow_test$p.value < 0.05){
				cat("Model coefficients unstable for pooling.\n")
				cat(chow_test)
				break
		}
		else {
			cat('Model coefficients stable.\n')
		}
	},
	error = function(e){cat("Chow test error\n")
						cat("ERROR :",conditionMessage(e), "\n")})
	
	fixed_test <- pFtest(as.formula(.formula), data = df, effect = 'individual') # fixed effects in a first-differenced model emulate individual trend effects
	random_test <- plmtest(as.formula(.formula), data = df, type = "bp") # we should still expect random effects to be preferred
	
	if ((fixed_test$p.value < 0.05) & (random_test$p.value < 0.05)){
		hausman_test <- phtest(.formula, data = df, vcov = function(x){vcovSCC(x, type = 'HC1', maxlag=4)})
		if (hausman_test$p.value < 0.05) {
			print("Fixed effects model preferred by Hausman")
			print(hausman_test)
			print("\n")
		}
		else {
			print("Random effects model preferred by Hausman")
			print(hausman_test)
			print("\n")
		}
	}
	else if ((fixed_test$p.value < 0.05) & (random_test$p.value >= 0.05)) {
		cat("Fixed effects model preferred\n")
		print(fixed_test)
		cat("\n")
		print(random_test)
	}
	else if ((fixed_test$p.value >= 0.05) & (random_test$p.value < 0.05)) {
		cat("Random effects model preferred\n")
		print(fixed_test)
		cat("\n")
		print(random_test)
	}
	else {
		cat("Pooled model preferred\n")
		print(fixed_test)
		cat("\n")
		print(random_test)
	}
}

panel_specification(panel_data_adm2, hypo_formula)
panel_specification(granger_data_adm2, hypo_formula)

# Repeat for regional models:
regional_dfs_adm2 <- panel_data_adm2 %>%
	group_by(FIDcalc) %>%
	ungroup() %>%
	mutate(group = case_when(Name %in% c('Burkina Faso', 'Mali', 'Niger', 'Nigeria', 'Chad', 'Mauritania') ~ 'West Africa',
							 Name %in% c('Ethiopia', 'Kenya', 'Somalia', 'Sudan', 'South Sudan', 'Uganda') ~ 'East Africa',
							 Name %in% c('Mozambique', 'Malawi', 'Zimbabwe', 'Zambia') ~ 'Southeast Africa')) %>%
	group_by(group) %>%
	group_split()

region_names <- sort(c('West Africa', 'East Africa', 'Southeast Africa'))
regional_dfs_adm2 <- lapply(regional_dfs_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(regional_dfs_adm2) <- region_names

for (region in region_names) {
	print(region)
	panel_specification(regional_dfs_adm2[[region]], hypo_formula)
}

regional_granger_dfs_adm2 <- granger_data_adm2 %>%
	group_by(FIDcalc) %>%
	ungroup() %>%
	mutate(group = case_when(Name %in% c('Burkina Faso', 'Mali', 'Niger', 'Nigeria', 'Chad', 'Mauritania') ~ 'West Africa',
							 Name %in% c('Ethiopia', 'Kenya', 'Somalia', 'Sudan', 'South Sudan', 'Uganda') ~ 'East Africa',
							 Name %in% c('Mozambique', 'Malawi', 'Zimbabwe', 'Zambia') ~ 'Southeast Africa')) %>%
	group_by(group) %>%
	group_split()
regional_granger_dfs_adm2 <- lapply(regional_granger_dfs_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(regional_granger_dfs_adm2) <- region_names

for (region in region_names) {
	print(region)
	panel_specification(regional_granger_dfs_adm2[[region]], hypo_formula)
}

# Repeat for country models:
country_dfs_adm2 <- panel_data_adm2 %>%
	group_by(Name) %>%
	group_split()
	
country_names <- sort(unique(panel_data_adm2$Name))
country_dfs_adm2 <- lapply(country_dfs_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(country_dfs_adm2) <- country_names

for (country in country_names) {
	print(country)
	panel_specification(country_dfs_adm2[[country]], hypo_formula)
}

country_granger_dfs_adm2 <- granger_data_adm2 %>%
	group_by(Name) %>%
	group_split()
country_names_granger <- sort(unique(granger_data_adm2$Name))
country_granger_dfs_adm2 <- lapply(country_granger_dfs_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(country_granger_dfs_adm2) <- country_names_granger

for (country in country_names_granger) {
	print(country)
	panel_specification(country_granger_dfs_adm2[[country]], hypo_formula)
}