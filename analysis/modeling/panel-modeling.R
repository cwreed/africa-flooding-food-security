library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(plm)
library(dotwhisker)
library(latex2exp)

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

panel_data_adm2[,c('mean_ipc_diff', 'no_floods_diff', 'total_flood_dur_diff', 'total_flood_area_diff')] <- scale(panel_data_adm2[,c('mean_ipc_diff', 'no_floods_diff', 'total_flood_dur_diff', 'total_flood_area_diff')], center = F)

granger_data_adm2 <- data_adm2 %>%
	group_by(FIDcalc) %>%
	arrange(FIDcalc, datetime_start) %>%
  mutate(time_index = as.numeric(as.factor(datetime_start))) %>%
	filter((sum(no_flood_events != 0, na.rm = T) >= 4) & (sum(mean_ipc_diff != 0, na.rm = T) >= 5)) %>%
	pdata.frame(index = c('FIDcalc', 'time_index'))

granger_data_adm2[,c('mean_ipc_diff', 'no_floods_diff', 'total_flood_dur_diff', 'total_flood_area_diff')] <- scale(granger_data_adm2[,c('mean_ipc_diff', 'no_floods_diff', 'total_flood_dur_diff', 'total_flood_area_diff')], center = F)


pcdtest(granger_data_adm2$mean_ipc_diff, test = "cd", na.action = na.omit)
	
.formula <- "mean_ipc_diff ~ plm::lag(no_floods_diff, 0:4) +
  plm::lag(total_flood_area_diff, 0:4) + 
  plm::lag(total_flood_dur_diff, 0:4)"

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

country_dfs_adm2 <- panel_data_adm2 %>%
	group_by(Name) %>%
	group_split()
	
country_dfs_adm2 <- lapply(country_dfs_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(country_dfs_adm2) <- sort(unique(panel_data_adm2$Name))

country_granger_dfs_adm2 <- granger_data_adm2 %>%
	group_by(Name) %>%
	group_split()
	
country_granger_dfs_adm2 <- lapply(country_granger_dfs_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(country_granger_dfs_adm2) <- sort(unique(granger_data_adm2$Name))

### Full models 
all_africa_full <- plm(.formula, data = panel_data_adm2, model = "random", random.method = "walhus")

outfolder = "../../outputs/model-summaries/"

sink(paste(outfolder, "all_africa_scaled_full_data.txt"))
print(pcdtest(all_africa_full, test = "cd", na.action = na.omit))
summary(all_africa_full, vcov=function(x) vcovSCC(x, type = 'HC1', maxlag=4))
sink()

sink(paste(outfolder, "regional_scaled_full_data.txt"))
for (name in region_names) {
	model <- plm(.formula, data = regional_dfs_adm2[[name]], model = "random", random.method = 'walhus')
	print(name)
	print(pcdtest(model, test = "cd"))
	print(summary(model, vcov=function(x) vcovSCC(x, type = 'HC1', maxlag=4)))
	cat("\n")
}
sink()

sink(paste(outfolder, "countries_scaled_full_data.txt"))
for (name in sort(unique(panel_data_adm2$Name))){
	print(name)
	tryCatch({
		model <- plm(.formula, data = country_dfs_adm2[[name]], model = "random", random.method = 'walhus')
		print(pcdtest(model, test = "cd"))
		print(summary(model, vcov=function(x) vcovSCC(x, type = 'HC1', maxlag=4)))
	},
		error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
	cat("\n")
}
sink()

all_africa_granger <- plm(.formula, data = granger_data_adm2, model = "random", random.method = "walhus")


sink(paste(outfolder, "all_africa_scaled_granger_data.txt"))
print(pcdtest(all_africa_granger, test = "cd"))
summary(all_africa_granger, vcov=function(x) vcovSCC(x, type = 'HC1', maxlag=4))
sink()

sink(paste(outfolder, "regional_scaled_granger_data.txt"))
for (name in region_names) {
	model <- plm(.formula, data = regional_granger_dfs_adm2[[name]], model = "random", random.method = 'walhus')
	print(name)
	print(pcdtest(model, test = "cd"))
	print(summary(model, vcov=function(x) vcovSCC(x, type = 'HC1', maxlag=4)))
	cat("\n")
}
sink()

sink(paste(outfolder, "countries_scaled_granger_data.txt"))
for (name in sort(unique(granger_data_adm2$Name))){
	print(name)
	tryCatch({
		model <- plm(.formula, data = country_granger_dfs_adm2[[name]], model = "random", random.method = 'walhus')
		print(pcdtest(model), test = "cd")
		print(summary(model, vcov=function(x) vcovSCC(x, type = 'HC1', maxlag=4)))
	},
		error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
	cat("\n")
}
sink()


