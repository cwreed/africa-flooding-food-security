library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(plm)
library(data.table)


data_file <- "../../Data/flood_ipc_seasonal_adm2_timeseries.gpkg"

data <- st_read(data_file)

panel_data <- as_tibble(data) %>%
  select(-contains(c("geom", "flood_1", "flood_29", "flood_15"))) %>%
  # filter(Name != 'Mauritania') %>%
  group_by(FIDcalc) %>% 
  arrange(FIDcalc, datetime_start) %>%
  mutate(time_index = as.numeric(as.factor(datetime_start))) %>%
  rename(total_flood_dur_deseasoned_diff = total_flood_dur_deseasoned_ret) %>%
  pdata.frame(index = c('FIDcalc', 'time_index'))
  
panel_data <- as_tibble(data) %>%
  select(-contains(c("geom", "flood_1", "flood_29", "flood_15"))) %>%
  # filter(Name != 'Mauritania') %>%
  mutate(place = as.factor(paste(Name, ADMIN1, sep = "_")),
  		 place_index = as.numeric(place)) %>%
  group_by(place_index) %>% 
  arrange(place_index, datetime_start) %>%
  mutate(time_index = as.numeric(as.factor(datetime_start)),
  		 logIPC = log(mean_ipc)) %>%
  rename(total_flood_dur_deseasoned_diff = total_flood_dur_deseasoned_ret) %>%
  pdata.frame(index = c('place_index', 'time_index'))

##########################################  
# Testing for cross-sectional dependence #
##########################################
pcdtest(granger_data$mean_ipc, test = 'cd', na.action = na.omit)


##########################################
# Testing for panel unit roots ###########
##########################################
test_data <- panel_data %>% 
	group_by(FIDcalc) %>%
	nest() %>%
	split(rep(1:ceiling(6449/150), each = 150)[1:6449]) %>%
	lapply(as_tibble) %>%
	lapply(unnest, cols = c(data)) %>%
	lapply(function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
	
	
cips_results_ipc <- vector(mode = "list", length = length(test_data))
cips_results_logipc <- vector(mode = "list", length = length(test_data))
cips_results_ipc_diff <- vector(mode = "list", length = length(test_data))
cips_results_ipc_deseasoned <- vector(mode = "list", length = length(test_data))
cips_results_ipc_deseasoned_diff <- vector(mode = "list", length = length(test_data))

# ADMIN 3
for (i in 1:length(test_data)) {
	test_data_selected <- test_data[[i]] %>%
							filter(FIDcalc != 1062) # Only 7 seasons of data
	
	cips_results_ipc[[i]] <- cipstest(test_data_selected$mean_ipc, lags = 4)
	cips_results_logipc[[i]] <- cipstest(test_data_selected$logIPC, lags = 4)
	cips_results_ipc_diff[[i]] <- cipstest(test_data_selected$mean_ipc_diff, lags = 4)
	cips_results_ipc_deseasoned[[i]] <- cipstest(test_data_selected$mean_ipc_deseasoned, lags = 4)
	cips_results_ipc_deseasoned_diff[[i]] <- cipstest(test_data_selected$mean_ipc_deseasoned_diff, lags = 4)
}

## Comments on CIPS Test results
### For the first-differenced mean IPC series and the deseasoned first-differenced mean IPC series,
### the null hypothesis fails to reject in only one subset of the data, which is block 6, corresponding
### to 150 zones in Uganda. Investigating this data shows that there is just remarkably little movement
### in the IPC data for these zones, with deviations from 1 occurring usually only once per time series.
### I had to split the data into chunks to accommodate the program, which accepts a maximum panel size of 200.
### Additionally, the final statistic is an average of individual statistics calculated for each panel. This 
### suggests that were we able to average ALL of the test statistics for all 6449 panels in one test, we would
### almost certainly reject the null in favor of dataset-wide stationarity.
###
### Therefore, it seems that either the first-differenced or the deseasoned first-difference IPC time series
### are sufficiently stationary for modeling.

cips_results_floods <- vector(mode = "list", length = length(test_data))
cips_results_floods_diff <- vector(mode = "list", length = length(test_data))
cips_results_floods_deseasoned <- vector(mode = "list", length = length(test_data))
cips_results_floods_deseasoned_diff <- vector(mode = "list", length = length(test_data))

for (i in 1:length(test_data)) {
	test_data_selected <- test_data[[i]] %>%
							filter(FIDcalc != 1062) # Only 7 seasons of data
	
	cips_results_floods[[i]] <- cipstest(test_data_selected$total_flood_area_km2_std, lags = 4)
	cips_results_floods_diff[[i]] <- cipstest(test_data_selected$total_flood_area_diff, lags = 4)
	cips_results_floods_deseasoned[[i]] <- cipstest(test_data_selected$total_flood_area_deseasoned, lags = 4)
	cips_results_floods_deseasoned_diff[[i]] <- cipstest(test_data_selected$total_flood_area_deseasoned_diff, lags = 4)
}

### None of the flood transformations are stationary across the board, but the first-differenced and deseasoned
### first-differences are the best.

# ADMIN 1
test_data <- panel_data %>% 
	group_by(place_index) %>%
	nest() %>%
	split(rep(1:ceiling(273/150), each = 150)[1:273]) %>%
	lapply(as_tibble) %>%
	lapply(unnest, cols = c(data)) %>%
	lapply(function(df) {pdata.frame(df, index = c("place_index", "time_index"))})

for (i in 1:length(test_data)) {
	test_data_selected <- test_data[[i]]
	
	cips_results_ipc[[i]] <- cipstest(panel_data$mean_ipc, lags = 4, na.rm = T)
	cips_results_logipc[[i]] <- cipstest(panel_data$logIPC, lags = 4, na.rm = T)
	cips_results_ipc_diff[[i]] <- cipstest(panel_data$mean_ipc_diff, lags = 4, na.rm = T)
	cips_results_ipc_deseasoned[[i]] <- cipstest(panel_data$mean_ipc_deseasoned, lags = 4, na.rm = T)
	cips_results_ipc_deseasoned_diff[[i]] <- cipstest(panel_data$mean_ipc_deseasoned_diff, lags = 4, na.rm = T)
}

test_data <- panel_data %>%
	group_by(FIDcalc) %>%
	filter((sum(no_flood_events != 0, na.rm = T) >= 4) & (sum(mean_ipc_diff != 0, na.rm = T) >= 5)) %>%
	pdata.frame(index = c('FIDcalc', 'time_index'))

cipstest(granger_data$total_flood_dur_deseasoned_diff, lags = 4, na.rm = T)

### All IPC variables are stationary according to CIPS tests

cips_results_floods <- vector(mode = "list", length = length(test_data))
cips_results_floods_diff <- vector(mode = "list", length = length(test_data))
cips_results_floods_deseasoned <- vector(mode = "list", length = length(test_data))
cips_results_floods_deseasoned_diff <- vector(mode = "list", length = length(test_data))

for (i in 1:length(test_data)) {
	test_data_selected <- test_data[[i]]
	
	cips_results_floods[[i]] <- cipstest(test_data_selected$total_flood_area_std, lags = 4, na.rm = T)
	cips_results_floods_diff[[i]] <- cipstest(test_data_selected$total_flood_area_diff, lags = 4, na.rm = T)
	cips_results_floods_deseasoned[[i]] <- cipstest(test_data_selected$total_flood_area_deseasoned, lags = 4, na.rm = T)
	cips_results_floods_deseasoned_diff[[i]] <- cipstest(test_data_selected$total_flood_area_deseasoned_diff, lags = 4, na.rm =T)
}


#####################
# Granger causality #
#####################

# ADMIN 2
granger_data <- panel_data %>%
	group_by(FIDcalc) %>%
	filter((sum(no_flood_events != 0, na.rm = T) >= 4) & (sum(mean_ipc_diff != 0, na.rm = T) >= 5)) %>%
	pdata.frame(index = c('FIDcalc', 'time_index'))
	
# ADMIN 1
granger_data <- panel_data %>%
	group_by(place_index) %>%
	filter((sum(no_flood_events != 0, na.rm = T) >= 3) & (sum(mean_ipc_diff != 0, na.rm = T) >= 2)) %>%
	pdata.frame(index = c('place_index', 'time_index'))

	
granger_res_nfloods <- pgrangertest(mean_ipc_deseasoned_diff ~ no_floods_deseasoned_diff, data = granger_data, order = 4L)

granger_res_flood_dur <- pgrangertest(mean_ipc_deseasoned_diff ~ total_flood_dur_deseasoned_diff, data = granger_data, order = 4L)
	
granger_res_flood_area <- pgrangertest(mean_ipc_deseasoned_diff ~ total_flood_area_deseasoned_diff, data = granger_data, order = 4L)

# All null hypotheses are very confidently rejected, suggesting floods, as measured in various ways, do Granger cause changes in average IPC for a panel, at least heterogeneously

print(granger_res_nfloods)
print(granger_res_flood_dur)
print(granger_res_flood_area)

# Export Granger results for mapping

# ADMIN 3

granger_pvalues_nfloods <- granger_res_nfloods$indgranger %>%
					mutate(FIDcalc = as.numeric(as.character(FIDcalc))) %>%
					select(c(FIDcalc, `p-value`)) %>%
					rename(`p-value_nfloods` = `p-value`)

granger_pvalues_flood_dur <- granger_res_flood_dur$indgranger %>%
					mutate(FIDcalc = as.numeric(as.character(FIDcalc))) %>%
					select(c(FIDcalc, `p-value`)) %>%
					rename(`p-value_flood_dur` = `p-value`)
					
granger_pvalues_flood_area <- granger_res_flood_area$indgranger %>%
					mutate(FIDcalc = as.numeric(as.character(FIDcalc))) %>%
					select(c(FIDcalc, `p-value`)) %>%
					rename(`p-value_flood_area` = `p-value`)

granger_pvalues_geom <- data %>%
						mutate(time_index = as.numeric(as.factor(datetime_start)),
								FIDcalc = as.numeric(as.character(FIDcalc))) %>%
						left_join(granger_pvalues_nfloods, by = 'FIDcalc') %>%
						left_join(granger_pvalues_flood_dur, by = 'FIDcalc') %>%
						left_join(granger_pvalues_flood_area, by = 'FIDcalc') %>%
						filter(time_index == 16) %>%
						mutate(sig_nfloods = case_when(`p-value_nfloods` < 0.05 ~ 'Yes',
												`p-value_nfloods` >= 0.05 ~ 'No',
												is.na(`p-value_nfloods`) ~ 'Not enough information'),
								sig_flood_dur = case_when(`p-value_flood_dur` < 0.05 ~ 'Yes',
												`p-value_flood_dur` >= 0.05 ~ 'No',
												is.na(`p-value_flood_dur`) ~ 'Not enough information'),
								sig_flood_area = case_when(`p-value_flood_area` < 0.05 ~ 'Yes',
												`p-value_flood_area` >= 0.05 ~ 'No',
												is.na(`p-value_flood_area`) ~ 'Not enough information'),) %>%
						select(c(Name, ADMIN1, ADMIN2, FIDcalc, `p-value_nfloods`, `p-value_flood_dur`,
								`p-value_flood_area`, sig_nfloods, sig_flood_dur, sig_flood_area, geom))
								
granger_outfile <- "../../Data/granger_results_test_adm3_geom.gpkg"
st_write(granger_pvalues_geom, granger_outfile)

# ADMIN 1

granger_pvalues_nfloods <- granger_res_nfloods$indgranger %>%
					mutate(place_index = as.numeric(as.character(place_index))) %>%
					select(c(place_index, `p-value`)) %>%
					rename(`p-value_nfloods` = `p-value`)

granger_pvalues_flood_dur <- granger_res_flood_dur$indgranger %>%
					mutate(place_index = as.numeric(as.character(place_index))) %>%
					select(c(place_index, `p-value`)) %>%
					rename(`p-value_flood_dur` = `p-value`)
					
granger_pvalues_flood_area <- granger_res_flood_area$indgranger %>%
					mutate(place_index = as.numeric(as.character(place_index))) %>%
					select(c(place_index, `p-value`)) %>%
					rename(`p-value_flood_area` = `p-value`)

granger_pvalues_geom <- data %>%
						mutate(time_index = as.numeric(as.factor(datetime_start)),
								place_index = as.numeric(as.character(place_index))) %>%
						left_join(granger_pvalues_nfloods, by = 'place_index') %>%
						left_join(granger_pvalues_flood_dur, by = 'place_index') %>%
						left_join(granger_pvalues_flood_area, by = 'place_index') %>%
						filter(time_index == 16) %>%
						mutate(sig_nfloods = case_when(`p-value_nfloods` < 0.05 ~ 'Yes',
												`p-value_nfloods` >= 0.05 ~ 'No',
												is.na(`p-value_nfloods`) ~ 'Not enough information'),
								sig_flood_dur = case_when(`p-value_flood_dur` < 0.05 ~ 'Yes',
												`p-value_flood_dur` >= 0.05 ~ 'No',
												is.na(`p-value_flood_dur`) ~ 'Not enough information'),
								sig_flood_area = case_when(`p-value_flood_area` < 0.05 ~ 'Yes',
												`p-value_flood_area` >= 0.05 ~ 'No',
												is.na(`p-value_flood_area`) ~ 'Not enough information'),) %>%
						select(c(Name, ADMIN1, place_index, `p-value_nfloods`, `p-value_flood_dur`,
								`p-value_flood_area`, sig_nfloods, sig_flood_dur, sig_flood_area, geom))
								
granger_outfile <- "../../Data/granger_results_deseasoned_diff_adm1_geom.gpkg"
st_write(granger_pvalues_geom, granger_outfile)