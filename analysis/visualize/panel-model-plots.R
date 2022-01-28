library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(plm)
library(dotwhisker)
library(latex2exp)
library(RColorBrewer)
library(purrr)
library(data.table)

data_dir <- "../../Data/"

adm2_file <- paste(data_dir, "flood_ipc_seasonal_adm2_timeseries.gpkg", sep = "")
granger_adm2_file <- paste(data_dir, "flood_ipc_seasonal_adm2_timeseries.gpkg", sep = "")
data_adm2 <- st_read(adm2_file)

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
	
.formula <- "mean_ipc_diff ~ plm::lag(no_floods_diff, 0:4) +
  plm::lag(total_flood_area_diff, 0:4) + 
  plm::lag(total_flood_dur_diff, 0:4)"
 
regional_dfs_adm2 <- panel_data_adm2 %>%
	group_by(FIDcalc) %>%
	ungroup() %>%
	mutate(group = case_when(Name %in% c('Burkina Faso', 'Mali', 'Niger', 'Nigeria', 'Chad', 'Mauritania') ~ 'West Africa + Chad',
							 Name %in% c('Ethiopia', 'Kenya', 'Somalia', 'Sudan', 'South Sudan', 'Uganda') ~ 'East Africa',
							 Name %in% c('Mozambique', 'Malawi', 'Zimbabwe', 'Zambia') ~ 'South Africa')) %>%
	group_by(group) %>%
	group_split()

region_names <- sort(c('West Africa + Chad', 'East Africa', 'South Africa'))
regional_dfs_adm2 <- lapply(regional_dfs_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(regional_dfs_adm2) <- region_names

regional_dfs_granger_adm2 <- granger_data_adm2 %>%
  group_by(FIDcalc) %>%
  ungroup() %>%
  mutate(group = case_when(Name %in% c('Burkina Faso', 'Mali', 'Niger', 'Nigeria', 'Chad', 'Mauritania') ~ 'West Africa + Chad',
                           Name %in% c('Ethiopia', 'Kenya', 'Somalia', 'Sudan', 'South Sudan', 'Uganda') ~ 'East Africa',
                           Name %in% c('Mozambique', 'Malawi', 'Zimbabwe', 'Zambia') ~ 'South Africa')) %>%
  group_by(group) %>%
  group_split()

regional_dfs_granger_adm2 <- lapply(regional_dfs_granger_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(regional_dfs_granger_adm2) <- region_names

country_dfs_adm2 <- panel_data_adm2 %>%
	group_by(Name) %>%
	group_split()
	
country_dfs_adm2 <- lapply(country_dfs_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(country_dfs_adm2) <- sort(unique(panel_data_adm2$Name))

country_dfs_granger_adm2 <- granger_data_adm2 %>%
  group_by(Name) %>%
  group_split()

country_dfs_granger_adm2 <- lapply(country_dfs_granger_adm2, function(df) {pdata.frame(df, index = c("FIDcalc", "time_index"))})
names(country_dfs_granger_adm2) <- sort(unique(granger_data_adm2$Name))


################################################################################

make_model_tidy <- function(df, .formula, name, dataset_type) {
  tryCatch({
    .model <- plm(.formula, data = df, model = "random", random.method = "walhus")
    .summary <- summary(.model, vcov=function(x) vcovSCC(x, type = 'HC1', maxlag=4))
    summary_tb <- as_tibble(.summary$coefficients)
    summary_tb$term <- rownames(.summary$coefficients)
    colnames(summary_tb) <- c('estimate', 'std.error', 'statistic', 'p.value', 'term')
    
    summary_tb <- relabel_predictors(summary_tb,
                                     c(`plm::lag(no_floods_diff, 0:4)0` = "Num. floods (t)",
                                       `plm::lag(no_floods_diff, 0:4)1` = "Num. floods (t-1)",
                                       `plm::lag(no_floods_diff, 0:4)2` = "Num. floods (t-2)",
                                       `plm::lag(no_floods_diff, 0:4)3` = "Num. floods (t-3)",
                                       `plm::lag(no_floods_diff, 0:4)4` = "Num. floods (t-4)",
                                       `plm::lag(total_flood_area_diff, 0:4)0` = "Total area (t)",
                                       `plm::lag(total_flood_area_diff, 0:4)1` = "Total area (t-1)",
                                       `plm::lag(total_flood_area_diff, 0:4)2` = "Total area (t-2)",
                                       `plm::lag(total_flood_area_diff, 0:4)3` = "Total area (t-3)",
                                       `plm::lag(total_flood_area_diff, 0:4)4` = "Total area (t-4)",
                                       `plm::lag(total_flood_dur_diff, 0:4)0` = "Total duration (t)",
                                       `plm::lag(total_flood_dur_diff, 0:4)1` = "Total duration (t-1)",
                                       `plm::lag(total_flood_dur_diff, 0:4)2` = "Total duration (t-2)",
                                       `plm::lag(total_flood_dur_diff, 0:4)3` = "Total duration (t-3)",
                                       `plm::lag(total_flood_dur_diff, 0:4)4` = "Total duration (t-4)"))
    
    summary_tb <- summary_tb %>% mutate(model = name,
                                        dataset_type = dataset_type,
                                        category = case_when(grepl('Num. floods', term) ~ "Flood occurrences",
                                                             grepl("Total area", term) ~ "Area affected",
                                                             grepl("Total duration", term) ~ "Cumulative duration"
                                                             ),
                                        shape = case_when(grepl('Full', dataset_type) ~ 19,
                                                          grepl('Granger', dataset_type) ~ 17))
    
    # summary_tb$shape <- as.factor(summary_tb$shape)
    
    summary_tb <- relabel_predictors(summary_tb,
                                     "Num. floods (t)" = "t",
                                     "Num. floods (t-1)" = "t-1",
                                     "Num. floods (t-2)" = "t-2",
                                     "Num. floods (t-3)" = "t-3",
                                     "Num. floods (t-4)" = "t-4",
                                     "Total area (t)" = "t",
                                     "Total area (t-1)" = "t-1",
                                     "Total area (t-2)" = "t-2",
                                     "Total area (t-3)" = "t-3",
                                     "Total area (t-4)" = "t-4",
                                     "Total duration (t)" = "t",
                                     "Total duration (t-1)" = "t-1",
                                     "Total duration (t-2)" = "t-2",
                                     "Total duration (t-3)" = "t-3",
                                     "Total duration (t-4)" = "t-4",
                                     ) %>%
      filter(!is.na(category))
    
    
    return(summary_tb)
  },
    error = function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}

make_plot <- function(summary_tibble, name, sig_only=FALSE, facet_by_granger=FALSE){
  
  color_palette <- brewer.pal(n = 7, 'Dark2')
  
  summary_tibble$name_val = as.numeric(as.factor(summary_tibble$model))
  
  summary_tibble <- summary_tibble %>%
                    rowwise() %>%
                    mutate(model_color = case_when(model %in% c('East Africa', 'South Africa', 'West Africa + Chad', 'All-Africa Full') ~ "#000000",
                                                   model %in% 'All-Africa Granger' ~ "#778899",
                                                   TRUE ~ ifelse(name_val <= length(color_palette), 
                                                                  color_palette[name_val],
                                                                  color_palette[name_val %% length(color_palette)])))
  
  
  if (sig_only) {
    summary_tibble_plot <- summary_tibble
    summary_tibble_plot[(summary_tibble_plot$`p.value` >= .05), c('estimate', 'std.error')] <- NA
    summary_tibble_plot <- summary_tibble_plot %>%
      mutate(fdr_sig = case_when(p.value >= p_fdr ~ F,
                                 T ~ T) )
  }
  else {
    summary_tibble_plot <- summary_tibble
  }
  shape_col = 'dataset_type'
  whisker_plot <- dwplot(summary_tibble_plot,
                         dot_args = list(aes(color = model_color, shape = dataset_type)),
                         whisker_args = list(aes(color = model_color, linetype=fdr_sig)),
                         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
    {if (facet_by_granger) {
      list(scale_color_identity(guide = "legend", labels = c(unique(summary_tibble_plot[order(summary_tibble_plot$model_color), ]$model))),
           scale_shape_manual(values = c(19, 17), guide = FALSE),
           scale_linetype_manual(values = c('dashed','solid'), guide = FALSE))
    }
    else {
      list(scale_color_identity(guide = FALSE),scale_shape_manual(values = c(19, 17)),
           scale_linetype_manual(values = c('dashed','solid'), guide = FALSE))
    }} +
    theme_bw() +
    theme(#legend.justification=c(.98, .007),
          #legend.position=c(.98, .01),
          legend.title = element_blank(),
          legend.background = element_rect(color="gray90")) +
    xlab("Coefficient Estimate") +
    ylab("") +
    ggtitle(name) +
    if (facet_by_granger){
      facet_grid(rows=vars(factor(category, levels = c("Flood occurrences", "Area affected", "Cumulative duration"))), 
                 cols=vars(dataset_type), scales = "free_y", space = "free_y", drop=TRUE, switch = "y")
    }
    else {
      facet_grid(rows=vars(factor(category, levels = c("Flood occurrences", "Area affected", "Cumulative duration"))), scales = "free_y", space = "free_y", drop=TRUE, switch = "y")
    }
    
  #return(summary_tibble_plot)
  return(whisker_plot)
}

get_p_fdr <- function(model_table, alpha_fdr){
  p_fdr <- model_table %>%
    arrange(p.value) %>%
    mutate(p_index = row_number(),
           bool_fdr = (`p.value` <= (p_index / dim(.)[1]) * alpha_fdr)) %>%
    filter(bool_fdr == TRUE)
  
  return(max(p_fdr$p.value))
}

################################################################################

all_africa_tidy <- make_model_tidy(panel_data_adm2, .formula, "All-Africa Full", "Full data")
all_africa_granger <- make_model_tidy(granger_data_adm2, .formula, "All-Africa Granger", "Granger data")

plot_1_tidy <- data.frame(rbind(all_africa_tidy, all_africa_granger))

all_africa_p_fdr <- get_p_fdr(plot_1_tidy, 0.10)
plot_1_tidy$p_fdr <- .05

plot_1 <- make_plot(plot_1_tidy, "All-Africa Random Effects Model: Full v. Granger-filtered data", sig_only=T, facet_by_granger = F)
# plot_1_fdr <- make_plot(plot_1_tidy, "All-Africa Random Effects Model: Full v. Granger-filtered data", sig_only=T, facet_by_granger = F)

regional_tidies <- lapply(seq_along(regional_dfs_adm2), function(i) make_model_tidy(regional_dfs_adm2[[i]], .formula, names(regional_dfs_adm2)[[i]], dataset_type = "Full data"))
names(regional_tidies) <- region_names

regional_granger_tidies <- lapply(seq_along(regional_dfs_granger_adm2), function(i) make_model_tidy(regional_dfs_granger_adm2[[i]], .formula, names(regional_dfs_granger_adm2)[[i]], dataset_type = "Granger data"))
names(regional_granger_tidies) <- region_names

country_tidies <- lapply(seq_along(country_dfs_adm2), function(i) make_model_tidy(country_dfs_adm2[[i]], .formula, names(country_dfs_adm2)[[i]], dataset_type = 'Full data'))
names(country_tidies) <- names(country_dfs_adm2)

country_granger_tidies <- lapply(seq_along(country_dfs_granger_adm2), function(i) make_model_tidy(country_dfs_granger_adm2[[i]], .formula, names(country_dfs_granger_adm2)[[i]], dataset_type = "Granger data"))
names(country_granger_tidies) <- names(country_dfs_granger_adm2)



sub_p_fdr <- get_p_fdr(data.frame(rbind(purrr::map_df(list(regional_tidies, regional_granger_tidies,
                                                   country_tidies, country_granger_tidies),
                                                   rbindlist))), 
                       0.10)

plot_2_tidy <- data.frame(rbind(regional_tidies[["East Africa"]],
                     country_tidies[["Ethiopia"]],
                     country_tidies[["Kenya"]],
                     country_tidies[["Somalia"]],
                     country_tidies[["Sudan"]],
                     country_tidies[["South Sudan"]],
                     country_tidies[["Uganda"]],
                     regional_granger_tidies[["East Africa"]],
                     #country_granger_tidies[["Ethiopia"]],
                     country_granger_tidies[["Kenya"]],
                     #country_granger_tidies[["Somalia"]],
                     country_granger_tidies[["Sudan"]],
                     country_granger_tidies[["South Sudan"]],
                     country_granger_tidies[["Uganda"]]))

plot_2_tidy$p_fdr <- sub_p_fdr

plot_2 <- make_plot(plot_2_tidy, "East Africa Random Effects Models", sig_only=T, facet_by_granger = T)

plot_3a_tidy <- data.frame(rbind(regional_tidies[["West Africa + Chad"]],
                     country_tidies[["Niger"]],
                     country_tidies[["Nigeria"]],
                     country_tidies[["Mali"]],
                     country_tidies[["Burkina Faso"]],
                     country_tidies[["Chad"]],
                     regional_granger_tidies[["West Africa + Chad"]],
                     country_granger_tidies[["Niger"]],
                     country_granger_tidies[["Nigeria"]],
                     country_granger_tidies[["Mali"]],
                     country_granger_tidies[["Burkina Faso"]],
                     country_granger_tidies[["Chad"]]))

plot_3a_tidy$p_fdr <- sub_p_fdr

plot_3a <- make_plot(plot_3a_tidy, "West Africa + Chad Random Effects Models", sig_only=T, facet_by_granger = T)

plot_4a_tidy <- rbind(regional_tidies[["South Africa"]],
                     country_tidies[["Mozambique"]],
                     country_tidies[["Malawi"]],
                     country_tidies[["Zambia"]],
                     country_tidies[["Zimbabwe"]],
                     regional_granger_tidies[["South Africa"]],
                     country_granger_tidies[["Mozambique"]],
                     country_granger_tidies[["Malawi"]],
                     country_granger_tidies[["Zambia"]],
                     country_granger_tidies[["Zimbabwe"]])

plot_4a_tidy$p_fdr <- sub_p_fdr
plot_4a <- make_plot(plot_4a_tidy, "South Africa Random Effects Models", sig_only=T, facet_by_granger = T)

plot_4b_tidy <- rbind(regional_tidies[["Southeast Africa"]],
                      country_tidies[["Mozambique"]],
                      country_tidies[["Malawi"]])

plot_4b <- make_plot(plot_4b_tidy, "South Africa Random Effects Models", sig_only=T, facet_by_granger = F)

