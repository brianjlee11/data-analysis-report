library("dplyr")
library("tidyr")
library("wbstats")
library("ggplot2")
library("stringr")
library("maps")
library("mapproj")
library("scales")

##############################################
#
# Load in all data
#
##############################################
options(scipen = 999)

# Load in corruption data
file_location <- "clean_data/CPI_"
unique_years <- c(2000:2011, "2012_2019")
files <- rev(paste0(file_location, unique_years, ".csv"))

corruption_df <- read.csv(files[1], stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>% 
  select(-contains("X."))
for (file in files[-1]) {
  temp_file <-read.csv(file, stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  corruption_df <- corruption_df %>% 
    left_join(temp_file)
}

corruption_cpi_df <- corruption_df %>% 
  select(country, ISO3, Region, contains("cpi")) %>% 
  mutate(cpi_2005 = as.numeric(cpi_2005))

# scale up the years 2000 - 2011
times_ten <- paste0("cpi_", 2011:2000)
corruption_cpi_df[, times_ten] <- corruption_cpi_df[, times_ten] * 10
colnames(corruption_cpi_df) <- gsub("cpi_", "", colnames(corruption_cpi_df))

# Load in press_freedom data
raw_press_freedom_index <- read.csv("clean_data/press_freedom_index.csv", stringsAsFactors = FALSE)

# Load in wb data
gdp_indic <- "NY.GDP.PCAP.CD"
updated_cache <- wbcache()
gdp_per_capita <- wb(country="countries_only", indicator = gdp_indic, mrv = 20, cache = updated_cache)
gdp_per_capita_wide <- gdp_per_capita %>% 
  spread(
    key = date, 
    value = value
  )

# helper function to make bins for choropleth data 
bin_it <- function(data_vector, bin_size = 10) {
  bins <- seq(0, 100, by = bin_size)
  if (bins[length(bins)] != 100) {
    bins <- c(bins, 100)
  }
  labels <- paste0(bins, " to ", bins[-1])
  labels <- labels[-length(labels)]
  return(cut(data_vector, breaks = bins, labels = labels))
}


###################################################
#
# Data Extract and Analysis for Press Freedom Index
#
###################################################

# select columns we want to use
press_freedom_index <-  raw_press_freedom_index %>% 
  filter(Indicator == "Press Freedom Index") %>%
  select(-Subindicator.Type)

# create a sample dataframe to display
years_to_filter <- paste0("X",c(2001:2009, 2012))
sample_press_freedom_data <- press_freedom_index %>% top_n(5, X2019) %>% 
  select(-years_to_filter)

press_freedom_long <- press_freedom_index %>% 
  gather(key = date,
         value = pf_index,
         -Country.ISO3,
         -Country.Name,
         -Indicator.Id,
         -Indicator) %>% 
  filter(pf_index < 100)

# rename a couple of columns and clean the year column
colnames(press_freedom_long)[1] <-"iso3c"
colnames(press_freedom_long)[5] <-"year"
press_freedom_long$year <- substr(press_freedom_long$year, 2, 5)

# Create central tendency data by year alone
central_tend_for_press_freedom <- press_freedom_long %>% 
  group_by(year) %>% 
  summarize(average = mean(pf_index, na.rm = TRUE), 
            median = median(pf_index, na.rm = TRUE))

# create central tendency data by year and country
central_tend_for_press_freedom_by_countries <- press_freedom_long %>%
  group_by(iso3c, Country.Name) %>% 
  summarize(average = mean(pf_index, na.rm = TRUE),
            median = median(pf_index, na.rm = TRUE),
            maximum = max(pf_index, na.rm = TRUE),
            minimum = min(pf_index, na.rm = TRUE))

# create a histogram of press-freedom averages
press_freedom_histo_plot_average <-central_tend_for_press_freedom_by_countries %>%
  filter(average <= 100 & average >= 0) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = average), binwidth = 10, fill = "RED", color = "BLACK") +
  labs(
    title = "Average Press Freedom Index for Each Country",
    x = "Average Press Freedom Index", 
    y = "Number of Countries"
  ) 

# create a box-plot from press_freedom_long
press_freedom_plot_median <- ggplot(data = press_freedom_long) +
  geom_boxplot(mapping = aes(x = year, y = pf_index, fill = "ORANGE")) +
  labs(
    title = "Median Press Freedom Index for Each Country",
    x = "Year", 
    y = "Press Freedom Index",
    fill = "Press Freedom Index"
  ) + scale_fill_manual(labels = "Median PFI", values = "ORANGE")

# add bins to our central_tend_for_press_by_countries
central_tend_for_press_freedom_by_countries$bin <- bin_it(central_tend_for_press_freedom_by_countries$average)

# get data for our choropleth map
polygons <- map_data("world")
iso <- iso.alpha(polygons[["region"]], n = 3)
polygons <- polygons %>% mutate(iso3c = iso)
central_tend_w_map <- left_join(central_tend_for_press_freedom_by_countries, polygons, "iso3c")

# Create choropleth plot
press_free_world_plot <- ggplot(data = central_tend_w_map,
                                mapping = aes(x = long, y = lat, group = group, fill = bin),) +
  geom_polygon() +
  coord_quickmap() +
  theme_void() + 
  ggtitle("Average Press Freedom Index Around the World 2001 - 2019") +
  theme(title = element_text(size = 10, hjust = 0.5)) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(fill = "Press Freedom Index")

# Create line plot of press_freedom_long averages grouped by year
press_freedom_line_plot <- press_freedom_long %>%  
  group_by(year) %>% 
  summarize(average = mean(pf_index, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = average, group = 1)) +
  geom_smooth(mapping = aes(x = year, y = average, group = 1), se = FALSE, method = "loess", formula = y ~ x) +
  ggtitle("Average Press Freedom Index Worldwide from 2001 - 2019") +
  theme(
    title = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(size = 7)
  ) 

argentina_press_freedom_score <- press_freedom_index %>% filter(Country.Name == "Argentina") %>% 
  pull(X2003)
kosovo_press_freedom_score <- press_freedom_index %>% filter(Country.Name == "Kosovo") %>% 
  pull(X2015)

############################################
#
# Data Extract for Corruption Data
#
############################################
# sample our corruption_cpi_df to show our data
corruption_sample_df <- corruption_cpi_df[1:5, 1:7]

# Make our data long
long_corruption <- corruption_cpi_df %>% 
  gather(
    key = year,
    value = CPI_score,
    -country,
    -ISO3,
    -Region
  )

# summarize our data using means, number of countries used and extrema
# grouped by year
corruption_summary <- long_corruption %>%  
  group_by(year) %>% 
  summarize(global_mean = mean(CPI_score, na.rm = T),
            worst_score =  min(CPI_score, na.rm = T),
            best_score = max(CPI_score, na.rm = T), 
            num_countries = sum(!is.na(CPI_score))) %>% 
  t()

# number of missing rows 
hole_plot <- long_corruption %>% 
  group_by(year) %>% 
  summarize(holes = sum(is.na(CPI_score))) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = holes), group = 1, color = "red") +
  geom_point(mapping = aes(x = year, y = holes), color = "blue") +
  labs(title = "Number of Countries Without Sufficient Sources", x ="Year", y = "Number of Missing Countries")

# make the year row our column headers
colnames(corruption_summary) <- corruption_summary[1,]
corruption_summary <- corruption_summary[-1,]

# Histogram of countries in 2019
corruption_histogram <- long_corruption %>% 
  filter(year == "2019") %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = CPI_score),color = "grey", bins = 10)+
  geom_vline(mapping = aes(xintercept = mean(CPI_score)), color = "red", linetype="dashed") + 
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "CPI Score 2019", x ="CPI Score", y = "Number of Countries")

# Regional Changes
regional_changes <- long_corruption %>% 
  group_by(Region, year) %>% 
  summarize(mean = mean(CPI_score, na.rm = T)) %>% 
  group_by(Region) %>% 
  ggplot() +
  geom_point(mapping = aes(x = year, y = mean, color = Region)) + 
  geom_line(mapping = aes(x = year, y = mean, group = Region, color = Region)) + 
  labs(title = "Corruption Changes by Region", x ="Year", y = "Corruption Perception Index Score") +
  theme(axis.text.x = element_text(size = 6, angle = 90))

# Country scores in 2019
world_map <- map_data("world")
world_map$ISO3 <- iso.alpha(world_map$region, n = 3)
world_corruption <- left_join(world_map, long_corruption) %>% 
  filter(year == 2019)
world_corruption$bins <- bin_it(world_corruption$CPI_score, 20)
country_scores_2019 <- world_corruption %>% 
  ggplot() +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = bins))+
  coord_quickmap() +
  scale_fill_brewer(palette = "RdYlGn") +
  theme(axis.text= element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
  labs(title = "CPI Score 2019")

###################################
#
# Data Extract for GDP per capita
#
###################################

GDP_mean_df <- gdp_per_capita %>% 
  group_by(iso3c, country) %>% 
  summarise(mean = mean(value))

# Data summary top five
summary_top_5 <- gdp_per_capita %>% 
  filter(date == 2018) %>% 
  arrange(-value) %>%  
  head(5) %>% 
  pull(iso3c)

# data summary bottom five
summary_bottom_5 <- gdp_per_capita %>% 
  filter(date == 2018) %>% 
  arrange(-value) %>% 
  tail(5) %>% 
  pull(iso3c)

summary_df <- c(summary_top_5,summary_bottom_5) 
summary_df <- gdp_per_capita %>% 
  filter(iso3c %in% summary_df) %>% 
  filter(date == 2018) %>% 
  arrange(-value) %>% 
  select(iso3c, date, value, indicatorID, indicator, iso2c, country)

# plot top ten and bottom ten GDP countries together
top_10_GDP_countries <- GDP_mean_df %>%
  arrange(-mean) %>% 
  head(10) 
bottom_10_GDP_countries <- GDP_mean_df %>%
  arrange(-mean) %>% 
  tail(10) 

top_bottom_10_GDP_plot <- rbind(top_10_GDP_countries, bottom_10_GDP_countries) %>% 
  ggplot() +
  geom_col(mapping= aes(x=reorder(iso3c,mean),y=mean))+
  labs(title ="Top and Bottom 10 Countries by Average GDP per Capita in 1999-2018",
       x = "Country",
       y = "GDP per Capita")+
  theme(axis.text.x = element_text(size = 7 , angle = 90))

# create histogram of country's average GDP_pc's
GDP_histo_plot <- ggplot(data = GDP_mean_df)+
  geom_histogram(mapping = aes(x = mean), bins = 30)+
  labs(title = "Average GDP per Capita 1999-2018 (per country)",
       x = "Average GDP per Capita",
       y = "Count") +
  geom_vline(mapping = aes(xintercept = mean(mean)), color = "purple",linetype = "dashed")+
  geom_vline(mapping = aes(xintercept = median(mean)), color = "red",linetype = "dashed")

# create a boxplot of global gdp_pc's for each year
GDP_box_plot <- gdp_per_capita %>% 
  ggplot() +
  geom_boxplot(mapping = aes(x = date, y = value))+
  labs(title = "GDP per Capita by Year (1999-2018)",
       x = "Year",
       y = "Value of GDP per Capita")+
  theme(axis.text.x = element_text(size = 7 , angle = 90))

# create a world map of gdp_pc in 2018
countries_map <- map_data("world") %>% 
  mutate(iso3c = iso.alpha(region, n=3))
choropleth_map <- left_join(countries_map, gdp_per_capita, by = "iso3c")

GDP_map <- choropleth_map %>% 
  filter(date == 2018) %>% 
  ggplot() +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = value))+
  scale_fill_distiller(palette = "RdYlGn", direction = +1) +
  labs(title = "World Average GDP per Capita 1999-2018")+
  coord_quickmap()+
  theme_void()

#########################################
#
# Q1: Global Corruption Trends
#
#########################################

#Collect summary statistics from the overall data
global_corruption <- long_corruption %>% 
  group_by(year) %>% 
  summarize(mean_corruption = mean(CPI_score, na.rm = T),
            median_corruption = median(CPI_score, na.rm = T),
            std_corruption = sd(CPI_score, na.rm = T),
            max_corruption = max(CPI_score, na.rm = T),
            min_corruption = min(CPI_score, na.rm = T))

#generate years for change in mean
years <- (2001:2019)
#generate the change in mean corruption
meanCorr <- global_corruption %>% 
  pull(mean_corruption)
delta <- meanCorr[19:1]-meanCorr[20:2]
delta_df <- data.frame("years" = years, "deltaMean" = delta)

#Plot the change in corruption per year
dCorruption <- ggplot(data=delta_df, aes(x=years, y=deltaMean, group=2)) +
  geom_line(color = "blue")+
  geom_point(color = "blue")

#Plot the average and standard deviation of corruption per year
meanCorruption <- ggplot(data=global_corruption, aes(x=year, y=mean_corruption, group=1)) +
  geom_line(color = "red")+
  geom_point(color = "red") +
  geom_smooth(method='lm') + 
  geom_errorbar(aes(ymin=mean_corruption-std_corruption, ymax=mean_corruption+std_corruption), width=.2, color = "red")

#####################################################################################
# Question 4: Is there correlation with freedom of press and concentration of wealth?
#####################################################################################

colnames(gdp_per_capita)[2] <-"year"
colnames(gdp_per_capita)[3] <-"gdp"
gdp_w_pfi <- left_join(press_freedom_long, gdp_per_capita, c("iso3c", "year", "Country.Name" = "country")) %>% 
  rename(country = Country.Name) %>% filter(!is.na(gdp))

sum_gdp_w_pfi <- gdp_w_pfi %>% group_by(iso3c, country) %>% 
  summarize(avg_pf = mean(pf_index, na.rm = TRUE), avg_gdp = mean(gdp, na.rm = TRUE))

top_20_sum_gdp_pfi <- head(sum_gdp_w_pfi, 20, iso3c)

gdp_pfi_plot <- ggplot(data = gdp_w_pfi, mapping = aes(x = pf_index, y = gdp)) + 
  geom_point() + geom_smooth(method = "loess", se = FALSE, formula = y ~ x, na.rm = TRUE, color = "RED") + 
  ggtitle("Press Freedom Index and GDP per Capita") +
  labs(
    x = "Press Freedom Index",
    y = "GDP per Capita ($)"
  ) + 
  theme(
    plot.title = element_text(size = 10, hjust = 0.5)
  ) + scale_y_continuous(labels = comma)

cor_test_gdp_pfi <- 
  cor.test(gdp_w_pfi$pf_index, gdp_w_pfi$gdp, use = "complete.obs")
cor_t_value <- round(unname(cor_test_gdp_pfi$statistic), 2)
cor_p_value <- round(cor_test_gdp_pfi$p.value, 4)
cor_r_value <- round(unname(cor_test_gdp_pfi$estimate), 2)
cor_df <- trunc(unname(cor_test_gdp_pfi$parameter))
cor_table <- data.frame(c(cor_t_value, cor_p_value, cor_r_value, cor_df), row.names = c("t Value", "p Value", "Correlation Coefficient", "Degree of Freedom")) %>% 
  rename("Correlation Test" = "c.cor_t_value..cor_p_value..cor_r_value..cor_df.")

highest_gdp <- gdp_w_pfi %>% filter(gdp == max(gdp)) 
high_gdp <- highest_gdp %>% pull(gdp) %>% round(2)
high_gdp_pfi <- highest_gdp %>% pull(pf_index)
high_gdp_country <- highest_gdp %>% pull(country)

highest_pfi <- gdp_w_pfi %>% filter(pf_index == max(pf_index)) 
high_pfi_gdp <- highest_pfi %>% pull(gdp) %>% round(2)
high_pfi <- highest_pfi %>% pull(pf_index)
high_pfi_country <- highest_pfi %>% pull(country)


#############################
# Section 5
#############################
# country changes from beginning of data to end of data
# what happens to the distribution of wealth in the countries that experience the greatest change in the distribution of wealth?
cpi_change <- corruption_cpi_df %>% 
  filter(!is.na(`2011`) & !is.na(`2016`)) %>% 
  mutate(change_over_time = `2014` - `2019`) %>% 
  select(country, ISO3, Region, change_over_time) %>% 
  gather(
    key = year,
    value = change_over_time,
    -country,
    -ISO3,
    -Region
  ) %>% 
  arrange(change_over_time)

five_year_change <- ggplot(cpi_change) + 
  geom_col(mapping = aes(x = reorder(ISO3, change_over_time), y = change_over_time))+ 
  labs(title = "Corruption Perception Index Score Changes", x ="Country (ISO3)", y = "Corruption Perception Index Score Delta") +
  theme(axis.text.x = element_text(size = 6, angle = 90))


# Making the epic plot
rising_scores <- cpi_change %>% 
  filter(change_over_time > 0) %>%
  pull(ISO3)
falling_scores <- cpi_change %>% 
  filter(change_over_time < 0) %>%
  pull(ISO3) %>%  head(30)
filtered_long_cpi <- long_corruption %>% 
  filter(ISO3 %in% c(rising_scores, falling_scores))

# replace all na with 0
corruption_cpi_df_0 <- corruption_cpi_df
colnames(corruption_cpi_df_0) <- sub("cpi_", "", colnames(corruption_cpi_df_0))
# helper function that takes in a dataframe with numeric variables
# outputs a dataframe that contains the difference of columns in those variables
col_diff <- function(dataframe, headers, rev = F){
  new_df <- dataframe %>% 
    select(headers)
  cols <- colnames(dataframe)
  cols <- cols[! cols %in% headers]
  # pattern of b - a
  col_a <- cols[-length(cols)]
  col_b <- cols[-1]
  if (rev) {
    col_temp <- col_b
    col_b <- col_a
    col_a <- col_temp
  }
  new_headers <- paste(col_b, col_a, sep = "-")
  new_df[new_headers] <- dataframe[col_b] - dataframe[col_a]
  return(new_df)
}

swing <- function(num) {
  num_greater <- num > 0
  new_num <- num
  new_num[num_greater] <- "Positive Change"
  num_greater <- num < 0
  new_num[num_greater] <- "Negative Change"
  num_greater <- num == 0
  new_num[num_greater] <- "No Change"
  return(new_num)
}

# get the columns that contain differences
difference_cpi <- col_diff(corruption_cpi_df, c("country", "ISO3", "Region"))
difference_wb <- col_diff(gdp_per_capita_wide, c("iso3c", "iso2c", "country", "indicatorID", "indicator"), rev = T) %>% 
  select(-iso2c)

# count number of increases, decreases, and do-nothings (excluding n/a) for both dfs\
swing_cpi <- difference_cpi %>% 
  gather(key = year_range, value = cpi_swing, -country, -ISO3, -Region) %>% 
  filter(!is.na(cpi_swing)) %>% 
  mutate(year_range = gsub("cpi_", "", year_range), swing = swing(cpi_swing))

swing_wb <- difference_wb %>% 
  gather(key = year_range, value = wb_swing, -country, -iso3c) %>% 
  mutate(swing = swing(wb_swing)) 

pos_cpi <- swing_cpi %>% 
  filter(swing == "Positive Change")
neg_cpi<- swing_cpi %>% 
  filter(swing == "Negative Change")
pos_wb<- swing_wb %>% 
  filter(swing == "Positive Change")
neg_wb<- swing_wb %>% 
  filter(swing == "Negative Change")

# filter for positive cpi and positive wb, join based on matches and count
pos_pos <- inner_join(pos_cpi, pos_wb, by = c("country", "year_range")) %>% 
  group_by(year_range) %>% 
  mutate(status = "CPI Rise, GDP Rise") %>%
  select(country, ISO3, year_range, status)
# gather(key = key, value = value, -year_range, -status)
# filter for negative cpi and positive wb, join based on matches and count
pos_neg <- inner_join(neg_cpi, pos_wb, by = c("country", "year_range")) %>% 
  group_by(year_range) %>% 
  mutate(status = "CPI Fall, GDP Rise") %>% 
  select(country, ISO3, year_range, status)
# filter for positive cpi and negative wb, join based on matches and count
neg_pos <- inner_join(pos_cpi, neg_wb, by = c("country", "year_range")) %>% 
  group_by(year_range) %>% 
  mutate(status = "CPI Rise, GDP Fall") %>% 
  select(country, ISO3, year_range, status)
# filter for negative cpi and negative wb, join based on matches and count
neg_neg <- inner_join(neg_cpi, neg_wb, by = c("country", "year_range")) %>% 
  group_by(year_range) %>% 
  mutate(status = "CPI Fall, GDP Fall") %>% 
  select(country, ISO3, year_range, status)

faceted_groups <- rbind(pos_pos, pos_neg, neg_pos, neg_neg) %>% 
  mutate(start = substr(year_range, 1, 4), stop = substr(year_range, 6, 9)) 

gdp_cpi_table_years <- faceted_groups %>% 
  group_by(status, year_range) %>% 
  summarize(total = n())

faceted_gdp_cpi_plot <-  gdp_cpi_table_years %>% 
  group_by(year_range) %>% 
  mutate(largest = total == max(total)) %>% 
  ggplot() +
  geom_col(mapping = aes(x = status, y = total, fill = status)) +
  scale_color_brewer(labels = c("Negative Negative",
                                "Negative Positive",
                                "Positive Negative",
                                "Positive Positive",
                                "what"
  ),palette = "Set1")+
  facet_wrap(~year_range)+
  theme(axis.text.x = element_blank())+
  labs(title = "GDP per Capita and CPI Changes by Year", x ="Status", y = "Number of Countries")

gdp_cpi_table_years_results <- gdp_cpi_table_years %>% 
  group_by(status) %>% 
  summarize(total = sum(total))

gdp_cpi_plot <- gdp_cpi_table_years_results %>% 
  ggplot() +
  geom_col(mapping = aes(x = status, y = total, fill = status)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Country status changes", x ="Year", y = "Number of Countries") +
  theme(axis.text.x = element_text(size = 6, angle = 90))

gdp_cpi_point <- ggplot(gdp_cpi_table_years) + 
  geom_point(mapping = aes(x = year_range, y = total, color = status)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Country status changes", x ="Year", y = "Number of Countries") +
  theme(axis.text.x = element_text(size = 6, angle = 90))

###################
# Analysis Part  ##
###################

top_5 <- gdp_per_capita %>% 
  filter(year == 2018) %>% 
  arrange(-gdp) %>%  
  head(5) 

superpower_iso3c <- c("USA","RUS","CHN","DEU","GBR","FRA","JPN","ISR","SAU","KOR")
super_power <- gdp_per_capita %>% 
  filter(year == 2018) %>%
  filter(iso3c %in%superpower_iso3c) 

combined_GDP <- rbind(top_5, super_power) %>% 
  select(country, gdp) %>% 
  arrange(-gdp)

combined_GDP_plot <- ggplot(data = combined_GDP)+
  geom_col(mapping= aes(x=reorder(country,gdp),y=gdp))+
  labs(title ="GDP per Capita in 2018",
       x = "Country",
       y = "GDP per Capita")+
  theme(axis.text.x = element_text(size = 7, angle = 90))



press_freedom_index2 <- raw_press_freedom_index

press_freedom_index2 <- press_freedom_index2 %>% 
  rename(country = Country.Name, ISO3 = Country.ISO3) %>% 
  gather(key = 'Year', value = 'Value', -country, -ISO3, -Indicator.Id, -Indicator, -Subindicator.Type) %>% 
  filter(Indicator == 'Press Freedom Index') %>% 
  select(-Indicator.Id, -Subindicator.Type) %>% 
  spread(Year, Value) 

###########################
# Question 3 Analysis 
###########################

corruption_index_long <- corruption_cpi_df %>% 
  gather(key = Year, value = Value, -country, -ISO3, -Region) %>% 
  group_by(ISO3) %>% 
  summarize(value = mean(Value, na.rm = TRUE)) %>% 
  mutate(Indicator = 'Avg_Corruption_Perception_Index') %>% 
  filter(value <= 100) %>% 
  spread(Indicator, value) %>% 
  mutate(Avg_Corruption_Perception_Index)
  
press_freedom_long <- press_freedom_index2 %>% 
  gather(key = Year, value = Value, -ISO3, -country, -Indicator) %>% 
  group_by(ISO3)  %>% 
  summarize(value = mean(Value, na.rm = TRUE)) %>% 
  mutate(Indicator = 'Avg_Press_Freedom_Index') %>% 
  filter(value <= 100) %>% 
  spread(Indicator, value) %>% 
  mutate(Avg_Press_Freedom_Index = 100 - Avg_Press_Freedom_Index)

cpi_pfi_wide <- left_join(corruption_index_long, press_freedom_long, by = 'ISO3')

world_power_averages <- cpi_pfi_wide %>% 
  gather(key = Indicator, value = value, -ISO3) %>% 
  filter(grepl("ARE|SAU|ISR|JPN|FRA|DEU|GBR|CHN|RUS|USA", ISO3))

# CPI PFI Plot
world_power_plot <- ggplot(data = world_power_averages, mapping = aes(x = reorder(ISO3, value), y = value, color = Indicator)) +
  geom_linerange(mapping = aes(ymin = 0, ymax = value)) +
  geom_point(mapping = aes(shape = Indicator)) +
  labs(title = "Corruption Perceptions vs Press Freedom of World Powers", x = "Country", y = "Index (2001-2018 Avg)")

# Charts
most_corrupt_2019 <- corruption_cpi_df %>% 
  arrange(`2019`) %>% 
  head(15) %>% 
  select(country, ISO3, `2019`)

least_free_2019 <- press_freedom_index2 %>% 
  arrange(-X2019) %>% 
  top_n(15, X2019) %>% 
  select(country, ISO3, X2019)

corruption_press_correlation_plot <- ggplot(data = cpi_pfi_wide, mapping = aes(x = Avg_Press_Freedom_Index, y = Avg_Corruption_Perception_Index), na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  geom_smooth(na.rm = TRUE, method = "loess", se = FALSE, formula = y ~ x) +
  labs(title = "Correlation Between Corruption Index and Press Freedom Index", x = 'Avg Press Freedom Index', y = 'Avg Corruption Perception Index')

corruption_press_coefficient <- cor(cpi_pfi_wide$Avg_Corruption_Perception_Index, cpi_pfi_wide$Avg_Press_Freedom_Index, use = "complete.obs")
corruption_press_cor_test <- cor.test(cpi_pfi_wide$Avg_Corruption_Perception_Index, cpi_pfi_wide$Avg_Press_Freedom_Index, use = "complete.obs")
corruption_press_t_value <- corruption_press_cor_test[["statistic"]][["t"]]
