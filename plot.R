# loading packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(gganimate)

#################################################################
##                        Data Cleaning                        ##
#################################################################
#importing data
gdp <- read.csv("./data/data_gdp.csv") %>% 
  select(-Series.Name, -Series.Code) %>%
  naniar::replace_with_na_all(condition = ~.x == "..") %>% # to replace .. with NAs
  na.omit()

mobilesub <- read.csv("./data/data_mobilesub.csv") %>% 
  select(-Series.Name, -Series.Code)  %>%
  naniar::replace_with_na_all(condition = ~.x == "..") %>% # to replace .. with NAs
  na.omit()

pop <- read.csv("./data/data_pop.csv") %>% 
  select(-Series.Name, -Series.Code)  %>%
  naniar::replace_with_na_all(condition = ~.x == "..") %>% # to replace .. with NAs
  na.omit()

income_classification <- readxl::read_excel("./data/class.xlsx") %>% 
  select(Code, 'Income group')

cleanheader <- c("country", "country_code", "2002":"2021")
names(gdp) <- cleanheader #cleaning up headers for gdp
names(mobilesub) <- cleanheader #cleaning up headers for mobilesub
names(pop) <- cleanheader #cleaning up headers for mobilesub
names(income_classification) <- c("country_code", "grouping")

#flipping orientation of data to long format
gdp_long <- gather(gdp, key = "year", value = "gdp_per_capita", "2002":"2021")
mobilesub_long <- gather(mobilesub, key = "year", value = "mobilesub_per100", "2002":"2021")
pop_long <- gather(pop, key = "year", value = "popsize", "2002":"2021")

#merging gdp and mobile sub datasets
mergeddata <- inner_join(gdp_long, mobilesub_long, by = c("country", "country_code", "year")) %>% 
  inner_join(pop_long, by = c("country", "country_code", "year")) %>% 
  inner_join(income_classification, by = "country_code")

#ensuring variables are of the correct type
mergeddata[,c("gdp_per_capita", "mobilesub_per100", "popsize")] <- lapply(mergeddata[,c("gdp_per_capita", "mobilesub_per100", "popsize")], as.numeric)
mergeddata$year <- as.numeric(mergeddata$year)
mergeddata[,c("gdp_per_capita", "mobilesub_per100")] <- round(mergeddata[,c("gdp_per_capita", "mobilesub_per100")], digits = 2)

mergeddata <- mergeddata %>% 
  mutate(gdp_per_capita_1000s = gdp_per_capita/1000, .after = gdp_per_capita)

##################################################################
##                         Static Plots                         ##
##################################################################
lmic_data <- mergeddata %>%
  filter(grouping != "High income")

baseplot <- lmic_data %>%
  filter(year == 2002 | year == 2011 | year == 2021) %>% 
  ggplot(aes(x = gdp_per_capita_1000s, y = mobilesub_per100, size = popsize, colour = grouping)) +
  geom_point(alpha = 0.4) +
  labs(x = "GDP per Capita (in Thousands of Dollars)", y = "Mobile Cellular Subscriptions (per 100 people)", colour = "Income Group") +
  scale_size(range = c(2,20), guide = "none") +
  scale_x_continuous(limits = c(0,15)) +
  scale_y_continuous(limits = c(0,200)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 12, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 12)))

baseplot + facet_wrap(vars(year))

#ggsave("./plots/dotplot.png", width = 10, height = 7, units = "in")

comparison_data <- mergeddata %>%
  filter(year == 2021) %>% 
  group_by(grouping) %>% 
  summarise(mean_mobilesub_per100 = mean(mobilesub_per100))

comparison_plot <- comparison_data %>% 
  ggplot(aes(x = mean_mobilesub_per100, y = fct_reorder(grouping, mean_mobilesub_per100))) +
  geom_col(width = 0.5, fill = "steelblue") +
  labs(x = "Average Mobile Cellular Subscriptions (per 100 people)", y = "Income Group") +
  geom_text(aes(label = round(mean_mobilesub_per100, digits = 2)), color = "white", size = 4, hjust = 1.3) +
  scale_x_continuous(limits = c(0,140)) +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 12, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 12)))
  
#ggsave("./plots/barplot.png", width = 12, height = 7, units = "in")

#################################################################
##                        Animated Plot                        ##
#################################################################
baseplot_anim <- lmic_data %>% 
  ggplot(aes(x = gdp_per_capita_1000s, y = mobilesub_per100, size = popsize, colour = grouping)) +
  geom_point(aes(group = country), alpha = 0.4) +
  scale_size(range = c(2,20), guide = "none") +
  scale_x_continuous(limits = c(0,15)) +
  scale_y_continuous(limits = c(0,200)) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 12, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 12)))

animatedplot <- baseplot_anim + 
  transition_time(year) +
  labs(title = "Mobile Cellular Subscriptions (per 100 people) in LMICs", subtitle = "Year: {as.integer(frame_time)}", x = "GDP per Capita (in Thousands of Dollars)", y = "Mobile Cellular Subscriptions (per 100 people)", colour = "Income Group")

animate(animatedplot,
        fps = 30,
        duration = 20,
        start_pause = 10,
        end_pause = 150)

anim_save("./plots/dotplot_anim.gif")
