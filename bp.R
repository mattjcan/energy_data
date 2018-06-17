# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(rgdal) 
library(leaflet)
library(knitr)
library(xaringan)
library(rmarkdown)
library(gridExtra)
library(widgetframe)
library(kableExtra)
library(ggthemes)

# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 14, face = "bold")) +
  theme(axis.text = element_text(size = 10, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.line.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2))

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Dropbox/01a. Resources/data/energy_data/" # parent directory for the data

coal_demand <- read_csv(paste0(d,"data/coal_consumption.csv"), skip = 2)

coal_supply <- read_csv(paste0(d,"data/coal_production.csv"), skip = 2)

elec_gen_coal <- read_csv(paste0(d,"data/elec_gen_coal.csv"), skip = 2)

coal <- read_csv(paste0(d,"data/elec_gen_coal.csv"), skip = 2)

gas <- read_csv(paste0(d,"data/elec_gen_gas.csv"), skip = 2)

hydro <- read_csv(paste0(d,"data/elec_gen_hydro.csv"), skip = 2)

nuclear <- read_csv(paste0(d,"data/elec_gen_nuclear.csv"), skip = 2)

oil <- read_csv(paste0(d,"data/elec_gen_oil.csv"), skip = 2)

solar <- read_csv(paste0(d,"data/elec_gen_solar.csv"), skip = 2)

wind <- read_csv(paste0(d,"data/elec_gen_wind.csv"), skip = 2)

elec_gen_fuel <- read_csv(paste0(d,"data/elec_gen_fuel.csv"), skip = 2)

pov <- read_csv(paste0(d,"data/poverty.csv"), skip = 0)

# coal prices?

# TIDY --------------------------------------------------------------------

# coal demand

coal_demand <- coal_demand[1:106, 1:54] # remove footnotes and extra columns

coal_demand <- coal_demand %>% 
  rename(Country = `Million tonnes oil equivalent`) %>% 
  filter(!is.na(Country)) 

reg_key <- tibble(Country = coal_demand$Country, region = c(rep("North America", 4), rep("South America", 12), rep("Europe", 34), rep("CIS", 10), rep("Middle East", 10), rep("Africa", 10), rep("Asia Pacific", 19)))

reg_key <- rbind(reg_key, c("Other S. & Cent. America", "South America"), c("Other Africa", "Africa"), c("Serbia", "Europe"), c("Zimbabwe", "Africa"), c("Mongolia", "Asia Pacific"))

coal_demand <- coal_demand %>% 
  filter(!str_detect(Country, 'Total'))

coal_demand[coal_demand == "^"] <- NA

coal_demand[coal_demand == "-"] <- NA

# coal supply

coal_supply <- coal_supply[1:54, 1:38] # remove footnotes and extra columns

coal_supply <- coal_supply %>% 
  rename(Country = `Million tonnes`) %>% 
  filter(!is.na(Country) & !str_detect(Country, 'Total'))

coal_supply <- mutate_all(coal_supply, funs(replace(., .=='-', NA)))

coal_supply <- mutate_all(coal_supply, funs(replace(., .=='^', NA)))

coal_supply <- mutate_all(coal_supply, funs(replace(., .=='n/a', NA)))

# Electricity generation by coal 

coal <- coal[1:50, 1:34]

coal <- coal %>% 
  rename(Country = `Terawatt-hours`) %>% 
  filter(!is.na(Country) & !str_detect(Country, 'Total'))

coal <- mutate_all(coal, funs(replace(., .=='-', NA)))

coal <- mutate_all(coal, funs(replace(., .=='^', NA)))

# Electricity generation by gas 

gas <- gas[1:50, 1:34]

gas <- gas %>% 
  rename(Country = `Terawatt-hours`) %>% 
  filter(!is.na(Country) & !str_detect(Country, 'Total'))

gas <- mutate_all(gas, funs(replace(., .=='-', NA)))

gas <- mutate_all(gas, funs(replace(., .=='^', NA)))

# Electricity generation by oil 

oil <- oil[1:50, 1:34]

oil <- oil %>% 
  rename(Country = `Terawatt-hours`) %>% 
  filter(!is.na(Country) & !str_detect(Country, 'Total'))

oil <- mutate_all(oil, funs(replace(., .=='-', NA)))

oil <- mutate_all(oil, funs(replace(., .=='^', NA)))

# Electricity generation by hydro 

hydro <- hydro[1:106, 1:54]

hydro <- hydro %>% 
  rename(Country = `Terawatt-hours`) %>% 
  filter(!is.na(Country) & !str_detect(Country, 'Total'))

hydro <- mutate_all(hydro, funs(replace(., .=='-', NA)))

hydro <- mutate_all(hydro, funs(replace(., .=='^', NA)))

# Electricity generation by nuclear 

nuclear <- nuclear[1:106, 1:54]

nuclear <- nuclear %>% 
  rename(Country = `Terawatt-hours`) %>% 
  filter(!is.na(Country) & !str_detect(Country, 'Total'))

nuclear <- mutate_all(nuclear, funs(replace(., .=='-', NA)))

nuclear <- mutate_all(nuclear, funs(replace(., .=='^', NA)))

# Electricity generation by solar 

solar <- solar[1:106, 1:54]

solar <- solar %>% 
  rename(Country = `Terawatt-hours`) %>% 
  filter(!is.na(Country) & !str_detect(Country, 'Total'))

solar <- mutate_all(solar, funs(replace(., .=='-', NA)))

solar <- mutate_all(solar, funs(replace(., .=='^', NA)))

# Electricity generation by wind 

wind <- wind[1:106, 1:54]

wind <- wind %>% 
  rename(Country = `Terawatt-hours`) %>% 
  filter(!is.na(Country) & !str_detect(Country, 'Total'))

wind <- mutate_all(wind, funs(replace(., .=='-', NA)))

wind <- mutate_all(wind, funs(replace(., .=='^', NA)))



# TRANSFORM --------------------------------------------------------------------

# coal demand

coal_demand <- coal_demand %>%
  gather(key = year, value = coal_demand, -Country)

coal_demand[,2:3] <- lapply(coal_demand[2:3], as.numeric)

coal_demand$region <- reg_key$region[match(coal_demand$Country, reg_key$Country)]

demand_by_region <- coal_demand %>% 
  group_by(year, region) %>% 
  summarise(coal_demand = sum(coal_demand, na.rm = TRUE))

demand_by_globe <- coal_demand %>% 
  group_by(year) %>% 
  summarise(coal_demand = sum(coal_demand, na.rm = TRUE))

# coal supply

coal_supply <- coal_supply %>%
  gather(key = year, value = coal_supply, -Country)

coal_supply[,2:3] <- lapply(coal_supply[2:3], as.numeric)

coal_supply$region <- reg_key$region[match(coal_supply$Country, reg_key$Country)]

supply_by_region <- coal_supply %>% 
  group_by(year, region) %>% 
  summarise(coal_supply = sum(coal_supply, na.rm = TRUE))

supply_by_globe <- coal_supply %>% 
  group_by(year) %>% 
  summarise(coal_supply = sum(coal_supply, na.rm = TRUE))

# Electricity generation by fuel 

coal <- coal %>% 
  gather(key = year, value = twh, -Country)

gas <- gas %>% 
  gather(key = year, value = twh, -Country)

oil <- oil %>% 
  gather(key = year, value = twh, -Country)

hydro <- hydro %>% 
  gather(key = year, value = twh, -Country)

nuclear <- nuclear %>% 
  gather(key = year, value = twh, -Country)

solar <- solar %>% 
  gather(key = year, value = twh, -Country)

wind <- wind %>% 
  gather(key = year, value = twh, -Country)

coal$fuel <- "coal"

gas$fuel <- "gas"

oil$fuel <- "oil"

hydro$fuel <- "hydro"

nuclear$fuel <- "nuclear"

solar$fuel <- "solar"

wind$fuel <- "wind"

elec_gen_fuel <- rbind(coal, gas, oil, hydro, nuclear, solar, wind)

elec_gen_fuel[, 2:3] <- lapply(elec_gen_fuel[, 2:3], as.numeric)

elec_gen_fuel$region <- reg_key$region[match(elec_gen_fuel$Country, reg_key$Country)]

elec_gen_india <- elec_gen_fuel %>% 
  filter(Country == "India", fuel %in% c("coal", "solar"))

fuel_by_region <- elec_gen_fuel %>% 
  group_by(year, region, fuel) %>% 
  summarise(twh = sum(twh, na.rm = TRUE))

fuel_by_globe <- elec_gen_fuel %>% 
  group_by(year, fuel) %>% 
  summarise(twh = sum(twh, na.rm = TRUE))

fuel_delta <- fuel_by_globe %>% 
  filter(year %in% c(2016, 2017)) %>% 
  group_by(fuel) %>% 
  arrange(year) %>% 
  mutate(delta = twh - first(twh)) %>% 
  ungroup(fuel) %>% 
  filter(year == "2017")

fuel_delta_ap <- fuel_by_region %>% 
  filter(year %in% c(2016, 2017) & region == "Asia Pacific") %>% 
  group_by(fuel) %>% 
  arrange(year) %>% 
  mutate(delta = twh - first(twh)) %>% 
  ungroup(fuel) %>% 
  filter(year == "2017")

fuel_delta_india <- elec_gen_fuel %>% 
  filter(year %in% c(2016, 2017) & Country == "India") %>% 
  group_by(fuel) %>% 
  arrange(year) %>% 
  mutate(delta = twh - first(twh)) %>% 
  ungroup(fuel) %>% 
  filter(year == "2017")


# Electricity generation by coal 

coal[, 2:3] <- lapply(coal[, 2:3], as.numeric)

coal$region <- reg_key$region[match(coal$Country, reg_key$Country)]

gen_by_region <- coal %>% 
  group_by(year, region) %>% 
  summarise(twh = sum(twh, na.rm = TRUE))

gen_by_globe <- coal %>% 
  group_by(year) %>% 
  summarise(twh = sum(twh, na.rm = TRUE))

gen_by_ap <- gen_by_region %>% 
  filter(region == "Asia Pacific") 

first_ap <- gen_by_ap$twh[1]


# VISUALISE --------------------------------------------------------------------

# global coal consumption

p_g_demand <- demand_by_globe %>% 
  ggplot(aes(x = year, y = coal_demand)) + 
  geom_line(color = "#4484ce", size = 1) + 
  theme_mc + 
  labs(title = "Global coal consumption", subtitle = "million tonnes of oil equivalent", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") +
  ylim(200,4000)

# regional coal consumption

f_r_demand <- function(reg) {
  demand_by_region %>% 
  filter(region == reg) %>% 
  ggplot(aes(x = year, y = coal_demand)) + 
  geom_line(color = "#2166ac", size = 1) +
  theme_mc + 
  labs(title = paste("Coal consumption in", reg), subtitle = "million tonnes of oil equivalent", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 
}

p_r_demand <- map(unique(coal_demand$region), f_r_demand)

names(p_r_demand) <- unique(coal_demand$region)

# coal consumption by country

f_c_demand <- function(country) { 
  coal_demand %>% 
  filter(Country == country) %>% 
  ggplot(aes(x = year, y = coal_demand)) + 
  geom_line(color = "#2166ac", size = 1) + 
  theme_mc + 
  labs(title = paste("Coal consumption in", country), subtitle = "million tonnes of oil equivalent", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 
}

p_c_demand <- map(unique(coal_demand$Country), f_c_demand)

names(p_c_demand) <- unique(coal_demand$Country)

# global coal production

p_g_supply <- supply_by_globe %>% 
  ggplot(aes(x = year, y = coal_supply)) + 
  geom_line(color = "#4484ce", size = 1) + 
  theme_mc + 
  labs(title = "Global coal production", subtitle = "million tonnes", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 

# regional coal production

f_r_supply <- function(reg) {
  supply_by_region %>% 
    filter(region == reg) %>% 
    ggplot(aes(x = year, y = coal_supply)) + 
    geom_line(color = "#2166ac", size = 1) + 
    theme_mc + 
    labs(title = paste("Coal production in", reg), subtitle = "million tonnes of oil equivalent", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 
}

p_r_supply <- map(unique(coal_supply$region), f_r_supply)

names(p_r_supply) <- unique(coal_supply$region)

# coal production by country

f_c_supply <- function(country) { 
  coal_supply %>% 
    filter(Country == country) %>% 
    ggplot(aes(x = year, y = coal_supply)) + 
    geom_line(color = "#2166ac", size = 1) + 
    theme_mc + 
    labs(title = paste("Coal production in", country), subtitle = "million tonnes of oil equivalent", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 
}

p_c_supply <- map(unique(coal_supply$Country), f_c_supply)

names(p_c_supply) <- unique(coal_supply$Country)

# global coal electricity generation

p_g_gen <- gen_by_globe %>% 
  ggplot(aes(x = year, y = twh)) + 
  geom_line(color = "#4484ce", size = 1) + 
  theme_mc + 
  labs(title = "Global electricty generated by coal", subtitle = "terawatt hours", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 

p_g_fuel <- fuel_by_globe %>% 
  filter(year > 1985) %>% 
  ggplot(aes(x = year, y = twh)) + 
  geom_line(aes(color = fuel), size = 1) + 
  theme_mc + 
  theme(legend.position = c(0.15, 0.867), legend.text = element_text(size=8), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank(), legend.key.size = unit(2.9, "mm")) +
  guides(color=guide_legend(ncol=3)) +
  scale_color_manual(values = c(coal = "black", gas = "#762a83", hydro = "#F19F4D", nuclear = "#d73027", oil = "#4d4d4d", solar = "#4d9221", wind = "#4484ce"), labels=c("Coal", "Gas", "Hydro", "Nuclear", "Oil", "Solar", "Wind")) +
  labs(title = "Global electricty generated by fuel type", subtitle = "terawatt hours", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 

p_g_india <- elec_gen_india %>% 
  filter(year > 1985) %>% 
  ggplot(aes(x = year, y = twh)) + 
  geom_line(aes(color = fuel), size = 1) + 
  theme_mc + 
  theme(legend.position = c(0.15, 0.4), legend.text = element_text(size=9), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank(), legend.key.size = unit(3.5, "mm")) +
  scale_color_manual(values = c(coal = "black", solar = "#4d9221"), labels=c("Coal", "Solar")) +
  labs(title = "Indian electricty", subtitle = "terawatt hours", caption = "Source: BP Statisical Review of World Energy", x ="", y = "")

p_delta <- fuel_delta %>% 
  ggplot(aes(x = reorder(fuel, -delta), y = delta,  fill = fuel))  + 
  geom_bar(stat="identity") + 
  theme_mc +
  scale_fill_manual(values = c(coal = "black", gas = "#762a83", hydro = "#F19F4D", nuclear = "#d73027", oil = "#4d4d4d", solar = "#4d9221", wind = "#4484ce"))  +
  scale_x_discrete(labels = c("Coal", "Solar", "Wind", "Nuclear", "Gas", "Hydro", "Oil")) +
  labs(title = paste("Change in global electricity generation from 2016 to 2017"), subtitle = "terawatt hours", caption = "Source: BP Statisical Review of World Energy", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(delta,1)), vjust = ifelse(delta >= 0, -1, 1.5)), size=3) +
  ylim(-120, 350)

p_delta_ap <- fuel_delta_ap %>% 
  ggplot(aes(x = reorder(fuel, -delta), y = delta,  fill = fuel))  + 
  geom_bar(stat="identity") + 
  theme_mc +
  scale_fill_manual(values = c(coal = "black", gas = "#762a83", hydro = "#F19F4D", nuclear = "#d73027", oil = "#4d4d4d", solar = "#4d9221", wind = "#4484ce")) +
  scale_x_discrete(labels = c("Coal", "Solar", "Wind", "Nuclear", "Gas", "Hydro", "Oil")) +
  labs(title = paste("Change in electricity generation in Asia Pacific from 2016 to 2017"), subtitle = "terawatt hours", caption = "Source: BP Statisical Review of World Energy", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(delta,1)), vjust = ifelse(delta >= 0, -1, 1.5)), size=3) +
  ylim(-75, 400)

p_delta_india <- fuel_delta_india %>% 
  ggplot(aes(x = reorder(fuel, -delta), y = delta,  fill = fuel))  + 
  geom_bar(stat="identity") + 
  theme_mc +
  scale_fill_manual(values = c(coal = "black", gas = "#762a83", hydro = "#F19F4D", nuclear = "#d73027", oil = "#4d4d4d", solar = "#4d9221", wind = "#4484ce")) +
  scale_x_discrete(labels = c("Coal", "Solar", "Wind", "Nuclear", "Gas", "Hydro", "Oil")) +
  labs(title = paste("Change from 2016 to 2017"), subtitle = "terawatt hours", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text.x = element_text(size = 8)) +
  geom_text(aes(label = paste(round(delta,1)), vjust = ifelse(delta >= 0, -1, 1.5)), size=3) +
  ylim(-5,60)

# regional coal electricity generation

f_r_gen <- function(reg) {
  gen_by_region %>% 
    filter(region == reg) %>% 
    ggplot(aes(x = year, y = twh)) + 
    geom_line(color = "#2166ac", size = 1) + 
    theme_mc + 
    labs(title = paste("Electricity generated by coal in", reg), subtitle = "terawatt hours", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 
}

p_r_gen <- map(unique(coal$region), f_r_gen)

names(p_r_gen) <- unique(coal$region)

# coal electricity generation by country

f_c_gen <- function(country) { 
  coal %>% 
    filter(Country == country) %>% 
    ggplot(aes(x = year, y = twh)) + 
    geom_line(color = "#2166ac", size = 1) + 
    theme_mc + 
    labs(title = paste("Electricity generated by coal in", country), subtitle = "terawatt hours", caption = "Source: BP Statisical Review of World Energy", x ="", y = "") 
}

p_c_gen <- map(unique(coal$Country), f_c_gen)

names(p_c_gen) <- unique(coal$Country)

# poverty

p_pov_ap <- pov %>% 
  filter(Year >= 1985) %>% 
  ggplot(aes(x = Year, y = Poverty)) + 
  geom_line(color = "#2166ac", size = 1) + 
  theme_mc + 
  labs(title = "Proportion of population living in poverty", subtitle = "East Asia & Pacific, below US$1.90 per day, real dollars", caption = "Source: World Bank DataBank, Poverty and Equity", x ="", y = "") 

