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

prices <- read_csv(paste0(d,"data/world_bank_prices.csv"), skip = 6)

met <- read_csv(paste0(d,"data/met_prices.csv"), skip = 0)

# TIDY ------------------------------------------------------------------

coal_prices <- prices[ ,c(1, 6)]

names(coal_prices) <- c("date", "price")

coal_prices$date <- str_replace(coal_prices$date, "M", "-")

coal_prices$date <- paste0(coal_prices$date, "-01")

coal_prices$date <- as.Date(coal_prices$date, "%Y-%m-%d")

coal_prices <- coal_prices[121:701, ]

coal_prices$price <- as.numeric(coal_prices$price)

coal_prices$type <- "thermal"

# met prices

met$date <- as.Date(met$date, "%d-%b-%y")

met$type <- "met"

coal_prices <- rbind(coal_prices, met)

# VISUALISE --------------------------------------------------------------------

p_coal_prices <- coal_prices %>%
  filter(date > "2001-01-01") %>% 
  ggplot(aes(x = date, y = price)) + 
  geom_line(aes(color = type), size = 1) + 
  theme_mc + 
  labs(title = "Coal prices", subtitle = "per metric tonne, US nominal dollars", caption = "Source: World Bank Commodity Price Data, Bloomberg and IHS Markit", x ="", y = "") +
  scale_color_manual(values = c(thermal = "black", met = "blue"), labels = c("Met", "Thermal")) + 
  theme(legend.position = "right", legend.text = element_text(size=8), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank()) 


# EXPORT --------------------------------------------------------------------


