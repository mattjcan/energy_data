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

background <- c("#d9d9d9")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme(axis.line.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 8)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2))

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Dropbox/01a. Resources/data/BP/" # parent directory for the data

prices <- read_csv(paste0(d,"data/world_bank_prices.csv"), skip = 6)

# TIDY ------------------------------------------------------------------

coal_prices <- prices[ ,c(1, 6)]

names(coal_prices) <- c("month", "aus_coal")

coal_prices$month <- str_replace(coal_prices$month, "M", "-")

coal_prices$month <- paste0(coal_prices$month, "-01")

coal_prices$month <- as.Date(coal_prices$month, "%Y-%m-%d")

coal_prices <- coal_prices[121:701, ]

coal_prices$aus_coal <- as.numeric(coal_prices$aus_coal)

# VISUALISE --------------------------------------------------------------------

p_coal_prices <- coal_prices %>%
  filter(month > "2001-01-01") %>% 
  ggplot(aes(x = month, y = aus_coal)) + 
  geom_line(color = "#4484ce", size = 1) + 
  theme_mc + 
  labs(title = "Thermal coal prices", subtitle = "per metric tonne, US nominal dollars", caption = "Source: World Bank Commodity Price Data, Newcastle/Port Kembla from 2002 onwards, 6,300 kcal/kg", x ="", y = "") 

# EXPORT --------------------------------------------------------------------

png("images/p_coal_prices.png", width = 6, height = 3, units = "in", res = 300)
p_coal_prices
dev.off() 
