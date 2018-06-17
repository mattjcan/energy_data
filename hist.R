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

hist_coal <- read_csv(paste0(d,"data/hist_coal.csv"), skip = 0)

# VISUALISE --------------------------------------------------------------------

p_hist_coal <- hist_coal %>%
  ggplot(aes(x = year, y = ej)) + 
  geom_area(aes(fill = type, color = type), size = 1) + 
  scale_fill_manual(values = c("#F19F4D", "#4484ce")) +
  scale_color_manual(values = c("#F19F4D", "#4484ce")) +
  theme_mc + 
  labs(title = "Primary energy coal use", subtitle = "exajoules", caption = "Source: IEA World Energy Outlook & Ritchie, H. and Roser, M. https://bit.ly/2HkJ0jf", x ="", y = "") + 
  annotate("text", label = "Before the 21st century\n5,700EJ of coal produced", x = 1850, y = 28, size = 3) +
  annotate("text", label = "First 40 years of 21st century 6100EJ\nof coal expected to be produced", x = 1950, y = 125, size = 3)


