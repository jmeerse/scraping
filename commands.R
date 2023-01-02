#install.packages("tidyverse")
library(tidyverse)
#install.packages("rvest")
library(rvest)
#install.packages("dplyr")
library(dplyr)
#install.packages("janitor")
library(janitor)
#install.packages("prismatic")
library(prismatic)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("ggimage")
library(ggimage)

load("./.RData")  #use this if environment doesn't load.
 
# url storage
url <- "https://fbref.com/en/comps/9/Premier-League-Stats"
# read_html to scrape the items on url page
full_table <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)
# html_nodes to pull all nodes under the "table" label
# the number (in this case 2) tells which table to pull from the list of tables
# html_table converts it to table format
prem <- full_table %>%  
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) 
# make row 1 column names
#prem <- prem %>% row_to_names(1)


prem <- left_join(prem, EPL_logos, by = "Squad")

prem %>% ggplot(aes(x = xG, y = GF)) + 
  geom_image(image = prem$logo_url) + 
  theme_light() + 
  geom_smooth(method = "lm", se = F)
