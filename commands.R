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

prem <- prem %>% select(-`Last 5`) #so that the rest of the data lines up

urls <- c("https://fbref.com/en/comps/9/2021-2022/2021-2022-Premier-League-Stats",
          "https://fbref.com/en/comps/9/2020-2021/2020-2021-Premier-League-Stats",
          "https://fbref.com/en/comps/9/2019-2020/2019-2020-Premier-League-Stats",
          "https://fbref.com/en/comps/9/2018-2019/2018-2019-Premier-League-Stats"
          )
for (i in urls) {
full_table <- read_html(i, as.data.frame=T, stringsAsFactors = TRUE)
# html_nodes to pull all nodes under the "table" label
# the number (in this case 2) tells which table to pull from the list of tables
# html_table converts it to table format
prem1 <- full_table %>%  
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) 

prem <- rbind(prem, prem1)
}

nums <- seq(1:length(prem$GF))
prem$nums <- nums
prem$cuts <- cut(prem$nums, 5, labels = F)
prem <- prem %>% mutate(season = case_when(cuts == 1 ~ "2023",
                                  cuts == 2 ~ "2022",
                                  cuts == 3 ~ "2021",
                                  cuts == 4 ~ "2020",
                                  cuts == 5 ~ "2019")
)

prem %>% ggplot(aes(x = GF, y = Pts)) + geom_point()
cor(x = prem$GF, y = prem$Pts)

prem <- prem %>% mutate(pct = (W + 0.5*D)/MP)
prem <- prem %>% mutate(pyth_pct = GF^1.2 / (GF^1.2 + GA ^ 1.2))

cor(x = prem$pct, y = prem$pyth_pct)

#looking for best exponent
exp <- seq(1, 20, by = 0.1)
best_exp <- data.frame(rmse = numeric(0))
dat <- data.frame(rmse = numeric(1))

for (i in exp) {
  dat$rmse <- sqrt(mean(
    (prem$pct - ((prem$GF ^ i)/((prem$GF ^ i) + (prem$GA ^ i))))^2))
  
  best_exp <- rbind(best_exp, dat)
}

best_exp$exp <- exp

best_exp %>% ggplot(aes(x = exp, y = rmse)) + geom_point()

min(best_exp$rmse) #so with exp = 1.2, rmse is minimized

#what's more important, pct or pts?
prem %>% filter(season < 2023) %>% ggplot(aes(x = pyth_pct, y = Pts)) + geom_point()






prem2 <- prem %>% filter(season < 2023)
cor(x = prem2$GD, y = prem2$Pts)
cor(x = prem2$GF, y = prem2$Pts)
cor(x = prem2$Pts, y = prem2$pct)
cor(x = prem$Pts, y = prem$pyth_pct)

prem2 %>% ggplot(aes(x = GD, y = Pts)) +
  geom_point() +
  geom_text_repel(data = filter(prem2, GD > 40), aes(x = GD, y = Pts, label = Squad)) +
  geom_text(data = filter(prem2, GD > 40), aes(x = GD, y = Pts, label = season), nudge_y = -3)
  

prem <- left_join(prem, EPL_logos, by = "Squad")

prem %>% ggplot(aes(x = xG, y = GF)) + 
  geom_image(image = prem$logo_url) + 
  theme_light() + 
  geom_smooth(method = "lm", se = F)


#### looking at xG and G, and maybe ELO ####
EPL22 %>% ggplot(aes(x = HxG, y = homeGF )) + 
  scale_x_continuous(breaks = c(1, 2, 3), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10), minor_breaks = NULL) +
  geom_jitter(height = 0.1) + 
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  facet_wrap(~Home) +
  labs(title = "GF vs xGF at Home",
       x = "Home team xG",
       y = "Home team GF")

EPL22 %>% ggplot(aes(x = AxG, y = awayGF )) + 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10), minor_breaks = NULL) +
  geom_jitter(height = 0.1) + 
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  facet_wrap(~Away) +
  labs(title = "GF vs xGF on the Road",
       x = "Away team xG",
       y = "Away team GF")
