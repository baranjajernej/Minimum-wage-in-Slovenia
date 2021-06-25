# load packages ----
library(tidyverse)
library(here)
library(gganimate)

# read in data ----
mw <- read_csv(here("data", "earn_mw_cur_2_Data.csv"))

# explore the data ----
view(mw)
str(mw)
summary(mw)

# rename ----
mw1 <- rename(mw, time = TIME, 
                  country = GEO, 
                  currency = CURRENCY, 
                  wage = Value)

# convert ----
col_conv <- c("wage") 

my_data_updated <- mw1

my_data_updated[ , col_conv] <- lapply(my_data_updated[ , col_conv], 
                                       function(x){ as.numeric(as.character(gsub(",", "", x))) })

# data manipulation
cleanmw <- my_data_updated %>% 
  filter(!grepl('S2', time)) %>% 
  mutate(year = str_sub(time, 1, -3)) %>% 
  select(-`Flag and Footnotes`, - `time`) %>% 
  filter(country == "Slovenia") 

# basic geom col & animation ----
cleanmw %>% 
  ggplot(aes(x = year, y = wage, fill = wage)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = wage), vjust = -0.6, size = 3) +
  labs(title = "Minimum wage in Slovenia 2008 - 2021",
       caption = "Data source: Eurostat", 
       y = "Minimum wage in Slovenia in €", 
       x = "Years: 2008 - 2021") +
  transition_states(year, state_length = 3)

# save animation
anim_save("outputmw.gif", animation = last_animation(), path = NULL)
