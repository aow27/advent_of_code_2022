library(tidyverse)


source('AoC functions.R')
source('.Rprofile')

download_advent(2022,
                1,
                readr::read_delim,
                delim = ' ',
                skip_empty_rows = FALSE,
                col_names = c('cals'))

data <- input %>% 
  mutate(id = cumsum(case_when(is.na(cals) ~ 1,
                               TRUE ~ 0))) %>% 
  filter(!is.na(cals))

data %>% 
  group_by(id) %>% 
  summarise(cals = sum(cals)) %>% 
  ungroup() %>% 
  pull(cals) %>% 
  max()


## Part 2

data %>% 
  group_by(id) %>% 
  summarise(cals = sum(cals)) %>% 
  ungroup() %>% 
  slice_max(cals, n = 3) %>% 
  pull(cals) %>% 
  sum()
