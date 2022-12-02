library(tidyverse)

data <- read_csv('data/input1.txt',
               col_names = 'cals',
               col_types = 'n',
               na = character(),
               skip_empty_rows = F) %>% 
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
