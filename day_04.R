library(tidyverse)

source('AoC functions.R')
source('.Rprofile')

download_advent(2022,
                4,
                readr::read_delim,
                delim = ',',
                col_names = c('elf1',
                              'elf2'))


data <- input %>% 
  mutate(across(elf1:elf2,
                ~ str_split(., '-'))) %>% 
  unnest_wider(elf1,
               names_sep = '_') %>% 
  unnest_wider(elf2,
               names_sep = '_') %>% 
  rename_all(~ str_replace(.
                           ,'_1',
                           '_min'))%>% 
  rename_all(~ str_replace(.
                           ,'_2',
                           '_max')) %>% 
  mutate(across(everything(),
                as.numeric)) 


data %>% 
  mutate(enclosed = case_when((elf1_min >= elf2_min & elf1_max <= elf2_max) ~ 1,
                             (elf1_min <= elf2_min & elf1_max >= elf2_max) ~ 1,
                             TRUE ~ 0)) %>% 
  summarise(sum(enclosed))


# part 2 ------------------------------------------------------------------

data %>% 
  mutate(enclosed = ifelse((elf1_max < elf2_min)|
                             (elf2_max < elf1_min),
                           0,
                           1)) %>% 
  summarise(sum(enclosed))
