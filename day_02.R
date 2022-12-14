library(tidyverse)

source('AoC functions.R')
source('.Rprofile')

download_advent(2022,
                2,
                readr::read_delim,
                delim = ' ',
                col_names = c('your_move',
                              'my_move'))


score <- tribble(~move, ~score,
                 'A', 1,
                 'B', 2,
                 'C', 3,
                 'X', 1,
                 'Y', 2,
                 'Z', 3)

data <- input %>% 
  left_join(score %>% 
              rename(your_score = score),
            by = c('your_move' = 'move')) %>% 
  left_join(score %>% 
              rename(my_score = score),
            by = c('my_move' = 'move')) %>% 
  mutate(result_score = case_when(my_score == your_score + 1 |
                                    your_score == 3 & my_score == 1 ~ 6,
                                  my_score == your_score ~ 3,
                                  TRUE ~ 0),
         score = my_score + result_score)

data %>% 
  summarise(sum(score))


# Part 2 ------------------------------------------------------------------

data_2 <- input %>% 
  left_join(score %>% 
              rename(your_score = score),
            by = c('your_move' = 'move')) %>% 
  mutate(my_score = case_when(my_move == 'X' ~ your_score - 1,
                              my_move == 'Y' ~ your_score,
                              TRUE  ~ your_score + 1),
         my_score = case_when(my_score == 0 ~ 3,
                              my_score == 4 ~ 1,
                              TRUE ~ my_score),
         result_score = case_when(my_move == 'X' ~ 0,
                                  my_move == 'Y' ~ 3,
                                  my_move == 'Z' ~ 6),
         score = my_score + result_score)

data_2 %>% 
  summarise(sum(score))
