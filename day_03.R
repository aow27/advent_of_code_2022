library(tidyverse)


source('AoC functions.R')
source('.Rprofile')

download_advent(2022,
                3,
                readr::read_csv,
                col_names = c('content'))


priority_value <- tibble(letter = c(letters,LETTERS),
       priority = 1:52)

analysis <- input %>% 
  mutate(length = str_count(content),
         first_half = str_sub(content,
                              1,
                              length/2),
         second_half = str_sub(content,
                               length/2 + 1,
                               length),
         match = str_extract(second_half,
                             paste0('[',first_half,']'))) %>% 
  left_join(priority_value,
            by = c('match' = 'letter')) 

analysis %>% 
  pull(priority) %>% 
  sum(., na.rm = T)

# Part 2 ------------------------------------------------------------------

analysis_1 <- analysis %>% 
  mutate(group = (row_number()-1) %/% 3,
         numb  = (row_number()-1) %% 3 + 1) %>%  
  select(group, numb, content) %>% 
  pivot_wider(group,
              names_from = numb,
              names_prefix = 'elf_',
              values_from = content) %>% 
  mutate(comp1 = map_chr(str_extract_all(elf_2,
                                 paste0('[',elf_1,']')),
                         ~ str_c(.x, collapse="")),
         comp2 = str_extract(elf_3,
                             paste0('[',comp1,']'))) %>% 
  
left_join(priority_value,
            by = c('comp2' = 'letter')) 

analysis_1 %>% 
  pull(priority) %>% 
  sum()

