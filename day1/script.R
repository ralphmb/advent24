library(dplyr)

setwd("~/Documents/coding/advent24/day1")
inp <- read.delim("./input.txt", header = FALSE)

# Part 1

nums <- inp %>%
  mutate(sepd = strsplit(V1, split = " ")) %>%
  rowwise() %>% 
  mutate(first = as.numeric(sepd[1]),
         sec = as.numeric(sepd[4])) %>% 
  ungroup() %>%
  select(first, sec)

firsts <- sort(nums$first)
secs <- sort(nums$sec)

print(sum(abs(firsts-secs)))

# Part 2

tab <- nums$sec %>% 
  table() %>% 
  as_tibble() %>%
  rename(t = ".") %>% 
  mutate(across(everything(), ~ as.numeric(.)))

nums_w <- nums %>% 
  left_join(tab, by = join_by(first == t)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(weighted = first * n) %>% 
  pull(weighted) %>%
  sum()
