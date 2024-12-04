library(dplyr)

rm(list = ls())
setwd("~/Documents/coding/advent24/day2")
inp <- read.delim("./input.txt", header = FALSE)

# Part 1 & 2

check_ln <- function(ln) {
  ln <- as.numeric(ln)
  conds <- c()
  for (i in seq_len(length(ln))) {
    ln2 <- ln[setdiff(1:length(ln), i)]
    diffs <- diff(ln2)
    sign_ch <- all(diffs > 0) | all(diffs < 0)
    mag_ch <- max(abs(diffs)) <= 3
    conds <- c(conds, sign_ch & mag_ch)
  }
  return(any(conds))
}

lines <- inp %>%
  mutate(sepd = strsplit(V1, split = " ")) %>% 
  rowwise() %>%
  mutate(check = check_ln(sepd)) %>% 
  filter(check) %>% 
  nrow()
