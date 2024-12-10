################################################################################
################################ -- Options -- #################################
################################################################################

rm(list = ls())
setwd(dirname(rstudioapi::documentPath()))
source("../init.R")

#inp <- read_lines("./test.txt")
#inp <- read_lines("./input.txt")

################################################################################
################################# -- Part 1 -- #################################
################################################################################
backfill <- function(c, first_dot, last_letter) {
  if (last_letter > first_dot) {
    c[first_dot] <- c[last_letter]
    c[last_letter] <- "."
  }
  c
}

backfill_all <- function(c) {
  exit <- FALSE
  while (!exit) {
    all_dots <- which(c == ".")
    all_letts <- which(c != ".")
    first_dot <- min(all_dots)
    last_letter <- max(all_letts)
    if (min(all_dots) > max(all_letts)) {
      exit <- TRUE
    }
    else {
      c <- backfill(c, first_dot, last_letter)
    }
  }
  c
}

#disk_map <- "2333133121414131402"
disk_map <- read_lines("./input.txt") %>% 
  mutate(lines = as.character(lines)) %>% 
  pull(lines) %>%
  paste0(., collapse = "")

dm_split <- as.numeric(split_full(disk_map))

blocks <- ""
for (i in 1:length(dm_split)) {
  curr_v <- dm_split[i]
  id <- (i - 1) / 2
  if (i %% 2 == 0) {
    blocks <- paste0(blocks, paste0(rep(".", curr_v), collapse = ""))
  } else {
    blocks <- paste0(blocks, paste0(rep(id, curr_v), collapse = ""))
  }
}

blocks <- split_full(blocks)

compress <- backfill_all(blocks)
compress[which(compress == ".")] <- 0
compress <- as.numeric(compress)

checksum <- sum(compress * 0:(length(compress)-1))

################################################################################
################################# -- Part 2 -- #################################
################################################################################

