################################################################################
################################ -- Options -- #################################
################################################################################

rm(list = ls())
setwd(dirname(rstudioapi::documentPath()))
source("../init.R")

################################################################################
################################# -- Part 1 -- #################################
################################################################################

inp <- read_to_matrix("./input.txt", as_nums = TRUE)

is_on_edge <- function(mat, posns) {
  height <- dim(mat)[1]
  width <- dim(mat)[2]
  t_or_b <- posns$row == height | posns$row == 1
  l_or_r <- posns$col == width  | posns$col == 1
  t_or_b | l_or_r
}

find_trailhead_coords <- function(mat) {
  zeros <- get_coords(mat, 0)
  zeros
}

add_trailhead_id <- function(coords) {
  coords %>% mutate(id = glue::glue("{row},{col}"))
}

get_adj_n <- function(mat, coords, n) {
  #Starting from positions given by `coords`, which adjacent spots contain `n`?
  #Return data frame of coords with ids matching those found in coords
  all_ns <- get_coords(mat, n)
  all_adj <- bind_rows(
    mutate(coords, row = row - 1),
    mutate(coords, row = row + 1),
    mutate(coords, col = col + 1),
    mutate(coords, col = col - 1)
  )
  adj_ns <- all_adj %>% 
    left_join(mutate(all_ns, t = 1), by = join_by(row, col)) %>% 
    filter(t == 1) %>% 
    select(-t)
  adj_ns
}

trailhead_coords <- add_trailhead_id(find_trailhead_coords(inp))
prev_coords <- trailhead_coords
for (n in 1:9) {
  these_coords <- get_adj_n(inp, prev_coords, n)
  prev_coords <- these_coords
}

out <- these_coords %>% 
  distinct() %>%
  group_by(id) %>% 
  summarise(count = n()) %>% 
  pull(count) %>% 
  sum()
################################################################################
################################# -- Part 2 -- #################################
################################################################################
inp <- read_to_matrix("./input.txt", as_nums = TRUE)


get_adj_n <- function(mat, coords, n) {
  #Starting from positions given by `coords`, which adjacent spots contain `n`?
  #Return data frame of coords with new coords glued onto old
  all_ns <- get_coords(mat, n)
  all_adj <- bind_rows(
    mutate(coords, row = row - 1),
    mutate(coords, row = row + 1),
    mutate(coords, col = col + 1),
    mutate(coords, col = col - 1)
  )
  adj_ns <- all_adj %>% 
    left_join(mutate(all_ns, t = 1), by = join_by(row, col)) %>% 
    filter(t == 1) %>% 
    select(-t) %>% 
    mutate(id = paste0(id, ";", row, ",", col))
  adj_ns
}

trailhead_coords <- add_trailhead_id(find_trailhead_coords(inp))
prev_coords <- trailhead_coords

for (n in 1:9) {
  these_coords <- get_adj_n(inp, prev_coords, n)
  prev_coords <- these_coords
}

out <- these_coords %>% 
  distinct() %>%
  group_by(row, col) %>% 
  summarise(count = n(), .groups = "drop") %>% 
  pull(count) %>% 
  sum()
