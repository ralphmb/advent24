library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

options(digits = 16)

################################################################################
############################### READING FUNCTIONS ##############################
################################################################################

read_lines <- function(path = "./input.txt", skip_blank = FALSE) {
  read.delim(path, header = FALSE, blank.lines.skip = skip_blank,
             colClasses = "character") %>%
    rename(lines = V1)
}

read_to_matrix <- function(path = "./input.txt", as_nums = FALSE) {
  conv_fn <- ifelse(as_nums, as.numeric, as.character)
  read_lines(path) %>%
    mutate(lines = lapply(lines, split_full)) %>%
    pull(lines) %>%
    do.call(rbind, .) %>%
    apply(2, conv_fn)
}

################################################################################
############################### MATRIX FUNCTIONS ###############################
################################################################################

matget <- function(M, r, c, default = 0) {
  height <- nrow(M)
  width <- ncol(M)
  if (r %in% 1:height & c %in% 1:width & is.numeric(c(r, c))) {
    M[r, c]
  } else {
    default
  }
}
matassign <- function(M, r, c, val) {
  dims <- dim(M)
  if (r <= 0 || c <= 0 || r > dims[1] || c > dims[2]) {
    M
  }
  else {
    M[r, c] <- val
    M
  }
}

get_coords <- function(mat, n) {
  width <- dim(mat)[2]
  pos <- which(mat == n)
  row <- clamp_mod(pos, width)
  col <- ((pos-1) %/% width) + 1
  coords <- data.frame(row, col)
  coords
}

################################################################################
############################### STRING FUNCTIONS ###############################
################################################################################

split_full <- function(s) {
  s <- as.character(s)
  strsplit(s, split = "")[[1]]
}

trimsplit <- function(s, split) {
  s <- trimws(s)
  as.numeric(strsplit(s, split = split)[[1]])
}

heirarchy_split <- function(s,
                            split_top = ":",
                            split_low = ",",
                            trim = TRUE,
                            as_nums = FALSE) {
  if (trim) {
    s <- trimws(s)
  }
  split_by_top <- strsplit(s, split = split_top)[[1]]
  if (trim) {
    split_by_top <- trimws(split_by_top)
  }
  split_lower <- strsplit(split_by_top, split = split_low)
  if (as_nums) {
    split_lower <- lapply(split_lower, as.numeric)
  }
  names(split_lower) <- paste0("part", 1:length(split_lower))
  split_lower
}

################################################################################
################################ OTHER FUNCTIONS ###############################
################################################################################

# Clamp modulus values into range 1-> max
# e.g. clamp_mod(8,6) = 2, clamp_mod(12,6) = 6
clamp_mod <- function(n, max) {
  n <- n %% max
  n <- n + (n == 0)*max
  n
}
