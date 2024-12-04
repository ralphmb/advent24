library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

read_lines <- function(path = "./input.txt") {
  read.delim(path, header = FALSE) %>%
    rename(lines = V1)
}

m_get <- function(M, i, j, default = 0) {
  height <- nrow(M)
  width <- ncol(M)
  if (i %in% 1:height & j %in% 1:width & is.numeric(c(i, j))) {
    M[i, j]
  } else {
    default
  }
}

split_full <- function(s) {
  strsplit(s, split = "")[[1]]
}
