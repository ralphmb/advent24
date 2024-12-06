library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

options(digits=16)


read_lines <- function(path = "./input.txt", skip_blank = FALSE) {
  read.delim(path, header = FALSE, blank.lines.skip = skip_blank) %>%
    rename(lines = V1)
}

read_to_matrix <- function(path = "./input.txt", type = "character") {
  conv_fn <- ifelse(type == "numeric", as.numeric, as.character)
  read_lines(path) %>% 
    mutate(lines = lapply(lines, split_full)) %>%
    pull(lines) %>%
    do.call(rbind, .) %>%
    apply(2, conv_fn)
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

trimsplit <- function(s, split) {
  s <- trimws(s)
  as.numeric(strsplit(s, split = split )[[1]])
}

