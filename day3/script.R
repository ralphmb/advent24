library(dplyr)
library(stringr)

rm(list = ls())
setwd("~/Documents/coding/advent24/day3")
inp <- read.delim("./input.txt", header = FALSE)

inp <- paste(inp)

# Part 1 

splitter <- function(s) {
  parts <- strsplit(s, split = "\\(")[[1]][2]
  parts <- strsplit(parts, split = "\\)")[[1]]
  parts <- strsplit(parts, split = ",")[[1]]
  parts <- as.numeric(parts)
  parts[1] * parts[2]
}

patt <- "mul\\([\\d]+,[\\d]+\\)"

t <- str_extract_all(inp, patt)

t <- t[[1]]

sum(sapply(t, splitter))

# Part 2 

parts <- strsplit(inp, split = "do\\(\\)")[[1]]
parts2 <- sapply(parts, \(x) strsplit(x, split = "don't\\(\\)")[[1]][1])

muls <- unlist(sapply(parts2, \(x) str_extract_all(x, patt)))

sum(sapply(muls, splitter))
