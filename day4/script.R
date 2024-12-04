################################################################################
################################ -- Options -- #################################
################################################################################

rm(list = ls())
setwd(dirname(rstudioapi::documentPath()))
source("../init.R")

# inp <- read_lines("./test.txt")
inp <- read_lines("./input.txt")

################################################################################
################################# -- Part 1 -- #################################
################################################################################
iter_str <- function(M) {
  indices <- list()
  counter <- 0
  for (i in 1:(nrow(M))) {
    for (j in 1:(ncol(M)-3)) {
      subs <- paste0(M[i,j], M[i,j+1], M[i,j+2], M[i,j+3])
      if (subs == "XMAS") {
        counter <- counter + 1
        indices[[length(indices) + 1]] <- c(i,j)
      }
    }
  }
  list(indices = indices, count = counter)
}

iter_diag <- function(M) {
  indices <- list()
  counter <- 0
  for (i in 1:(nrow(M)-3)) {
    for (j in 1:(ncol(M)-3)) {
      subs <- paste0(M[i,j], M[i+1,j+1], M[i+2,j+2], M[i+3,j+3])
      if (subs == "XMAS") {
        counter <- counter + 1
        indices[[length(indices) + 1]] <- c(i,j)
      }
    }
  }
  list(indices = indices, count = counter)
}

mat <- inp %>% 
  mutate(lines = lapply(lines, split_full)) %>%
  pull(lines) %>%
  do.call(rbind, .)


w <- ncol(mat)

mats1 <- list(mat ,t(mat), mat[,w:1], t(mat[w:1,w:1]))
mats2 <- list(mat ,mat[,w:1], mat[w:1,], mat[w:1,w:1])

results1 <- lapply(mats1, iter_str)
results2 <- lapply(mats2, iter_diag)
ans <- sum(map_dbl(results1, ~ .x[[2]])) + sum(map_dbl(results2, ~ .x[[2]]))
print(ans)

################################################################################
################################# -- Part 2 -- #################################
################################################################################

iterr <- function(M) {
  indices <- list()
  counter <- 0
  for (i in 1:(nrow(M)-2)) {
    for (j in 1:(ncol(M)-2)) {
      subs1 <- paste0(M[i,j], M[i+1,j+1], M[i+2,j+2])
      subs2 <- paste0(M[i+2,j], M[i+1,j+1], M[i,j+2])
      if (subs1 == "MAS" & subs2 == "MAS") {
        counter <- counter + 1
        indices[[length(indices) + 1]] <- c(i,j)
      }
    }
  }
  list(indices = indices, count = counter)
}

mat <- inp %>% 
  mutate(lines = lapply(lines, split_full)) %>%
  pull(lines) %>%
  do.call(rbind, .)


w <- ncol(mat)

mats <- list(mat ,t(mat), mat[,w:1], t(mat[w:1,w:1]))

results1 <- lapply(mats, iterr)

ans <- sum(map_dbl(results1, ~ .x[[2]]))

print(ans)

