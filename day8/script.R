################################################################################
################################ -- Options -- #################################
################################################################################

rm(list = ls())
setwd(dirname(rstudioapi::documentPath()))
source("../init.R")

################################################################################
################################# -- Part 1 -- #################################
################################################################################

locator <- function(positions, length, width) {
  out <- list()
  for (i in 1:length(positions)) {
    out[[i]] <- c( clamp_mod(positions[i], length), (positions[i]-1) %/% length + 1)
  }
  out
}

inp <- read_to_matrix("./input.txt")

lw <- dim(inp)
freqs <- setdiff(unique(as.character(inp)), ".")

anmap <- matrix(0, nrow = lw[1], ncol = lw[2])
for (freq in freqs) {
  locs <- which(inp == freq)
  locs <- locator(locs, lw[1])
  for (i in 1:length(locs)) {
    for (j in 1:length(locs)) {
      if (i == j) {
        break
      }
      pos1 <- locs[[i]]
      pos2 <- locs[[j]]
      dif <- pos1 - pos2
      an1 <- pos1 + dif
      an2 <- pos2 - dif
      anmap <- matassign(anmap, an1[1], an1[2], 1)
      anmap <- matassign(anmap, an2[1], an2[2], 1)
    }
  }
}

print(sum(anmap))

################################################################################
################################# -- Part 2 -- #################################
################################################################################

inp <- read_to_matrix("./input.txt")

lw <- dim(inp)
freqs <- setdiff(unique(as.character(inp)), ".")

anmap <- matrix(0, nrow = lw[1], ncol = lw[2])
for (freq in freqs) {
  locs <- which(inp == freq)
  locs <- locator(locs, lw[1])
  for (i in 1:length(locs)) {
    for (j in 1:length(locs)) {
      if (i == j) {
        break
      }
      pos1 <- locs[[i]]
      pos2 <- locs[[j]]
      dif <- pos1 - pos2
      #Any big number will do here
      iters <- min(lw / dif) + 100
      for (k in 0:iters) {
        l1 <- pos1 + k*dif
        l2 <- pos2 - k*dif
        anmap <- matassign(anmap, l1[1], l1[2], 1)
        anmap <- matassign(anmap, l2[1], l2[2], 1)
      }
    }
  }
}

print(sum(anmap))
