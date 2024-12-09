################################################################################
################################ -- Options -- #################################
################################################################################

rm(list = ls())
setwd(dirname(rstudioapi::documentPath()))
source("../init.R")

#inp <- read_lines("./test.txt")
inp <- read_lines("./input.txt")

################################################################################
################################# -- Part 1 -- #################################
################################################################################

check_ops <- function(target, curr, nums) {
  l <- length(nums)
  if (l == 0) {
    target == curr
  } else if (l == 1) {
    if (target == curr + nums[1]) {
      TRUE
    } else if (target == curr * nums[1]) {
      TRUE
    } else {
      FALSE
    }
  } else {
    check_ops(target, curr * nums[1], nums[2:l]) ||
      check_ops(target,curr + nums[1], nums[2:l])
  }
}

result <- inp %>%
  mutate(s = strsplit(lines, split = ":")) %>% 
  rowwise() %>%
  mutate(target = as.numeric(s[[1]]),
         nums = list(trimsplit(s[[2]], split = " "))) %>%
  select(target, nums) %>%
  rowwise() %>% 
  mutate(check = check_ops(target, nums[1], nums[2:length(nums)])) %>% 
  filter(check) %>%
  pull(target) %>% 
  sum()
result  

################################################################################
################################# -- Part 2 -- #################################
################################################################################

num_concat <- function(x, y) {
  (x*10^(ceiling(log(y+1,10)))) + y
}

check_ops <- function(target, curr, nums) {
  l <- length(nums)
  if (l == 1) {
    if (target == curr + nums[1]) {
      TRUE
    } else if (target == curr * nums[1]) {
      TRUE
    } else if (target == num_concat(curr, nums[1])) {
      TRUE
    } else {
      FALSE
    }
  } else {
    check_ops(target, curr + nums[1], nums[2:l]) ||
      check_ops(target,curr * nums[1], nums[2:l]) ||
      check_ops(target, num_concat(curr, nums[1]), nums[2:l])
  }
}
result <- inp %>%
  mutate(s = strsplit(lines, split = ":")) %>%
  rowwise() %>%
  mutate(target = as.numeric(s[[1]]),
         nums = list(trimsplit(s[[2]], split = " "))) %>%
  select(target, nums) %>%
  rowwise() %>%
  mutate(check = check_ops(target, nums[1], nums[2:length(nums)])) %>%
  filter(check) %>%
  pull(target) %>%
  sum()
print(result)