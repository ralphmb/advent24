################################################################################
################################ -- Options -- #################################
################################################################################

rm(list = ls())
setwd(dirname(rstudioapi::documentPath()))
source("../init.R")

################################################################################
################################# -- Part 1 -- #################################
################################################################################

fill_rules <- function(rules1) {
  for (i in 1:length(rules)) {
    
  }
}

inp <- read_lines("test.txt", skip_blank = FALSE)

linebr <- which(inp$lines == "")

rules_raw <- inp[1:(linebr-1),]
updates <- inp[(linebr+1):nrow(inp),]

# fill_rules <- function(rules) {
#   print(rules)
#   #for (n in 1:length(names(rules))) {
#     for (key in names(rules)) {
#       for (other_key in names(rules)) {
#         print(paste0(key))
#         if (other_key %in% rules[[key]]) {
#           rules[[key]] <- c(rules[[key]], rules[[other_key]])
#         }
#       }
#     }
#   #}
# }

inp <- read_lines("./test.txt")

br <- which(inp$lines == "")
rules_r <- inp[1:(br-1),]
updates <- inp[(br+1):nrow(inp),] %>% 
  lapply(., \(x) strsplit(x, split = ","))


rules <- list()
all_parts <- c()
for (rule in rules_raw) {
  parts <- strsplit(rule, split = "\\|")[[1]]
  rules[[parts[1]]] <- c(rules[[parts[1]]], parts[2])
  all_parts <- unique(c(all_parts, parts))
}
for (check in updates) {
  seen <- c()
  for (curr in check) {
    seen <- c(seen, curr)
    
  }
}

################################################################################
################################# -- Part 2 -- #################################
################################################################################

order <- tibble(keys = names(rules), vals = rules) %>%
  rowwise() %>%
  mutate(l  = length(vals)) %>% 
  arrange(l) %>% 
  pull(keys)

res <- c()
for (check in updates){
  check <- check[[1]]
  last_ind <- 0
  out <- TRUE
  print(out)
  for (curr in check) {
    this_ind <- -which(order == curr)
    cond <- this_ind <= last_ind
    print(cond)
    out <- out & cond
    last_ind <- this_ind
  }
  res <- c(res, out)
}

#Test check
print(all(res == c(1,1,1,0,0,0)))
################################################################################
################################# -- Part 2 -- #################################
################################################################################


