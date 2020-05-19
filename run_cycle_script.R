suppressPackageStartupMessages(library(spaMM))
suppressPackageStartupMessages(library(sf))
library(geojsonio)
library(httr)
library(jsonlite)
library(origami)
library(parallel)
library(ranger)
library(caret)
library(RANN)

# Run adaptive sampling experiment
run_cycle <- dget("adaptive_sampling_routine.R")

# Test
res <- run_cycle("HTI", 50, 1)

# Generate list to run function on in parallel
# n_sim <- 4
# batch_size=c(rep(50,n_sim*4), rep(10, n_sim*4), rep(5, n_sim*4))
# iso3=rep(c("PHL", "HTI", "CIV", "MWI"), n_sim*3)
set.seed(1981)
seed = as.list(sample(1:100000, 50, replace = FALSE))

# Build input list
# runs_list <- list()
# for(i in 1:length(iso3)){
#   runs_list[[i]] <- c(iso3[i],
#                               batch_size[i],
#                       seed[i])
# }

mwi_res_1 <- mclapply(seed, FUN=function(x){run_cycle("MWI", 1, seed = x)},
                       mc.cores = 7)
civ_res_1 <- mclapply(seed, FUN=function(x){run_cycle("CIV", 1, seed = x)},
                       mc.cores = 7)
hti_res_1 <- mclapply(seed, FUN=function(x){run_cycle("HTI", 1, seed = x)},
                       mc.cores = 7)
phl_res_1 <- mclapply(seed, FUN=function(x){run_cycle("PHL", 1, seed = x)},
                       mc.cores = 7)

mwi_res_10 <- mclapply(seed, FUN=function(x){run_cycle("MWI", 10, seed = x)},
         mc.cores = 7)
civ_res_10 <- mclapply(seed, FUN=function(x){run_cycle("CIV", 10, seed = x)},
                       mc.cores = 7)
hti_res_10 <- mclapply(seed, FUN=function(x){run_cycle("HTI", 10, seed = x)},
                       mc.cores = 7)
phl_res_10 <- mclapply(seed, FUN=function(x){run_cycle("PHL", 10, seed = x)},
                       mc.cores = 7)

mwi_res_50 <- mclapply(seed, FUN=function(x){run_cycle("MWI", 50, seed = x)},
                       mc.cores = 7)
civ_res_50 <- mclapply(seed, FUN=function(x){run_cycle("CIV", 50, seed = x)},
                       mc.cores = 7)
hti_res_50 <- mclapply(seed, FUN=function(x){run_cycle("HTI", 50, seed = x)},
                       mc.cores = 7)
phl_res_50 <- mclapply(seed, FUN=function(x){run_cycle("PHL", 50, seed = x)},
                       mc.cores = 7)


# Save
time_stamp <- gsub(":", "-", gsub(" ", "-", Sys.time(), fixed = TRUE))
save(list=ls(), file=paste0("outputs/results-", time_stamp, ".RData"))




