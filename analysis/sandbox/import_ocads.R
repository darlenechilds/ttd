##############
#BETTER WAY?


source("R/read_ocads.r")
source("R/read_ocads_file.R")
source("R/read_all_ocads")
source("R/select_variables.R")
source("R/variable_lists.R")

d <- read.csv("data/raw/ocads_urls.csv")
urls <- d$url

ocads <- read_all_ocads(urls)

tracers  <- select_variables(ocads, tracer_vars)
oxygen   <- select_variables(ocads, oxygen_vars)
nutrients <- select_variables(ocads, nutrient_vars)

