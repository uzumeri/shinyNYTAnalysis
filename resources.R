library(tidyverse)
resources <- read_csv("Hospitalization_all_locs.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))
