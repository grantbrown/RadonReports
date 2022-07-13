library(knitr)

knitr::write_bib(x = c("knitr", "jsonlite", "tigris", "viridis", "tidyverse", 
                         "tidycensus", "spdep", "nimble", "tmaptools"), 
                 file = "Bibliography.bib", prefix = "R-")
