library(tidyverse)
library(tabulizer)

extract_tables("~/Downloads/MWRAData20201203-data.pdf") -> tables

tables %>% keep(~ ncol(.x) == 9) %>% do.call(rbind, .) %>% data.frame()-> mat
names(mat) <- mat[1,] %>% unlist()
mwra_raw <- mat[-1, ] %>%
    as_tibble() %>%
    rename_all(.funs = list(str_squish))

mwra_data <- mwra_raw %>%
    transmute(sample_date = lubridate::mdy(`Sample Date`),
              southern_cpml = as.integer(`Southern (copies/mL)`),
              northern_cpml = as.integer(`Northern (copies/mL)`),
              southern_7day = as.integer(`Southern 7 day avg`),
              northern_7day = as.integer(`Northern 7 day avg`),
              southern_low  = as.integer(`Southern Low Confidence Interval`),
              northern_low  = as.integer(`Northern Low Confidence Interval`),
              southern_hi   = as.integer(`Southern High Confidence Interval`),
              northern_hi   = as.integer(`Northern High Confidence Interval`),
    )
