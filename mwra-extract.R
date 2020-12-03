library(rvest)
library(tidyverse)
library(tabulizer)

mwra_web <- read_html("https://www.mwra.com/biobot/biobotdata.htm")
mwra_pdf_filename <- mwra_web %>%
    html_nodes("a") %>% html_attr("href") %>%
    .[grepl("MWRAData20[0-9]{6}-data.pdf", .)] %>%
    unique()
extract_tables(paste0("https://www.mwra.com/biobot/", mwra_pdf_filename) -> tables

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
    ) %>%
    mutate(southern_low = ifelse(is.na(southern_cpml), NA, southern_cpml - southern_low),
           southern_hi  = ifelse(is.na(southern_cpml), NA, southern_cpml + southern_hi),
           northern_low = ifelse(is.na(northern_cpml), NA, northern_cpml + northern_low),
           northern_hi  = ifelse(is.na(northern_cpml), NA, northern_cpml + northern_hi),
           )
