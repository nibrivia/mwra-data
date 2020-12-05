suppressPackageStartupMessages({
    library(rvest)
    library(tidyverse)
    library(tabulizer)
})

mwra_web <- read_html("https://www.mwra.com/biobot/biobotdata.htm")
mwra_pdf_filename <- mwra_web %>%
    html_nodes("a") %>% html_attr("href") %>%
    .[grepl("MWRAData20[0-9]{6}-data.pdf", .)] %>%
    unique()

if (!file.exists(mwra_pdf_filename)) {
    mwra_pdf_url <- paste0("https://www.mwra.com/biobot/", mwra_pdf_filename)
    download.file(mwra_pdf_url, mwra_pdf_filename)
}

tables <- extract_tables(mwra_pdf_filename)

mat <- tables %>% keep(~ ncol(.x) == 9) %>% do.call(rbind, .) %>% data.frame()
names(mat) <- mat[1,] %>% unlist()
mwra_raw <- mat[-1, ] %>%
    as_tibble() %>%
    rename_all(.funs = list(str_squish)) %>%
    mutate_all(.funs = list(as.character))

mwra_data <- mwra_raw %>%
    transmute(sample_date = lubridate::mdy(`Sample Date`),
              southern_cpml = as.integer(`Southern (copies/mL)`),
              northern_cpml = as.integer(`Northern (copies/mL)`),
              southern_7day = as.integer(`Southern 7 day avg`),
              northern_7day = as.integer(`Northern 7 day avg`),
              southern_low  = as.integer(`Southern Low Confidence Interval`),
              northern_low  = as.integer(`Northern Low Confidence Interval`),
              southern_hi   = as.integer(`Southern High Confidence Interval`),
              northern_hi   = as.integer(`Northern High Confidence Interval`)
    ) %>%
    mutate(southern_low = ifelse(is.na(southern_cpml), NA, southern_cpml - southern_low),
           southern_hi  = ifelse(is.na(southern_cpml), NA, southern_cpml + southern_hi),
           northern_low = ifelse(is.na(northern_cpml), NA, northern_cpml - northern_low),
           northern_hi  = ifelse(is.na(northern_cpml), NA, northern_cpml + northern_hi)
           )

write_csv(mwra_data, "mwra-data.csv")

data_date <- mwra_data %>%
    filter(!is.na(northern_cpml)) %>%
    .$sample_date %>%
    max()

require(hrbrthemes)
plot <- mwra_data %>%
    filter(sample_date <= data_date) %>%
    ggplot(aes(x = sample_date,
               y = northern_cpml,
               ymin = northern_low,
               ymax = northern_hi)) +
    geom_pointrange(size = .2) +
    hrbrthemes::scale_y_comma() +
    scale_x_date(breaks = "1 month", date_labels = "%m/%d/%y",
                 minor_breaks = "1 week") +
    theme_ipsum_rc() +
    labs(title = "Viral load in North MWRA system",
         subtitle = paste("Data as of", format.Date(data_date, "%a %b %e, %Y")),
         x = NULL, y = "copies/ml",
         caption = "Olivia Brode-Roger")

ggsave(plot,
       filename = "mwra-north.png",
       width = 10, height = 7, units = "in",
       dpi = 100)
