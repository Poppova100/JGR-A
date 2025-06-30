# Load required libraries, install if missing
libs <- c('data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyr', 'dplyr', 'ggnewscale')
to_install <- setdiff(libs, rownames(installed.packages()))
if(length(to_install)) install.packages(to_install)
invisible(lapply(libs, require, character.only = TRUE))

# Load data
tab_JS <- readRDS("./data/tab_June_Sep_11.rds")
setDT(tab_JS)[, CT := CT2][, CT2 := NULL]  # Efficient column update/removal in one chain

tab_HW <- readRDS("./data/HW_TAB_11.rds")
setDT(tab_HW)  # Ensure data.table

klima <- readRDS("./data/klima.rds")

# Merge, drop columns, reorder, compute, round
TABLE_2 <- merge(tab_HW, tab_JS, by = "CT", sort = FALSE)[
  , c('pet_p_HWG', 'pet_p_HWL', 'pet_p_HWH', 'pet_p_HWO', 'pet_p_JS') := NULL][
    , setcolorder(.SD, c('CT', 'prob_HWG', 'Cef_HWG', 'tg_HWG', 'p_HWG', 
                         'prob_HWL', 'Cef_HWL', 'tg_HWL', 'p_HWL',
                         'prob_HWH', 'Cef_HWH', 'tg_HWH', 'p_HWH', 
                         'prob_HWO', 'Cef_HWO', 'tg_HWO', 'p_HWO', 
                         'prob_JS', 'tg_JS', 'p_JS'))][
                           , tg_JS := tg_JS - klima][
                             , lapply(.SD, function(x) if(is.numeric(x)) round(x, 2) else x)
                           ]

# #saveRDS(TABLE_2, "./data/table_2.rds")