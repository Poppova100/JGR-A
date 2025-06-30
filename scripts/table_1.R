# Keep only the required packages
required_pkgs <- c('data.table')
to_install <- required_pkgs[!required_pkgs %in% installed.packages()[,'Package']]
if(length(to_install)) install.packages(to_install)
lapply(required_pkgs, require, character.only = TRUE)

# Load data as data.tables directly
tab_JS <- readRDS("./data/tab_June_Sep_27.rds")
tab_HW <- readRDS("./data/HW_TAB_27.rds")
klima <- readRDS("./data/klima.rds")

# Ensure data.table class (if not already)
setDT(tab_JS)
setDT(tab_HW)

# Join tables
TABLE_1 <- tab_HW[tab_JS, on = "CT"]

# Drop unwanted columns (more efficient in data.table)
cols_to_drop <- c('pet_p_HWG', 'pet_p_HWL', 'pet_p_HWH', 'pet_p_HWO', 'pet_p_JS')
TABLE_1[, (cols_to_drop) := NULL]

# Set column order (as in your script)
setcolorder(TABLE_1, c('CT', 'prob_HWG', 'Cef_HWG', 'tg_HWG', 'p_HWG', 'prob_HWL', 'Cef_HWL', 'tg_HWL', 'p_HWL',
                       'prob_HWH', 'Cef_HWH', 'tg_HWH', 'p_HWH', 'prob_HWO', 'Cef_HWO', 'tg_HWO', 'p_HWO', 'prob_JS', 'tg_JS', 'p_JS'))

# Subtract klima from tg_JS column
TABLE_1[, tg_JS := tg_JS - klima]

# Round all numeric columns to 2 decimals (data.table way)
num_cols <- names(TABLE_1)[sapply(TABLE_1, is.numeric)]
TABLE_1[, (num_cols) := lapply(.SD, round, 2), .SDcols = num_cols]

# #saveRDS(TABLE_1, "./data/table_1.rds")