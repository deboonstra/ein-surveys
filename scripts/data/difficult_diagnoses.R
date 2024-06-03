# The function of this script file is to import and clean the Difficult
# Diagnoses spread sheet from Data for saturation point analyses_2024_05_16.xlsx

# Loading libraries and functions
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Import data ###
diff_diag <- readxl::read_xlsx(
  path = "./data-raw/Data for saturation point analyses_2024_05_16.xlsx",
  sheet = "Difficult Diagnoses",
  range = "A1:AC644"
)

# Export data ####
saveRDS(
  object = diff_diag,
  file = "./data/difficult_diagnoses.rds"
)
