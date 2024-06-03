# The function of this script file is to import and clean the Contact
# Precautions spread sheet from
# Data for saturation point analyses_2024_05_16.xlsx

# Loading libraries and functions
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Import data ###
contact_prec <- readxl::read_xlsx(
  path = "./data-raw/Data for saturation point analyses_2024_05_16.xlsx",
  sheet = "Contact Precautions",
  range = "A1:AS284"
)

# Export data ####
saveRDS(
  object = contact_prec,
  file = "./data/contact_precautions.rds"
)