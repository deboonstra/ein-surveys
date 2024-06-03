# The function of this script file is to import and clean the Brain Abscess
# spread sheet from Data for saturation point analyses_2024_05_16.xlsx

# Loading libraries and functions
R <- list.files(path = "./R", pattern = "*.R", full.names = TRUE)
sapply(R, source, .GlobalEnv)

# Import data ###
brain <- readxl::read_xlsx(
  path = "./data-raw/Data for saturation point analyses_2024_05_16.xlsx",
  sheet = "Brain Abscess",
  range = "A1:Z552"
)

# Export data ####
saveRDS(
  object = brain,
  file = "./data/brain_abscess.rds"
)