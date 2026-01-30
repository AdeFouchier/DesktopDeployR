# ==============================================================================
# PROJECT: Stimuli Panel Randomizer ####
# SECTION: GLOBAL CONFIGURATION & LIBRARIES
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: LIBRARY IMPORTS ####
# ------------------------------------------------------------------------------
for(package in c("shiny",
                 "dplyr",
                 "tidyr",
                 "DT",
                 "shinythemes",
                 "readxl",
                 "writexl")){
  if (!require(package,
               character.only = TRUE,
               quietly = T)) install.packages(package)  
  library(package,
          character.only = TRUE)
}
rm(package)

# ------------------------------------------------------------------------------
# SECTION 2: GLOBAL VARIABLES & OPTIONS ####
# ------------------------------------------------------------------------------
options(shiny.maxRequestSize = 5 * 1024^2)
options(stringsAsFactors = FALSE)