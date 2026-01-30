# ==============================================================================
# PROJECT: Stimuli Panel Randomizer ####
# SECTION: GLOBAL CONFIGURATION & LIBRARIES
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: LIBRARY IMPORTS ####
# ------------------------------------------------------------------------------
if (!require("shiny",
             quietly = T)) install.packages("shiny")
if (!require("dplyr",
             quietly = T)) install.packages("dplyr")
if (!require("tidyr",
             quietly = T)) install.packages("tidyr")
if (!require("DT",
             quietly = T)) install.packages("DT")
if (!require("shinythemes",
             quietly = T)) install.packages("shinythemes")
if (!require("readxl",
             quietly = T)) install.packages("readxl")
if (!require("writexl",
             quietly = T)) install.packages("writexl") # Added for Excel export

library(shiny)
library(dplyr)
library(tidyr)
library(DT)
library(shinythemes)
library(readxl)
library(writexl)

# ------------------------------------------------------------------------------
# SECTION 2: GLOBAL VARIABLES & OPTIONS ####
# ------------------------------------------------------------------------------
options(shiny.maxRequestSize = 5 * 1024^2)