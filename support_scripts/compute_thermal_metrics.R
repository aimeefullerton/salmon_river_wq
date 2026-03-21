# Setup ----
library(dplyr)
library(nhdplusTools)
library(sf)
library(tidyverse)

source("support_scripts/functions_metrics.R")
source("R/load_data.R")

# life stages ----
#lifestages <- read.csv("data/lifestages.csv")
species <- "Chinook"
lh = c("generic", "fall_outmigrants", "win_outmigrants", "spr_outmigrants")
lifestages <- lifestages[lifestages$Lifestage %in% c("prespawn", "incubat", "rearing"),]

# Empirical dataset
emp.data <- obs_temps[, c("Date", "SiteCode", "COMID", "AvgDailyTemp")]
emp.data <- as.data.frame(emp.data[!is.na(emp.data$AvgDailyTemp),])
year.range <- sort(unique(1900 + as.POSIXlt(emp.data$Date)$year))
emp.out <- fncComputeMetrics(stdata = emp.data, species = species, lh = lh, year.range = year.range, 
                             st.col = "AvgDailyTemp", site.col = "SiteCode", date.col = "Date")
emp.out <- unique(emp.out)
data.table::fwrite(emp.out, paste0("data/thermal_metrics_empirical.csv"))


