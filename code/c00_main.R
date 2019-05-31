
library(magrittr)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(broom)
library(tibble)
library(tidyquant)
library(timetk)
library(lubridate)
library(zoo)
library(xts)
library(tis)
library(KFAS)
library(ggplot2)
library(plotly)
library(dygraphs)
library(scales)
library(Hmisc)
library(questionr)

# library(conflicted)

# conflict_scout(pkgs = NULL)

# main directory
mdir_cps <- "C:/Drive/Dropbox/User/Academic/00Projects/03CPS/"
# directory with R code
sdir_cps <- paste0(mdir_cps, "code/")
# directory where CPS data is located; monthly files must be extracted and named 'bYYYYMM'
ddir_cps <- "D:/Data/CPS/data/basic/unpacked/"
# directory for CPS extracts
edir_cps <- "D:/Data/CPS/extracts/flows/"
# directory for output
odir_cps <- paste0(mdir_cps, "output/")

# directory where source BLS data is located
ddir_bls <- "C:/Drive/Dropbox/Data/download.bls.gov/pub/time.series/ln/"
# directory for output
odir_bls <- "C:/Drive/Dropbox/Data/BLS/output/R/"

setwd(mdir_cps)

theme_set(theme_bw())

# first and last period to be used
tfirst <- 197601
tlast  <- 201903

# load NBER recession dates and functions to 1) include recessions bars in ggplot and dygraph, and 2) simple seasonal adjustment using Kalman filter
source(paste0(sdir_cps, "cXX_funs.R"))
source(paste0(sdir_cps, "cXX_oaxacablinder.R"))

# extract the data from the BLS
source(paste0(sdir_cps, "c01_extract_bls_data.R"))
# extract the data from the raw CPS files
source(paste0(sdir_cps, "c02_extract_cps_data.R"))
# match individuals in consecutive monthly files when possible
source(paste0(sdir_cps, "c03_join_2m.R"))
# source(paste0(sdir_cps, "c04_join_3m.R"))
# source(paste0(sdir_cps, "c05_join_4m.R"))
# source(paste0(sdir_cps, "c06_join_2m_yoy.R"))

# merge data from all months into one dataset
source(paste0(sdir_cps, "c07_merge_1m.R"))

# merge matched individuals data from all months into one dataset
source(paste0(sdir_cps, "c08_merge_2m.R"))
# source(paste0(sdir_cps, "c09_merge_2m_yoy.R"))


#### aggregate ####

# construct stocks and flows
source(paste0(sdir_cps, "c11_construct_stocks_agg.R"))
source(paste0(sdir_cps, "c12_construct_flows_2m_agg.R"))

# construct population shares - counterfactual scenarios
source(paste0(sdir_cps, "c13_construct_rates_2m_agg_counter.R"))
source(paste0(sdir_cps, "c14_construct_shares_agg_lom.R"))

# estimate linear probability models, perform Oaxaca-Blinder decomposition
source(paste0(sdir_cps, "c15_estimate_lpm_agg.R"))
source(paste0(sdir_cps, "c16_estimate_lpm_agg_oaxaca.R"))


#### occupation specific ####

# construct stocks and flows
source(paste0(sdir_cps, "c21_construct_stocks_occ.R"))
source(paste0(sdir_cps, "c22_construct_flows_2m_occ.R"))

# construct population shares - counterfactual scenarios
source(paste0(sdir_cps, "c23_construct_rates_2m_occ_counter.R"))
source(paste0(sdir_cps, "c24_construct_shares_occ_lom.R"))

# estimate linear probability models, perform Oaxaca-Blinder decomposition
source(paste0(sdir_cps, "c25_estimate_lpm_occ.R"))
source(paste0(sdir_cps, "c26_estimate_lpm_occ_oaxaca.R"))
