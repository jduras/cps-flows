
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

# main directory
mdir.cps <- "C:/Drive/Dropbox/User/Academic/00Projects/03CPS/"
# directory with R code
sdir.cps <- paste0(mdir.cps, "code/")
# directory where CPS data AND flows_67.dta is located; monthly files must be extracted and named 'bYYYYMM'
ddir.cps <- "D:/Data/CPS/data/basic/unpacked/"
# directory for CPS extracts
edir.cps <- "D:/Data/CPS/extracts/flows/"
# directory for output
odir.cps <- paste0(mdir.cps, "output/")

# directory where source BLS data is located
ddir.bls <- "C:/Drive/Dropbox/Data/download.bls.gov/pub/time.series/ln/"
# directory for output
odir.bls <- "C:/Drive/Dropbox/Data/BLS/output/R/"

setwd(mdir.cps)

theme_set(theme_bw())

# first and last period to be used
tfirst <- 197601
tlast  <- 201806

# load NBER recession dates and functions to 1) include recessions bars in ggplot and dygraph, and 2) simple seasonal adjustment using Kalman filter
source(paste0(sdir.cps, "cXX_funs.R"))

# extract the data from the BLS
source(paste0(sdir.cps, "c01_extract_bls_data.R"))
# extract the data from the raw CPS files
source(paste0(sdir.cps, "c01_extract_cps_data.R"))
# match individuals in consecutive monthly files when possible
source(paste0(sdir.cps, "c02_join_2m.R"))
source(paste0(sdir.cps, "c03_join_3m.R"))
source(paste0(sdir.cps, "c04_join_4m.R"))
source(paste0(sdir.cps, "c05_join_2m_yoy.R"))

# merge data from all months into one dataset
source(paste0(sdir.cps, "c06_merge_1m.R"))

# merge matched individuals data from all months into one dataset
source(paste0(sdir.cps, "c07_merge_2m.R"))
source(paste0(sdir.cps, "c08_merge_2m_yoy.R"))

# construct flows and transition rates: overall
source(paste0(sdir.cps, "c_flows_occ.R"))

# construct flows and transition rates by occupation
source(paste0(sdir.cps, "c_flows_occ.R"))


# create the stocks data
source(paste0(sdir.cps, "c_stocks_occ.R"))
# create the flow data
source(paste0(sdir.cps, "c_flows_occ.R"))
# seasonally adjust using ratio-to-moving-average
# source(paste0(sdir.cps,"c_sa.R"))
# plot graphs
# source(paste0(sdir.cps,"c_graphs.R"))
# labor force
# source(paste0(sdir.cps,"c_lf.R"))


