# Version 0.1
# Last updated 11/13/2019 by Daniel Philippus at Colorado School of Mines.
#
# Run Nasrin's SWMM calibration script
# Open RSWMM_Autocalibration.R as well, source on save and save.
# Can change pop size to change # of iterations
#
# The working directory should contain a SWMM input file (.inp) and an Observed.xlsx and
# ParametersBound.csv as well as the RSWMM_Autocalibration.R script.
# Also the two .dat files included.  Examples of the necessary files are included.
####################################################################
# 1 - Import libraries
####################################################################
library(ggplot2)
library(stringr)
library(readxl)
library(hydroGOF)
library(xlsx)
library(mco)
source("RSWMM_Autocalibration.R")

####################################################################
# 2 - Set up input parameters
####################################################################
envir<-.GlobalEnv

# Specify iType
#iType: SUBCATCH=0;  NODE     = 1;LINK     = 2; SYS      = 3;

iType <- 1

#Specify vIndex
# vIndex
#Code number of each node variable
#0 for depth of water above invert (ft or m),
#1 for hydraulic head (ft or m),
#2 for volume of stored + ponded water (ft3 or m3),
#3 for lateral inflow (flow units),
#4 for total inflow (lateral + upstream) (flow units),
#5 for flow lost to flooding (flow units),
#6 for concentration of first pollutant,
#...
#5 + N for concentration of N-th pollutant.
vIndex <- 4

# Specifiy nameInOutputFile - name of subcatchment, node, or link
nameInOutputFile <- "4"

# Provide the path for output file
OutFile <- "DR_upstream"

# Provide path for optimization file
SWMMOptFile <- "DR_Upstream_opt.inp"

# Provide path for csv containing parameter bounds
ParametersFile <- "ParametersBound.csv"

# Provide path for SWMM executable file
swmm <- "C:\\Program Files (x86)\\EPA SWMM 5.1.013\\swmm5"

# Set up code
iteration <- 1
Bounds <- ParametersBound(ParametersFile)
initial <- c(as.vector(Bounds["Initial"]))$Initial
lower <- c(as.vector(Bounds["Minimum"]))$Minimum
upper <- c(as.vector(Bounds["Maximum"]))$Maximum
StatParameters <- c("NashsutcliffeTimesMinus1",
                   "absBias",
                   "negativeRSquared")

####################################################################
# 3 - Run code
####################################################################
OptimizationFunction(SWMMOptFile,
                     OutFile,
                     swmm,
                     Timeseries,
                     StatParameters,
                     initial,
                     lower,
                     upper,
                     generations = 10,
                     popsize = 20)

####################################################################
# 4 - Process optimization data
####################################################################

# filelist <- list.files(pattern = "^Combination.*\\.xlsx$")
# files <-
#   lapply(filelist, read.xlsx, sheetName = "Sheet1", header = TRUE)
# files <- lapply(files, function(x)
#   x[-1])
# n <- length(data.frame(files)) / length(files)
# Combination <- Reduce(function(...)
#   merge(..., by = 1:n, all = T), files)
# write.xlsx(Combination, "Calibration Results.xlsx")

####################################################################
# 5 - Delete excel files
####################################################################
# Which directory?
mydir <- getwd()
# What phrase do you want contained in
# the files to be deleted?
deletephrase <- "Combinations"

# Look at directory
dir(mydir)
# Figure out which files should be deleted
id <- grep(deletephrase, dir(mydir))
# Get the full path of the files to be deleted
todelete <- dir(mydir, full.names = TRUE)[id]
# DELETE
#unlink(todelete)
