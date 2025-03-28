#############################
# this is a template source file
# please change all parameters accordingly
#############################
# make files readable for all
Sys.umask(mode = "0002") 

#############################
# R library and R packages
#############################
# used R versions: R >= 4.2.2

suppressPackageStartupMessages(library(data.table))
setDTthreads(1)
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(WriteXLS))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(biomaRt))
suppressPackageStartupMessages(library(Biobase))
suppressPackageStartupMessages(library(ivreg))
suppressPackageStartupMessages(library(pgenlibr))
suppressPackageStartupMessages(library(toolboxH))
suppressPackageStartupMessages(library(qqman))

#############################
# Other Tools 
#############################
# PLINK2 path
path_plink2 = "path/to/plink2.0/unix_64/plink2"
path_plink1.9 = "path/to/plink1.9/unix64/plink"

#############################
# path to LIFE data 
#############################
# all individual level data is not part of the repository!
#
# phenotype data
path_LIFEAdult = "path/to/pv785/data_adult/"
path_LIFEHeart = "path/to/pv785/data_heart/"

# genetic data
path_LIFEAdult_Genetics = "path/to/LIFE-Adult_TOPMed_minimal_filtered_chr1to23"
path_LIFEHeart_Genetics = "path/to/LIFE-Heart_TOPMed_minimal_filtered_chr1to23"

path_LIFEAdult_GeneticPCs = "path/to/LIFE-Adult/SamplesAsImputed.RData"
path_LIFEHeart_GeneticPCs = "path/to/Life-Heart/SamplesAsImputedB3.RData"

# results from data preparation
path_LIFEprepped = "path/to/dataQC/"
