# ANALYSIS
rm(list = ls())

# libraries
library(tidyverse)
library(openxlsx)


# load analysis files
data <-"./input/REACH_HTI_2002_ICSM_Dataset_Analysis_R5_to HQ.xlsx"

sheets <- openxlsx::getSheetNames(data)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=data)
names(SheetList) <- sheets

data <- SheetList[[3]]

medians <- data %>% select("commune", ends_with("_prix")) %>% 
                    group_by(commune) %>%
                    summarise_all(funs(median(., na.rm = TRUE)))  
