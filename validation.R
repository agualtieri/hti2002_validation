rm(list = ls())

# libraries
library(tidyverse)
library(openxlsx)
library(cleaninginspectoR)
library(arsenal)
library(cluster)


source("./data_falsification.R")
source("./check_log.R")

# load analysis files
data <-"./input/HTI2002_Data_Cleaning_R7.xlsx"

sheets <- openxlsx::getSheetNames(data)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=data)
names(SheetList) <- sheets

clean <- SheetList[["Clean Data"]]
names(clean)[names(clean) == "_uuid"] <- "uuid"

clean_log <- SheetList[["Cleaning Log"]]
del <- SheetList[["Deletions"]]
names(del)[names(del) == "_uuid"] <- "uuid"

# check issues
issues <- inspect_all(clean) %>% filter(!is.na(index))

write.xlsx(issues, paste0("./output/cleaning_issues_",lubridate::today(),".xlsx"))

# check if cleaning log has been applied
clean_log <- clean_log %>% filter(question.name %in% names(clean))
clean_log <- clean_log %>% filter(uuid %in% clean$uuid)

clean.c <- check_log(data = clean, log = clean_log,
                     variable = "question.name",
                     old_log_var = "old.value", 
                     new_log_var = "new.value") 




clean.c <- clean.c %>% mutate(check = ifelse(new.value == value_extracted, "Log applied correctly", "Please check log")) %>%
  filter(check == "Please check log")

write.xlsx(clean.c, paste0("./output/cleaning process_",lubridate::today(),".xlsx"))

# check deletion
delc <- semi_join(del, clean, "uuid") ## tutti e due ok

# check falsification
tool <- SheetList[["Kobo survey"]]

similar.surveys <- calculateDifferences(clean, tool)
similar.surveys <- similar.surveys %>% filter(number.different.columns < 5)
write.xlsx(similar.surveys, paste0("./output/enumerator falsification_",lubridate::today(),".xlsx"))


