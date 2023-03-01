library(tidyverse)

project_dir <- "Z:/Shared/Projects/Biohaven/BV_220014 Zavegepant AMCP, ITC, & Post-hoc/"
dat_dir <- paste0(project_dir, "Indirect treatment comparison/")
file_name <- "ITC data extraction sheet nasal therapies combined v0.9 PJ SW.xlsx"


## Patient characteristics ####

### Read column names and types ####

dat_pc_raw <- readxl::read_xlsx(
  path = paste0(dat_dir, file_name),
  sheet = "Patient characte NMA only",
  skip = 3,
  n_max = 2,
  col_names = FALSE,
  na = c("--", "x", "_", "-")
) %>% 
  data.frame(
    stringsAsFactors = FALSE
  )

pc_names <- dat_pc_raw[1,]
pc_classes <- dat_pc_raw[2,]


## Read the data and assign names and classes ####

# PC <- data.frame(read_xlsx(paste0(dat_dir, file_name), sheet = "Patient characteristics", skip = 5, col_names = F, col_types = as.character(PC_classes)), stringsAsFactors = FALSE)
dat_pc_raw <- readxl::read_xlsx(
  path = paste0(dat_dir, file_name), 
  sheet = "Patient characte NMA only", 
  skip = 5, 
  na = c("--", "x", "_", "-"),
  col_types = as.character(pc_classes),
  col_names = as.character(pc_names)
)

# warnings() %>% 
#   names() %>% 
#   str_remove(
#     pattern = "Expecting "
#   ) %>% 
#   str_remove(
#     pattern = " got "
#   ) %>% 
#   str_remove_all(
#     pattern = "\'"
#   ) %>% 
#   str_replace(
#     pattern = " in ",
#     replacement = ":"
#   ) %>% 
#   str_split_fixed(
#     pattern = ":",
#     n = 3
#   ) %>% 
#   as_tibble() %>% 
#   mutate(
#     V2 = V2 %>% 
#       str_remove(
#         pattern = " / .*"
#       )
#   ) %>% 
#   select(
#     cell = V2,
#     expecting = V1,
#     current_value = V3
#   ) %>% 
#   write.table(
#     "clipboard",
#     sep = "\t",
#     row.names = FALSE
#   )

### Clean the data ####

dat_pc <- dat_pc_raw %>% 
  mutate(
    treatment = case_when(
      treatment == "Sumatriptan containing 1-O-n-dodecyl-β-D-maltopyranoside 0.2% (DFN-02)" ~ "Sumatriptan- DFN-02 0.20% DDM",
      TRUE ~ treatment
    )
  )

rm(
  dat_pc_raw,
  pc_names,
  pc_classes
)


## Clinical outcomes ####

### Read column names and types ####

dat_co_raw <- readxl::read_xlsx(
  path = paste0(dat_dir, file_name),
  sheet = "Clinical outcomes NMA only",
  skip = 1,
  n_max = 3,
  col_names = FALSE,
  na = c("--", "x", "_", "-")
) %>% 
  data.frame(
    stringsAsFactors = FALSE
  )

co_names <- dat_co_raw[2,]
co_classes <- dat_co_raw[1,]


## Read the data and assign names and classes ####

# PC <- data.frame(read_xlsx(paste0(dat_dir, file_name), sheet = "Patient characteristics", skip = 5, col_names = F, col_types = as.character(PC_classes)), stringsAsFactors = FALSE)
dat_co_raw <- readxl::read_xlsx(
  path = paste0(dat_dir, file_name), 
  sheet = "Clinical outcomes NMA only", 
  skip = 5, 
  na = c("--", "x", "_", "-"),
  col_types = as.character(co_classes),
  col_names = as.character(co_names)
)

# warnings() %>% 
#   names() %>% 
#   str_remove(
#     pattern = "Expecting "
#   ) %>% 
#   str_remove(
#     pattern = " got "
#   ) %>% 
#   str_remove_all(
#     pattern = "\'"
#   ) %>% 
#   str_replace(
#     pattern = " in ",
#     replacement = ":"
#   ) %>% 
#   str_split_fixed(
#     pattern = ":",
#     n = 3
#   ) %>% 
#   as_tibble() %>% 
#   mutate(
#     V2 = V2 %>% 
#       str_remove(
#         pattern = " / .*"
#       )
#   ) %>% 
#   select(
#     cell = V2,
#     expecting = V1,
#     current_value = V3
#   ) %>% 
#   write.table(
#     "clipboard",
#     sep = "\t",
#     row.names = FALSE
#   )

### Clean the data ####

dat_co <- dat_co_raw %>% 
  mutate(
    treatment = case_when(
      treatment == "Sumatriptan containing 1-O-n-dodecyl-β-D-maltopyranoside 0.2% (DFN-02)" ~ "Sumatriptan- DFN-02 0.20% DDM",
      TRUE ~ treatment
    )
  )

rm(
  dat_co_raw,
  co_names,
  co_classes
)



# This first part is to get the names and classes of the columns
CO <- data.frame(read_xlsx(paste0(dat_dir, file_name), sheet = "Clinical outcomes", skip = 3, col_names = F), stringsAsFactors = FALSE)
CO_names <- CO[1,]
CO_classes <- CO[2,]

# Then we reread in with the proper classes and define the names
CO <- data.frame(read_xlsx(paste0(dat_dir, file_name), sheet = "Clinical outcomes", skip = 5, col_names = F, col_types = as.character(CO_classes)), stringsAsFactors = FALSE)
names(CO) <- CO_names

#Remove column where name is 'delete'
CO <- CO[, names(CO) != "delete"]

#Prep full treatment variable
#Some incorrect spelling
CO$treatment[CO$treatment %in% c("Lasmiditin", "Lasmitidan", "lasmiditan")] <- "Lasmiditan"
CO$treatment[CO$treatment %in% c("rimegepant")] <- "Rimegepant"
CO$treatment[CO$treatment %in% c("ubrogepant")] <- "Ubrogepant"

#Fix doses
CO$dose <- gsub("mg", "", CO$dose)

CO <- CO %>% mutate(treatment = ifelse(treatment %in% c("Placebo", "--", NA), treatment, paste0(treatment, " ", dose)),
                    node = gsub("Lasmiditan", "LAS" , treatment),
                    node = gsub("Ubrogepant", "UBRO", node),
                    node = gsub("Rimegepant", "RIM" , node),
                    node = gsub("Placebo"   , "PBO" , node)) %>%
  filter(treatment %in% trts_of_int) #Subset only to nodes of interest
