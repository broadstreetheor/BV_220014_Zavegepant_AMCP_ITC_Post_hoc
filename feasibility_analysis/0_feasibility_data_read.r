library(tidyverse)

project_dir <- "Z:/Shared/Projects/Biohaven/BV_220014 Zavegepant AMCP, ITC, & Post-hoc/"
dat_dir <- paste0(project_dir, "Indirect treatment comparison/")
file_name <- "ITC data extraction sheet nasal therapies combined v0.11 SW2 PJ.xlsx"


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
    # node = case_when(
    #   
    # )
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
co_classes <- dat_co_raw[1,] %>% 
  str_replace(
    pattern = "character",
    replacement = "text"
  )


## Read the data and assign names and classes ####

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
  select(
    !starts_with("delete")
  )

rm(
  dat_co_raw,
  co_names,
  co_classes
)


## Headache pain free at 60 minutes
hpf60 <- dat_co %>% 
  select(
    trial = trial_ID,
    treatment = Treatment,
    N = N_HPF60,
    hpf60_r = r_HPF60,
    hpf60_p = p_HPF60
  ) %>% 
  as.data.frame() %>% 
  Calc_np(an_only = FALSE)

## Headache pain free at 90 minutes
hpf90 <- dat_co %>% 
  select(
    trial = trial_ID,
    treatment = Treatment,
    N = N_HPF90,
    hpf90_r = r_HPF90,
    hpf90_p = p_HPF90
  ) %>% 
  as.data.frame() %>% 
  Calc_np(an_only = FALSE)

## Headache pain free at 2 hour
hpf2hr <- dat_co %>% 
  select(
    trial = trial_ID,
    treatment = Treatment,
    N = N_HPF2hr,
    hpf2hr_r = r_HPF2hr,
    hpf2hr_p = p_HPF2hr
  ) %>% 
  as.data.frame() %>% 
  Calc_np(an_only = FALSE)

## Headache pain free at 24 hour
hpf24hr <- dat_co %>% 
  select(
    trial = trial_ID,
    treatment = Treatment,
    N = `N_sustainPRelf2-24hr`,
    hpf24hr_r = `r_sustainPRelf2-24hr`,
    hpf24hr_p = `p_sustainPRelf2-24hr`
  ) %>% 
  as.data.frame() %>% 
  Calc_np(an_only = FALSE)


## Headache pain free at 48 hour
hpf48hr <- dat_co %>% 
  select(
    trial = trial_ID,
    treatment = Treatment,
    N = `N_sustainPRelf2-24hr`,
    hpf48hr_r = `r_sustainPRelf2-48hr`,
    hpf48hr_p = `p_sustainPRelf2-48hr`
  ) %>% 
  as.data.frame() %>% 
  Calc_np(an_only = FALSE)


CO_clean <- cbind(
  data.frame(Trial = dat_co$trial_ID, Treatment = dat_co$Treatment, stringsAsFactors = FALSE),
  hpf60       %>% select(contains("_clean")),
  hpf90       %>% select(contains("_clean")),
  hpf2hr      %>% select(contains("_clean")),
  hpf24hr     %>% select(contains("_clean")),
  hpf48hr     %>% select(contains("_clean"))
  # hpr60       %>% select(contains("_clean")),
  # hpr90       %>% select(contains("_clean")),
  # hpr2hr      %>% select(contains("_clean")),
  # hpr24hr     %>% select(contains("_clean")),
  # hpr48hr     %>% select(contains("_clean")),
  # ffphot2hr   %>% select(contains("_clean")),
  # ffphon2hr   %>% select(contains("_clean")),
  # ffnau2hr    %>% select(contains("_clean")),
  # ffvom2hr    %>% select(contains("_clean")),
  # ffmbs90     %>% select(contains("_clean")),
  # ffmbs2hr    %>% select(contains("_clean")),
  # painrel48hr %>% select(contains("_clean")),
  # afn60       %>% select(contains("_clean")),
  # afn90       %>% select(contains("_clean")),
  # afn2hr      %>% select(contains("_clean")),
  # afn24hr     %>% select(contains("_clean")),
  # afn48hr     %>% select(contains("_clean")),
  # resc        %>% select(contains("_clean")),
  # cr          %>% select(contains("_clean")),
  # pr          %>% select(contains("_clean")),
  # subr        %>% select(contains("_clean"))
) %>%
  rename_with(~gsub("_clean", "", .x))

CO_names <- data.frame(rbind(
  c("hpf60"      , "Pain freedom 60 mins"),
  c("hpf90"      , "Pain freedom 90 mins"),
  c("hpf2hr"     , "Pain freedom 2 hrs"),
  c("hpf24hr"    , "Pain freedom 24 hrs"),
  c("hpf48hr"    , "Pain freedom 48 hrs")
  # c("hpr60"      , "Pain relief 60 mins"),
  # c("hpr90"      , "Pain relief 90 mins"),
  # c("hpr2hr"     , "Pain relief 2 hrs"),
  # c("hpr24hr"    , "Pain relief 24 hrs"),
  # c("hpr48hr"    , "Pain relief 48 hrs"),
  # c("ffphot2hr"  , "FF photophobia 2 hrs"),
  # c("ffphon2hr"  , "FF phonophobia 2 hrs"),
  # c("ffnau2hr"   , "FF nausea 2 hrs"),
  # c("ffvom2hr"   , "FF vomiting 2 hrs"),
  # c("ffmbs90"    , "FF MBS 90 mins"),
  # c("ffmbs2hr"   , "FF MBS 2 hrs"),
  # c("painrel48hr", "Pain relapse 48 hrs"),
  # c("afn60"      , "Ab to func 60 mins"),
  # c("afn90"      , "Ab to func 90 mins"),
  # c("afn2hr"     , "Ab to func 2 hrs"),
  # c("afn24hr"    , "Ab to func 24 hrs"),
  # c("afn48hr"    , "Ab to func 48 hrs"),
  # c("resc"       , "Rescue medication 24 hrs"),
  # c("cr"         , "Complete response"),
  # c("pr"         , "Partial response"),
  # c("subr"       , "Suboptimal response")
))
names(CO_names) <- c("var", "name")

for(i in 1:dim(CO_names)[1]){
  names(CO_clean)[names(CO_clean) == CO_names$var[i]] <- CO_names$name[i]
}

CO_clean <- CO_clean %>%
  mutate(Trial = gsub(" \\(.*", "", Trial),
         Treatment = ifelse(Treatment == "PBO", "Placebo", Treatment)) %>% 
  filter(
    str_detect(
      Trial,
      pattern = "^Djupesland",
      negate = TRUE
    ) & 
      !Trial %in% c("DiSerio 1989", "Tepper 2015")
  )



