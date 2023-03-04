
library(tidyverse)

project_dir <- "Z:/Shared/Projects/Biohaven/BV_220014 Zavegepant AMCP, ITC, & Post-hoc/"
code_dir <- paste0(project_dir, "Analysis/R code/BV_220014_Zavegepant_AMCP_ITC_Post_hoc/feasibility_analysis/")
out_dir <- paste0(project_dir, "Indirect treatment comparison/feasibility assesment/")

## Read Evan's functions
source("Z:/Shared/Projects/Pfizer/PF_220163 Acute model updates, SLR, and NMA/SLR & NMA/Analysis/R code/00_functions.R")


## Read data ####

source(paste0(code_dir, "0_feasibility_data_read.r"))


trts_of_int <- c("Placebo", "Rimegepant 75", "Ubrogepant 25", "Ubrogepant 50", "Ubrogepant 100", "Lasmiditan 50", "Lasmiditan 100", "Lasmiditan 200")
tri_ord <- c("BHV3000-301 (Study 301)", "BHV3000-302 (Study 302)", "BHV3000-303 (Study 303)", "BHV3000-310 (Study 310)", "Marcus 2014", #RIM
             "CENTURION", "MONONOFU", "SAMURAI", "SPARTAN", "COL MIG-202", #LAS (COL MIG-202 = Farkkila et al 2012)
             "ACHIEVE I", "ACHIEVE II", "MK-1602-006") #UBRO (MK-1602-006 = Voss et al 2016)
tri_ord <- data.frame(trial = tri_ord, Order = 1:length(tri_ord), seperate = "No", stringsAsFactors = F)


## Patient characteristics ####

### Check data ####

#### Treatment ####
dat_pc %>% 
  count(
    treatment
  )

dat_pc %>% 
  filter(
    is.na(treatment)
  )

PC_clean %>% 
  count(
    treatment
  ) %>% 
  print(
    n = 100
  )


#### Rescue therapy ####

PC_clean %>% 
  select(
    id,
    resc_pop_n,
    resc_n,
    resc_p
  ) %>% 
  mutate(
    resc_p_cal = round(resc_n*100/resc_pop_n, digits = 1)
  ) %>% 
  filter(
    resc_p != resc_p_cal
  )



#### Sex ####

male_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    n,
    male_r,
    male_p
  ) %>% 
  mutate(
    male_p = round(male_p, digits = 0),
    male_p_cal = round(male_r*100/n, digits = 0)
  ) %>% 
  filter(
    (male_p != male_p_cal) | 
      (!is.na(male_p_cal) & is.na(male_p))
  )

female_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    n,
    female_r,
    female_p
  ) %>% 
  mutate(
    female_p = round(female_p, digits = 0),
    female_p_cal = round(female_r*100/n, digits = 0)
  ) %>% 
  filter(
    (female_p != female_p_cal) | 
      (!is.na(female_p_cal) & is.na(female_p))
  )


#### Race ####

white_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    n,
    white_r,
    white_p
  ) %>% 
  mutate(
    white_p = round(white_p, digits = 0),
    white_p_cal = round(white_r*100/n, digits = 0)
  ) %>% 
  filter(
    (white_p != white_p_cal) | 
      (!is.na(white_p_cal) & is.na(white_p))
  )

black_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    n,
    black_r,
    black_p
  ) %>% 
  mutate(
    black_p = round(black_p, digits = 0),
    black_p_cal = round(black_r*100/n, digits = 0)
  ) %>% 
  filter(
    (black_p != black_p_cal) | 
      (!is.na(black_p_cal) & is.na(black_p))
  )

asian_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    n,
    asian_r,
    asian_p
  ) %>% 
  mutate(
    asian_p = round(asian_p, digits = 0),
    asian_p_cal = round(asian_r*100/n, digits = 0)
  ) %>% 
  filter(
    (asian_p != asian_p_cal) | 
      (!is.na(asian_p_cal) & is.na(asian_p))
  )

raceother_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    n,
    raceother_r,
    raceother_p
  ) %>% 
  mutate(
    raceother_p = round(raceother_p, digits = 0),
    raceother_p_cal = round(raceother_r*100/n, digits = 0)
  ) %>% 
  filter(
    (raceother_p != raceother_p_cal) | 
      (!is.na(raceother_p_cal) & is.na(raceother_p))
  )


#### History of aura ####

aura_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    n,
    aura_r,
    aura_p
  ) %>% 
  mutate(
    aura_p = round(aura_p, digits = 0),
    aura_p_cal = round(aura_r*100/n, digits = 0)
  ) %>% 
  filter(
    (aura_p != aura_p_cal) | 
      (!is.na(aura_p_cal) & is.na(aura_p))
  )



#### Migraine characteristics ####

migraine_severe_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    migrane_n,
    severe_r,
    severe_p
  ) %>% 
  mutate(
    severe_p = round(severe_p, digits = 0),
    severe_p_cal = round(severe_r*100/migrane_n, digits = 0)
  ) %>% 
  filter(
    (severe_p != severe_p_cal) | 
      (!is.na(severe_p_cal) & is.na(severe_p))
  )

migraine_moderate_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    migrane_n,
    moderate_r,
    moderate_p
  ) %>% 
  mutate(
    moderate_p = round(moderate_p, digits = 0),
    moderate_p_cal = round(moderate_r*100/migrane_n, digits = 0)
  ) %>% 
  filter(
    (moderate_p != moderate_p_cal) | 
      (!is.na(moderate_p_cal) & is.na(moderate_p))
  )


migraine_mild_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    migrane_n,
    mild_r,
    mild_p
  ) %>% 
  mutate(
    mild_p = round(mild_p, digits = 0),
    mild_p_cal = round(mild_r*100/migrane_n, digits = 0)
  ) %>% 
  filter(
    (mild_p != mild_p_cal) | 
      (!is.na(mild_p_cal) & is.na(mild_p))
  )



#### Refractory to treatment ####

refract_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    n,
    refract_r,
    refract_p
  ) %>% 
  mutate(
    refract_p = round(refract_p, digits = 0),
    refract_p_cal = round(refract_r*100/n, digits = 0)
  ) %>% 
  filter(
    (refract_p != refract_p_cal) |
      (!is.na(refract_p_cal) & is.na(refract_p))
  )


#### Comorbid conditions ####

como_var_names <- c(
  "cardio",
  "hyper",
  "diabetes",
  "hiv",
  "depress",
  "psych",
  "neuro",
  "comorother"
)

como <- como_var_names %>% 
  lapply(function(v){
    
    var_r <- sym(paste0(v, "_r"))
    var_p <- sym(paste0(v, "_p"))
    var_p_cal <- sym(paste0(v, "_p_cal"))
    
    out <- PC_clean %>% 
      select(
        id,
        arm,
        n,
        !!var_r, !!var_p
      ) %>% 
      mutate(
        "{v}_p" := round(!!var_p, digits = 0),
        "{v}_p_cal" := round(!!var_r*100/n, digits = 0)
      ) %>% 
      filter(
        ({{var_p}} != !!var_p_cal) | 
          (!is.na(!!var_p_cal) & is.na(!!var_p))
      )
    
    return(out)
    
  })

names(como) <- paste0("como_", como_var_names)


#### Symptoms associated with migraine ####

symp_var_names <- c(
  "sympnausea",
  "sympvomit",
  "sympphono",
  "sympphoto",
  "sympphonaphoto",
  "sympnone",
  "sympother"
)

symp <- symp_var_names %>% 
  lapply(function(v){
    
    var_r <- sym(paste0(v, "_r"))
    var_p <- sym(paste0(v, "_p"))
    var_p_cal <- sym(paste0(v, "_p_cal"))
    
    out <- PC_clean %>% 
      select(
        id,
        arm,
        symp_n,
        !!var_r, !!var_p
      ) %>% 
      mutate(
        "{v}_p" := round(!!var_p, digits = 0),
        "{v}_p_cal" := round(!!var_r*100/symp_n, digits = 0)
      ) %>% 
      filter(
        ({{var_p}} != !!var_p_cal) | 
          (!is.na(!!var_p_cal) & is.na(!!var_p))
      )
    
    return(out)
    
  })

names(symp) <- paste0("symp_", symp_var_names)


#### Most bothersome symptom (MBS) ####

mbsnausea_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    mbs_n,
    mbsnausea_r,
    mbsnausea_p
  ) %>% 
  mutate(
    mbsnausea_p = round(mbsnausea_p, digits = 0),
    mbsnausea_p_cal = round(mbsnausea_r*100/mbs_n, digits = 0)
  ) %>%
  filter(
    mbsnausea_p != mbsnausea_p_cal
  )


mbsphono_p_diff <- PC_clean %>% 
  select(
    id,
    arm,
    mbs_n,
    mbsphono_r,
    mbsphono_p
  ) %>% 
  mutate(
    mbsphono_p = round(mbsphono_p, digits = 0),
    mbsphono_p_cal = round(mbsphono_r*100/mbs_n, digits = 0)
  ) %>%
  filter(
    mbsphono_p != mbsphono_p_cal
  )



PC_clean %>% 
  select(
    id,
    arm,
    mbs_n,
    mbsphoto_r,
    mbsphoto_p
  ) %>% 
  mutate(
    mbsphoto_p = round(mbsphoto_p, digits = 0),
    mbsphoto_p_cal = round(mbsphoto_r*100/mbs_n, digits = 0)
  ) %>%
  filter(
    mbsphoto_p != mbsphoto_p_cal
  )


#### Export the results ####

list(
  sex_male = male_p_diff,
  sex_female = female_p_diff,
  race_white = white_p_diff,
  race_black = black_p_diff,
  race_asian = asian_p_diff,
  race_other = raceother_p_diff,
  aura = aura_p_diff,
  migraine_severe = migraine_severe_p_diff,
  migraine_moderate = migraine_moderate_p_diff,
  migraine_mild = migraine_mild_p_diff,
  mbs_nausea = mbsnausea_p_diff,
  mbs_phono = mbsphono_p_diff
) %>% 
  c(
    como, symp
  ) %>% 
  writexl::write_xlsx(
    path = paste0(out_dir, "patient_char_p_diff.xlsx")
  )

## Lorenzi Plots ####

dat_pc_tmp <- dat_pc %>% 
  filter(
    trial != "DiSerio 1989"
  )

### Preventive therapy ####
prev <- dat_pc_tmp %>% 
  select(
    trial,
    treatment,
    N = prev_n,
    prev_p
  ) %>% 
  mutate(
    prev_r = N*prev_p/100
  ) %>% 
  as.data.frame() %>% 
  Calc_np(an_only = T)

LP_dat_prev <- prepare.baseline.data(
  id.var = dat_pc_tmp$trial, 
  trt.var = dat_pc_tmp$focus_tr, 
  n.var = dat_pc_tmp$n,
  baseline.vars = prev["prev_p"]
)
names(LP_dat_prev)[3] <- "var"

lorenzi_plot(dat = LP_dat_prev,
             # order_dat = tri_ord,
             axis_lab = "Preventive therapy (%)",
             out_path = paste0(out_dir, "Lorenzi plots/Lorenzi plot - Preventive therapy.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

### Rescue therapy ####

resc <- dat_pc_tmp %>% 
  select(
    trial,
    treatment,
    N = resc_pop_n,
    resc_r = resc_n,
    resc_p
  ) %>% 
  as.data.frame() %>% 
  Calc_np(an_only = T)

LP_dat_resc <- prepare.baseline.data(
  id.var = dat_pc_tmp$trial, 
  trt.var = dat_pc_tmp$focus_tr, 
  n.var = dat_pc_tmp$n,
  baseline.vars = resc["resc_p"]
)
names(LP_dat_resc)[3] <- "var"

lorenzi_plot(dat = LP_dat_resc,
             # order_dat = tri_ord,
             axis_lab = "Rescue therapy (%)",
             out_path = paste0(out_dir, "Lorenzi plots/Lorenzi plot - Rescue therapy.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

### Age ####

AGE <- dat_pc_tmp %>% 
  mutate(
    age_n = n, 
    age_median = age_est, 
    age_mean = NA_real_,
    age_sd = age_disp, 
    age_se = NA_real_, 
    age_95l = NA_real_, 
    age_95u = NA_real_,
    age_iqrl = NA_real_, 
    age_iqru = NA_real_
  ) %>% 
  as.data.frame() %>% 
  Prep_cont(prefix = "age", clean = F) %>%
  mutate(est_type_age = "Mean") #Just 'hacking' this so it works properly for this function

LP_dat_age <- prepare.baseline.data(
  id.var = dat_pc_tmp$trial, 
  trt.var = dat_pc_tmp$focus_tr, 
  n.var = dat_pc_tmp$n,
  baseline.vars = AGE[, 1, drop = F]
)
names(LP_dat_age)[3] <- "var"

lorenzi_plot(dat = LP_dat_age,
             # order_dat = tri_ord,
             axis_lab = "Age (Years)",
             out_path = paste0(out_dir, "Lorenzi plots/Lorenzi plot - Age.png"),
             lowl = 0,
             upl = 50)

#Female
female <- Calc_np(PC[, c("trial", "treatment", "n", "female_r", "female_p")] %>% rename(N = n), an_only = T)

LP_dat_female <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                       baseline.vars = female["female_p"])
names(LP_dat_female)[3] <- "var"

lorenzi_plot(dat = LP_dat_female,
             order_dat = tri_ord,
             axis_lab = "Female (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Sex.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

#Ethnicity
ethnic <- Calc_np(PC[, c("trial", "treatment", "n", "white_r", "white_p", "black_r", "black_p", "asian_r", 
                         "asian_p", "raceother_r", "raceother_p")] %>% rename(N = n), an_only = T)


LP_dat_ET <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                   baseline.vars = ethnic[,grepl("_p", names(ethnic))]) %>%
  rename(`White` = white_p, `Black` = black_p, `Asian` = asian_p, `Other` = raceother_p)

lorenzi_plot(dat = LP_dat_ET,
             order_dat = tri_ord,
             axis_lab = "Ethnicity (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Ethnicity.png"),
             stacked = T)

#Aura
aura <- Calc_np(PC[, c("trial", "treatment", "n", "aura_r", "aura_p")] %>% rename(N = n), an_only = T)

LP_dat_aura <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                     baseline.vars = aura["aura_p"])
names(LP_dat_aura)[3] <- "var"

lorenzi_plot(dat = LP_dat_aura,
             order_dat = tri_ord,
             axis_lab = "History of aura (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Aura.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

#Migraine severity
severity <- Calc_np(PC[, c("trial", "treatment", "sever_n", "severe_r", "severe_p", "moderate_r", "moderate_p", "mild_r", 
                           "mild_p")] %>% rename(N = sever_n), an_only = T)


LP_dat_SEV <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                    baseline.vars = severity[,grepl("_p", names(severity))]) %>%
  rename(`Severe` = severe_p, `Moderate` = moderate_p, `Mild` = mild_p)

lorenzi_plot(dat = LP_dat_SEV,
             order_dat = tri_ord,
             axis_lab = "Migraine severity (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Migraine severity.png"),
             stacked = T)

#Duration of migraine
DUR <- Prep_cont(dat = PC %>% mutate(dur_n = n, dur_median = dur_est, dur_mean = NA_real_,
                                     dur_sd = dur_disp, dur_se = NA_real_, dur_95l = NA_real_, dur_95u = NA_real_,
                                     dur_iqrl = NA_real_, dur_iqru = NA_real_), prefix = "dur", clean = F) %>%
  mutate(est_type_dur = "Mean") #Just 'hacking' this so it works properly for this function

LP_dat_dur <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n, baseline.vars = DUR[, 1, drop = F])
names(LP_dat_dur)[3] <- "var"

lorenzi_plot(dat = LP_dat_dur,
             order_dat = tri_ord,
             axis_lab = "Duration of migraine (Hours)",
             out_path = paste0(out_dir, "Lorenzi plot - Duration of migraine.png"),
             lowl = 0,
             upl = 40)

#Time from diagnosis
TFD <- Prep_cont(dat = PC %>% mutate(tfd_n = n, tfd_median = tfd_est, tfd_mean = NA_real_,
                                     tfd_sd = tfd_disp, tfd_se = NA_real_, tfd_95l = NA_real_, tfd_95u = NA_real_,
                                     tfd_iqrl = NA_real_, tfd_iqru = NA_real_), prefix = "tfd", clean = F) %>%
  mutate(est_type_tfd = "Mean") #Just 'hacking' this so it works properly for this function

LP_dat_tfd <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n, baseline.vars = TFD[, 1, drop = F])
names(LP_dat_tfd)[3] <- "var"

lorenzi_plot(dat = LP_dat_tfd,
             order_dat = tri_ord,
             axis_lab = "Time from diagnosis (Years)",
             out_path = paste0(out_dir, "Lorenzi plot - Time from diagnosis.png"),
             lowl = 0,
             upl = 30)

#Attacks per month
APM <- Prep_cont(dat = PC %>% mutate(attacks_n = n, attacks_median = attacks_est, attacks_mean = NA_real_,
                                     attacks_sd = attacks_disp, attacks_se = NA_real_, attacks_95l = NA_real_, attacks_95u = NA_real_,
                                     attacks_iqrl = NA_real_, attacks_iqru = NA_real_), prefix = "attacks", clean = F) %>%
  mutate(est_type_attacks = "Mean") #Just 'hacking' this so it works properly for this function

LP_dat_attacks <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n, baseline.vars = APM[, 1, drop = F])
names(LP_dat_attacks)[3] <- "var"

lorenzi_plot(dat = LP_dat_attacks,
             order_dat = tri_ord,
             axis_lab = "Attacks per month",
             out_path = paste0(out_dir, "Lorenzi plot - Attacks per month.png"),
             lowl = 0,
             upl = 10,
             breaks = 1)

#Symptoms - nausea
sympnausea <- Calc_np(PC[, c("trial", "treatment", "symp_n", "sympnausea_r", "sympnausea_p")] %>% rename(N = symp_n), an_only = T)

LP_dat_sympnausea <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                           baseline.vars = sympnausea["sympnausea_p"])
names(LP_dat_sympnausea)[3] <- "var"

lorenzi_plot(dat = LP_dat_sympnausea,
             order_dat = tri_ord,
             axis_lab = "Symptoms associated with migraine - nausea (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Nausea symptoms.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

#Symptoms - Vomiting
sympvomit <- Calc_np(PC[, c("trial", "treatment", "symp_n", "sympvomit_r", "sympvomit_p")] %>% rename(N = symp_n), an_only = T)

LP_dat_sympvomit <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                          baseline.vars = sympvomit["sympvomit_p"])
names(LP_dat_sympvomit)[3] <- "var"

lorenzi_plot(dat = LP_dat_sympvomit,
             order_dat = tri_ord,
             axis_lab = "Symptoms associated with migraine - vomiting (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Vomiting symptoms.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

#Symptoms - phonophobia
sympphono <- Calc_np(PC[, c("trial", "treatment", "symp_n", "sympphono_r", "sympphono_p")] %>% rename(N = symp_n), an_only = T)

LP_dat_sympphono <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                          baseline.vars = sympphono["sympphono_p"])
names(LP_dat_sympphono)[3] <- "var"

lorenzi_plot(dat = LP_dat_sympphono,
             order_dat = tri_ord,
             axis_lab = "Symptoms associated with migraine - phonophobia (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Phonophobia symptoms.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

#Symptoms - photophobia
sympphoto <- Calc_np(PC[, c("trial", "treatment", "symp_n", "sympphoto_r", "sympphoto_p")] %>% rename(N = symp_n), an_only = T)

LP_dat_sympphoto <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                          baseline.vars = sympphoto["sympphoto_p"])
names(LP_dat_sympphoto)[3] <- "var"

lorenzi_plot(dat = LP_dat_sympphoto,
             order_dat = tri_ord,
             axis_lab = "Symptoms associated with migraine - photophobia (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Photophobia symptoms.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

#Symptoms - none
sympnone <- Calc_np(PC[, c("trial", "treatment", "symp_n", "sympnone_r", "sympnone_p")] %>% rename(N = symp_n), an_only = T)

LP_dat_sympnone <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                         baseline.vars = sympnone["sympnone_p"])
names(LP_dat_sympnone)[3] <- "var"

lorenzi_plot(dat = LP_dat_sympnone,
             order_dat = tri_ord,
             axis_lab = "Symptoms associated with migraine - none (%)",
             out_path = paste0(out_dir, "Lorenzi plot - No symptoms.png"),
             stacked = T,
             single = T,
             rmv_leg = T)

#Most bothersome symptom
mbs <- Calc_np(PC[, c("trial", "treatment", "mbs_n", "mbsnausea_r", "mbsnausea_p", "mbsphono_r", "mbsphono_p", "mbsphoto_r", 
                      "mbsphoto_p")] %>% rename(N = mbs_n), an_only = T)

LP_dat_MBS <- prepare.baseline.data(id.var = PC$trial, trt.var = PC$node, n.var = PC$n,
                                    baseline.vars = mbs[,grepl("_p", names(mbs))]) %>%
  rename(`Nausea` = mbsnausea_p, `Phonophobia` = mbsphono_p, `Photophobia` = mbsphoto_p)

lorenzi_plot(dat = LP_dat_MBS,
             order_dat = tri_ord,
             axis_lab = "Most bothersome symptom (%)",
             out_path = paste0(out_dir, "Lorenzi plot - MBS.png"),
             stacked = T)

#Redosing (we read this in from a separate file)
redosing <- data.frame(read_xlsx(paste0(dat_dir, "combined redosing data.xlsx"), sheet = "Arm level"), stringsAsFactors = FALSE)
rds <- Calc_np(redosing[, c("trial", "treatment", "n", "rds_r", "rds_p")] %>% rename(N = n), an_only = T)

LP_dat_redosing <- prepare.baseline.data(id.var = redosing$trial, trt.var = redosing$node, n.var = redosing$n,
                                         baseline.vars = rds["rds_p"])
names(LP_dat_redosing)[3] <- "var"

lorenzi_plot(dat = LP_dat_redosing,
             order_dat = tri_ord,
             axis_lab = "Redosed with trial drug (%)",
             out_path = paste0(out_dir, "Lorenzi plot - Redosing.png"),
             stacked = T,
             single = T,
             rmv_leg = T)
