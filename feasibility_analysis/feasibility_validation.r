
library(tidyverse)

project_dir <- "Z:/Shared/Projects/Biohaven/BV_220014 Zavegepant AMCP, ITC, & Post-hoc/"
code_dir <- paste0(project_dir, "Analysis/R code/BV_220014_Zavegepant_AMCP_ITC_Post_hoc/feasibility_analysis/")
out_dir <- paste0(project_dir, "Indirect treatment comparison/feasibility assesment/")


## Read data ####

source(paste0(code_dir, "0_feasibility_data_read.r"))


## Patient characteristics ####

### Check data ####


#### Treatment ####
PC %>% 
  count(
    treatment
  ) %>% 
  view()
  # write.table(
  #   "clipboard",
  #   sep = "\t",
  #   row.names = FALSE
  # )

PC %>% 
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

