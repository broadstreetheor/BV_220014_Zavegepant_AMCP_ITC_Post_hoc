library(tidyverse)


dat_dir <- "Z:/Shared/Projects/Biohaven/BV_220014 Zavegepant AMCP, ITC, & Post-hoc/Indirect treatment comparison/feasibility assesment/"

raw_dat <- paste0(dat_dir, "feasibility_outcomes_pbo.csv") %>% 
  read_csv(
    na = c("", "--", "NA")
  )

dat <- raw_dat %>% 
  mutate(
    trial_long = paste(Trial, `Study arm`, sep = " - ")
  ) %>% 
  select(
    -Trial,
    -`Study arm`
  ) %>% 
  pivot_longer(
    cols = !trial_long,
    names_to = "outcome",
    values_to = "value"
  ) %>%
  pivot_wider(
    id_cols = outcome,
    names_from = trial_long,
    values_from = value
  ) %>% 
  rowwise() %>% 
  mutate(
    n = sum(!is.na(c_across(cols = !outcome))),
    mean = mean(c_across(cols = !outcome), na.rm = TRUE),
    sd = sd(c_across(cols = !outcome), na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    lower_l = mean - sd,
    upper_l = mean + sd,
    outcome = outcome %>% 
      fct_relevel(
        "Headache pain free at 2 hour",
        "Sustained Pain Freedom 2-24 hours post-dose",
        "Sustained Pain Freedom 2-48 hours post-dose",
        "Pain relief (2 hrs post-dose)",
        "Sustained pain relief (2-24 hrs post-dose)",
        "Sustained pain relief (2-48 hrs post-dose)",
        "Freedom from Photophobia at 2 hours",
        "Freedom from Nausea at 2 hours"
      )
  )

p <- dat %>% 
  select(
    outcome,
    n,
    mean,
    lower_l,
    upper_l
  ) %>% 
  ggplot(aes(
    x = mean,
    y = outcome, 
    xmin = lower_l, 
    xmax = upper_l)) + 
  geom_point() + 
  geom_errorbar() + 
  geom_label(
    aes(
      x = 0,
      label = n
    ),
    hjust = 0
  ) +
  geom_point(
    aes(
      x = value,
      y = outcome,
      colour = study
    ),
    shape = 4,
    size = 3,
    data = dat %>% 
      select(
        outcome,
        `Dawson 1991 - PBO`:`Salonen 1994 - PBO (two-nostril app.)`
      ) %>% 
      pivot_longer(
        cols = `Dawson 1991 - PBO`:`Salonen 1994 - PBO (two-nostril app.)`,
        names_to = "study",
        values_to = "value"
      ),
    inherit.aes = FALSE
  ) + 
  labs(
    x = "Mean (± SD)",
    y = "Outcome",
    colour = "Study",
    caption = "Individual values marked as 'x'"
  ) +
  scale_x_continuous(
    limits = c(0, 75)
  ) +
  scale_colour_hue(l = 40) +
  theme_minimal() + 
  guides(colour = guide_legend(ncol = 1))


png(
  filename = paste0(dat_dir, "feasibility_outcomes_pbo_forest_plot.png"),
  width = 1280,
  height = 720,
  units = "px"
)
p
dev.off()
