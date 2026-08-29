# Table Exercise: Demographic summary table using {gtsummary}

# Create a Demography table split by treatment

# Setup
## Load necessary packages
library(gtsummary)
library(tidyverse)

## Import data
df_gtsummary_exercise <- pharmaverseadam::adsl |>
  filter(SAFFL == "Y") |>
  left_join(
    pharmaverseadam::advs |>
      filter(PARAMCD %in% c("BMI", "HEIGHT", "WEIGHT"), !is.na(AVAL)) |>
      arrange(ADY) |>
      slice(1, .by = c(USUBJID, PARAMCD)) |>
      pivot_wider(id_cols = USUBJID, names_from = PARAMCD, values_from = AVAL),
    by = "USUBJID"
  ) |>
  select(USUBJID, TRT01A, AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT) |>
  labelled::set_variable_labels(
    BMI = "BMI",
    HEIGHT = "Height, cm",
    WEIGHT = "Weight, kg"
  )

# 1. Use tbl_summary() to summarize AGE, AGEGR1, SEX, RACE, ETHNIC, BMI, HEIGHT, WEIGHT by TRT01A
# 2. For all continuous variables, present the following stats: c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}")
# 3. Ensure the AGEGR1 levels are reported in the correct order
# 4. View the ARD saved in the gtsummary table using `gather_ard()` function
# BONUS!
# 5. Add the header "**Active Treatment**" over the 'Xanomeline' treatments using the `modify_spanning_header()` function

adsl <- pharmaverseadam::adsl |> filter(SAFFL == "Y")

tbl <-
  adsl |>
  mutate(AGEGR1 = factor(AGEGR1)) |>
  tbl_summary(
    by = TRT01A,
    include = AGE,
    type = AGE ~ 'continuous2',
    statistic = list(AGE ~ c('{N_nonmiss}','{mean} ({sd})','{median}','{min} - {max}')),
    label =  list(AGE ~ 'Age in years'),
    digits = list(AGE ~ c(mean=1, sd=2))
  ) |> gtsummary::modify_table_body(~ .x |> mutate(label=ifelse(label=='No. obs.','n',label))) |> 
  # add a header above the 'Xanomeline' treatments. HINT: Use `show_header_names()` to know the column names
  modify_spanning_header(c(stat_2,stat_3) ~ '*active treatments*') |> 
  modify_footnote_header(footnote = 'all', columns = all_stat_cols())
  

tbl$table_body

mean <- 1

glue::glue('this is the {mean}')


tbl <-
  adsl |>
  mutate(AGEGR1 = factor(AGEGR1)) |>
  tbl_summary(
    by = TRT01A,
    include = AGE,
    type = AGE ~ 'continuous2',
    statistic = AGE ~ c('{N_nonmiss}','{mean} ({sd})','{median}','{min} - {max}'),
    label =  ,
    digits = ,
      missing = 'no'
  ) |> gtsummary::modify_table_body(~ .x |> mutate(label=ifelse(label=='No. obs.','n',label))) |> 
  # add a header above the 'Xanomeline' treatments. HINT: Use `show_header_names()` to know the column names
  modify_spanning_header(c(stat_2,stat_3) ~ '*active treatments*') |> 
  modify_footnote_header(footnote = 'all', columns = all_stat_cols())



# extract the ARD from the table
