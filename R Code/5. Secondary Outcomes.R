# Secondary Outcomes
library(tidyverse)
library(gtsummary)
library(bstfun)

# Select Surgical Outcome Data
surg_outcomes <- truncal2 |>  
  select(author, intervention_type_intervention_1:intervention_type_intervention_2, first_bowel_int1:ileus_int2)

# Create Table of Surgical Outcomes
surg_outcomes_cont <- surg_outcomes |>  
  select(hosp_los_int1, first_bs_int1, time_ambulation_int1, ileus_int1) |>  
  rename("Hospital LOS" = hosp_los_int1,
         "Time to First Bowel Sounds (Hrs)" = first_bs_int1,
         "Time to Ambulation (Hrs)" = time_ambulation_int1,
         "Ileus" = ileus_int1) |>  
  mutate_all(as.numeric) |>  
  mutate(Intervention = "Control")

surg_outcomes_int <- surg_outcomes |>  
  select(hosp_los_int2, first_bs_int2, time_ambulation_int2, ileus_int2) |>  
  rename("Hospital LOS" = hosp_los_int2,
         "Time to First Bowel Sounds (Hrs)" = first_bs_int2,
         "Time to Ambulation (Hrs)" = time_ambulation_int2,
         "Ileus" = ileus_int2) |>  
  mutate_all(as.numeric) |>  
  mutate(Intervention = "Truncal Block")

surg_outcomes_total <- rbind(surg_outcomes_cont, surg_outcomes_int)

table_surg_outcome <- surg_outcomes_total |>  
  select("Hospital LOS":"Intervention") |>  
  tbl_summary(by = "Intervention", missing = "no",
              type = everything() ~ "continuous") |>  
  add_p() |>  
  modify_spanning_header(starts_with("stat_") ~ "**Intervention**")

# Select Procedure Morbidity Data
procedure_morb <- truncal2|> 
  select(author, intervention_type_intervention_1:intervention_type_intervention_2, procedure_compl_int1, procedure_compl_int2, block_failure_int1, block_failure_int2)

# Create Table of Procedural Morbidity
proc_morb_cont <- procedure_morb|>
  select(procedure_compl_int1, block_failure_int1)|>
  rename("Procedural Complication" = procedure_compl_int1,
         "Block Failure" = block_failure_int1)|>
  mutate_all(as.numeric)|>
  mutate(Intervention = "Control")

proc_morb_int <- procedure_morb|>
  select(procedure_compl_int2, block_failure_int2)|>
  rename("Procedural Complication" = procedure_compl_int2,
         "Block Failure" = block_failure_int2)|>
  mutate_all(as.numeric)|>
  mutate(Intervention = "Truncal Block")

proc_morb_total <- rbind(proc_morb_cont, proc_morb_int)

table_proc_morb <- proc_morb_total|>
  select("Procedural Complication":"Intervention")|>
  tbl_summary(by = "Intervention", missing = "no",
              type = everything() ~ "continuous")|>
  add_p()|>
  modify_spanning_header(starts_with("stat_") ~ "**Intervention**")

# Select Opioid Side Effect Data
opioid_se <- truncal2|>
  select(author, intervention_type_intervention_1:intervention_type_intervention_2, sedation_int1:pruritis_int1, sedation_int2:pruritis_int2, -c(procedure_compl_int1, procedure_compl_int2))

# Create Table of Opioid Side Effects

opioid_se_cont <- opioid_se|>
  select(sedation_int1:pruritis_int1)|>
  rename("Sedation" = sedation_int1,
         "Nausea & Vomiting" = nv_int1,
         "Hypotension" = hypotension_int1,
         "Pruritis" = pruritis_int1)|>
  mutate_all(as.numeric)|>
  mutate(Intervention = "Control")

opioid_se_int <- opioid_se|>
  select(sedation_int2:pruritis_int2)|>
  rename("Sedation" = sedation_int2,
         "Nausea & Vomiting" = nv_int2,
         "Hypotension" = hypotension_int2,
         "Pruritis" = pruritis_int2)|>
  mutate_all(as.numeric)|>
  mutate(Intervention = "Truncal Block")

opioid_se_total <- rbind(opioid_se_cont, opioid_se_int)

table_opioid_se <- opioid_se_total|>
  select("Sedation":"Intervention")|>
  tbl_summary(by = "Intervention", missing = "no",
              type = everything() ~ "continuous")|>
  add_p()|>
  modify_spanning_header(starts_with("stat_") ~ "**Intervention**")

# Merge Secondary Outcomes Tables
sec_outcomes <- tbl_stack(list(table_surg_outcome, table_proc_morb, table_opioid_se),
                          group_header = c("Surgical Outcomes", "Procedural Morbidity", "Opioid Side Effects"))|>
  bold_italicize_group_labels(bold = TRUE)
