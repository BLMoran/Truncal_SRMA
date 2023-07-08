
#Load Libraries
library(readr)
library(tidyverse)
library(glue)
library(gt)
library(janitor)

#Import Data from Covidence
truncal <- read_csv("Data/Data_Extraction_Initial.csv")

#Clean Column Names
truncal <- truncal |>  
  clean_names()

# Generate Table 1
#Create variable for % of females
truncal <- truncal |> 
  mutate(female_pct_1 = female_intervention_1*100/participant_number_intervention_1,
         female_pct_2 = female_intervention_2*100/participant_number_intervention_2,
         female_pct_3 = female_intervention_3*100/participant_number_intervention_3,
         female_pct_4 = female_intervention_4*100/participant_number_intervention_4) |> 
  mutate(across(female_pct_1:female_pct_4, round, 1))

# Combine Columns
truncal <- truncal|> 
  mutate(
    Study = str_glue("{author}, {year}"),
    Age_Intervention_1 = glue("{age_mean_intervention_1} ({age_sd_intervention_1})"),
    Age_Intervention_2 = glue("{age_mean_intervention_2} ({age_sd_intervention_2})"),
    Age_Intervention_3 = glue("{age_mean_intervention_3} ({age_sd_intervention_3})"),
    Age_Intervention_4 = glue("{age_mean_intervention_4} ({age_sd_intervention_4})"),
    ASA_Intervention_1 = glue("{asa_1_intervention_1}/{asa_2_intervention_1}/{asa_3_intervention_1}/{asa_4_intervention_1}"),
    ASA_Intervention_2 = glue("{asa_1_intervention_2}/{asa_2_intervention_2}/{asa_3_intervention_2}/{asa_4_intervention_2}"),
    ASA_Intervention_3 = glue("{asa_1_intervention_3}/{asa_2_intervention_3}/{asa_3_intervention_3}/{asa_4_intervention_3}"),
    ASA_Intervention_4 = glue("{asa_1_intervention_4}/{asa_2_intervention_4}/{asa_3_intervention_4}/{asa_4_intervention_4}"),
    Female_Intervention_1 = glue("{female_intervention_1} ({female_pct_1}%)"),
    Female_Intervention_2 = glue("{female_intervention_2} ({female_pct_2}%)"),
    Female_Intervention_3 = glue("{female_intervention_3} ({female_pct_3}%)"),
    Female_Intervention_4 = glue("{female_intervention_4} ({female_pct_4}%)"),
    Group_Number_1 = glue("{intervention_type_intervention_1} ({participant_number_intervention_1})"),
    Group_Number_2 = glue("{intervention_type_intervention_2} ({participant_number_intervention_2})"),
    Group_Number_3 = glue("{intervention_type_intervention_3} ({participant_number_intervention_3})"),
    Group_Number_4 = glue("{intervention_type_intervention_4} ({participant_number_intervention_4})"))

#Generate Descriptive Table

tbl1data <- truncal|> 
  select(Study,
         cohort,
         Group_Number_1,
         Group_Number_2,
         Group_Number_3,
         Group_Number_4,
         dose_schedule_intervention_1,
         dose_schedule_intervention_2,
         dose_schedule_intervention_3,
         dose_schedule_intervention_4,
         Female_Intervention_1,
         Female_Intervention_2,
         Female_Intervention_3,
         Female_Intervention_4,
         Age_Intervention_1,
         Age_Intervention_2,
         Age_Intervention_3,
         Age_Intervention_4,
         ASA_Intervention_1,
         ASA_Intervention_2,
         ASA_Intervention_3,
         ASA_Intervention_4,
         incision_location,
         regime_int1,
         regime_int2,
         regime_int3,
         regime_int4,
         duration_fu) |> 
  arrange(Study)

#Remove NA character strings after glue
tbl1data <- tbl1data|> 
  mutate(
    Group_Number_1 = na_if(Group_Number_1, "NA (NA)"),
    Group_Number_2 = na_if(Group_Number_2, "NA (NA)"),
    Group_Number_3 = na_if(Group_Number_3, "NA (NA)"),
    Group_Number_4 = na_if(Group_Number_4, "NA (NA)"),
    Female_Intervention_1 = na_if(Female_Intervention_1, "NA (NA%)"),
    Female_Intervention_2 = na_if(Female_Intervention_2, "NA (NA%)"),
    Female_Intervention_3 = na_if(Female_Intervention_3, "NA (NA%)"),
    Female_Intervention_4 = na_if(Female_Intervention_4, "NA (NA%)"),
    Age_Intervention_1 = na_if(Age_Intervention_1, "NA (NA)"),
    Age_Intervention_2 = na_if(Age_Intervention_2, "NA (NA)"),
    Age_Intervention_3 = na_if(Age_Intervention_3, "NA (NA)"),
    Age_Intervention_4 = na_if(Age_Intervention_4, "NA (NA)"),
    ASA_Intervention_1 = na_if(ASA_Intervention_1, "NA/NA/NA/NA"),
    ASA_Intervention_2 = na_if(ASA_Intervention_2, "NA/NA/NA/NA"),
    ASA_Intervention_3 = na_if(ASA_Intervention_3, "NA/NA/NA/NA"),
    ASA_Intervention_4 = na_if(ASA_Intervention_4, "NA/NA/NA/NA")
  )


tbl1data <- tbl1data %>% 
  pivot_longer(
    cols = !c(Study, cohort, incision_location),
    names_to = c(".value", ".value", ".value", ".value", ".value"),
    names_pattern = "(.)(.)(.)(.)(.)",
    values_drop_na = TRUE
  ) |> 
  group_by(Study, cohort) |>  
  mutate(cohort = replace(cohort, duplicated(cohort), NA)) |>  
  group_by(Study, incision_location) |>  
  mutate(incision_location = replace(incision_location, duplicated(incision_location), NA))


#Generate Table 1
tbl1 <- tbl1data |> 
  gt(groupname_col = c("Study")) |>  
  tab_stubhead(label = "Study") |>  
  tab_options(
    row_group.as_column = TRUE,
    row_group.border.top.width = px(3),
    row_group.font.weight = "bold",
    table.border.bottom.width = px(3),
    table.border.bottom.color = "black",
    table.border.top.color = "white",
    heading.border.bottom.color = "black",
    heading.border.bottom.width = px(3),
    heading.title.font.weight = "bold") |>  
  sub_missing(
    rows = everything(),
    missing_text = " ") |>  
  tab_header(
    title = "Table 1: Characteristics of Included Studies"
  ) |>  
  opt_align_table_header(align = "left") |>  
  cols_label(
    cohort = "Surgical Cohort",
    incision_location = "Incision Location",
    Group = "Intervention (Number)",
    dose_ = "Dose Schedule",
    Femal = "Female (%)",
    Age_I = "Mean Age (SD)",
    ASA_I = "ASA",
    regim = "Intervention Regime",
    durat = "Follow-Up"
  ) |>  
  cols_move(
    columns = incision_location,
    after = ASA_I
  ) |>  
  tab_style(
    locations = cells_body(columns = everything(), rows = everything()),
    style = cell_borders(
      sides = "bottom", 
      color = "white")
  ) |>  
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_text(weight = "bold"),
      cell_borders(sides = "bottom", weight = px(3))
    )
  ) |>  
  tab_style(
    locations = cells_row_groups(groups = everything()),
    style = cell_text(weight = "bold")
  ) |>  
  tab_style(
    locations = cells_stubhead(),
    style = list(
      cell_text(weight = "bold"),
      cell_borders(sides = "bottom", weight = px(3))
    )
  ) |>  
  tab_style(
    locations = cells_body(columns = everything()),
    style = list(
      cell_borders(sides = "left", weight = px(3), color = "light grey")
    )
  ) |>  
  cols_align(align = "left", columns = everything()) %>% 
  tab_footnote(
    footnote = "Abbreviations: Control= Standard care (e.g. Patient-Controlled Analgesia), Placebo= Block performed with 0.9% NaCl, 
  TAP= Transversus Abdominus Plane, TEA= Thoracic Epidural Analgesia, PVB= Paravertebral Block",
    locations = cells_column_labels(
      columns = Group
    )
  )

tbl1