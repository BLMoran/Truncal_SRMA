#Import Dataset with Separated Intervention Groups for Multi-arm Trials and Clean and Order Data
library(tidyverse)
library(metafor)

# Import the Data
truncal2 <- read.csv(paste0("Data/Data_Split_Multiarm_Trials 10_7_22.csv"))

#Clean Column Names & Remove 'et al.'
truncal2 <- truncal2 |>  
  janitor::clean_names() |> 
  mutate(author = stringr::str_remove(author, "et al."))

# Generate Subgroups for Meta Analysis
truncal2 <- truncal2 |>  
  mutate(subgroup = case_when(
    (intervention_type_intervention_1 == "Placebo") ~ "Placebo Vs Truncal Block",
    (intervention_type_intervention_1 == "Control") ~ "Control vs Truncal Block",
    (intervention_type_intervention_1 == "TEA") ~ "TEA Vs Truncal Block",
    (intervention_type_intervention_1 == "TAP") ~ "Intermittent Bolus Vs Continuous Infusion",
    (intervention_type_intervention_1 == "Wound") ~ "Wound Vs Truncal Block",
    (intervention_type_intervention_1 == "Quadratus Lumborum") ~ "Other"),
    subgroup = factor(subgroup,
                      levels = c("Control vs Truncal Block",
                                 "Placebo Vs Truncal Block",
                                 "TEA Vs Truncal Block",
                                 "Wound Vs Truncal Block",
                                 "Intermittent Bolus Vs Continuous Infusion",
                                 "Other")
    )
  )

# Create Function to Generate Dataframes for each outcome (eg Dynamic NRS at 24, 48, 72 hours, etc) and escalc()
var_select <- function(df, x, y, z){
  if(missing(x)) {
    varname1 <- paste({y}, "mean", {z}, "int1", sep = "_")} 
  else {varname1 <- paste({x}, {y}, "mean", {z}, "int1", sep = "_")}
  
  if(missing(x)) {
    varname2 <- paste({y}, "sd", {z}, "int1", sep = "_")} 
  else {varname2 <- paste({x}, {y}, "sd", {z}, "int1", sep = "_")}
  
  if(missing(x)) {
    varname3 <- paste({y}, "mean", {z}, "int2", sep = "_")} 
  else {varname3 <- paste({x}, {y}, "mean", {z}, "int2", sep = "_")}
  
  if(missing(x)) {
    varname4 <- paste({y}, "sd", {z}, "int2", sep = "_")}
  else {varname4 <- paste({x}, {y}, "sd", {z}, "int2", sep = "_")}  
  
  dat <- df |> 
    dplyr::select(author, year, intervention_type_intervention_1:intervention_type_intervention_2, subgroup, n_int1, n_int2, varname1:varname4) |> 
    drop_na()
  
  dat<- metafor::escalc(measure = "MD", n1i = n_int2, n2i = n_int1, m1i = dat[[varname3]], sd1i = dat[[varname4]], 
                        m2i = dat[[varname1]], sd2i = dat[[varname2]], data = dat) |> 
    mutate(sei = sqrt(vi))
  return(dat) 
}

# Create Dataframes for Each Outcome (ie Dynamic NRS at 24, 48, 72 Hours, etc)
dynamic_nrs_24 <- var_select(truncal2, "dynamic", "pain", "24h") 
dynamic_nrs_48 <- var_select(truncal2, "dynamic", "pain", "48h")
dynamic_nrs_72 <- var_select(truncal2, "dynamic", "pain", "72h")
static_nrs_24 <- var_select(truncal2, "static", "pain", "24h")
static_nrs_48 <- var_select(truncal2, "static", "pain", "48h")
static_nrs_72 <- var_select(truncal2, "static", "pain", "72h")
ome_24 <- var_select(truncal2, , "ome", "24h")
ome_48 <- var_select(truncal2, , "ome", "48h")
ome_72 <- var_select(truncal2, , "ome", "72h")