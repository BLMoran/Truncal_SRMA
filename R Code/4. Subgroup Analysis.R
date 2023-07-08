# Subgroup Analysis
# Single Shot Only
truncal_ss <- truncal2 |> 
  filter((dose_schedule_intervention_1 == "Single Shot") |
           (dose_schedule_intervention_2 == "Single Shot"))

# Create Dataframes for Each Outcome
dynamic_nrs_24_ss <- var_select(truncal_ss, "dynamic", "pain", "24h") 
dynamic_nrs_48_ss <- var_select(truncal_ss, "dynamic", "pain", "48h")
dynamic_nrs_72_ss <- var_select(truncal_ss, "dynamic", "pain", "72h")
static_nrs_24_ss <- var_select(truncal_ss, "static", "pain", "24h")
static_nrs_48_ss <- var_select(truncal_ss, "static", "pain", "48h")
static_nrs_72_ss <- var_select(truncal_ss, "static", "pain", "72h")
ome_24_ss <- var_select(truncal_ss, , "ome", "24h")
ome_48_ss <- var_select(truncal_ss, , "ome", "48h")
ome_72_ss <- var_select(truncal_ss, , "ome", "72h")

## Use the escalc() function to calculate individual study mean differences (yi) and variance (vi)
# Dynamic NRS 24 Hours
dynamic_nrs_24_ss <- metafor::escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = dynamic_pain_sd_24h_int2, 
                                     sd2i = dynamic_pain_sd_24h_int1, m1i = dynamic_pain_mean_24h_int2, m2i = dynamic_pain_mean_24h_int1,
                                     data = dynamic_nrs_24_ss) |>  mutate(sei = sqrt(vi))

# Dynamic NRS 48 Hours
dynamic_nrs_48_ss <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = dynamic_pain_sd_48h_int2, 
                            sd2i = dynamic_pain_sd_48h_int1, m1i = dynamic_pain_mean_48h_int2, m2i = dynamic_pain_mean_48h_int1,
                            data = dynamic_nrs_48_ss) |>  mutate(sei = sqrt(vi))

# Dynamic NRS 72 Hours
dynamic_nrs_72_ss <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = dynamic_pain_sd_72h_int2, 
                            sd2i = dynamic_pain_sd_72h_int1, m1i = dynamic_pain_mean_72h_int2, m2i = dynamic_pain_mean_72h_int1,
                            data = dynamic_nrs_72) |>  mutate(sei = sqrt(vi))

# Static NRS 24 Hours
static_nrs_24_ss <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = static_pain_sd_24h_int2, 
                           sd2i = static_pain_sd_24h_int1, m1i = static_pain_mean_24h_int2, m2i = static_pain_mean_24h_int1,
                           data = static_nrs_24_ss) |>  mutate(sei = sqrt(vi))

# Static NRS 48 Hours
static_nrs_48_ss <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = static_pain_sd_48h_int2, 
                           sd2i = static_pain_sd_48h_int1, m1i = static_pain_mean_48h_int2, m2i = static_pain_mean_48h_int1,
                           data = static_nrs_48_ss) |>  mutate(sei = sqrt(vi))

# Static NRS 72 Hours -> No observations

# OME 24 Hours
ome_24_ss <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = ome_sd_24h_int2, 
                    sd2i = ome_sd_24h_int1, m1i = ome_mean_24h_int2, m2i = ome_mean_24h_int1,
                    data = ome_24_ss) |>  mutate(sei = sqrt(vi))

# OME 48 Hours
ome_48_ss <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = ome_sd_48h_int2, 
                    sd2i = ome_sd_48h_int1, m1i = ome_mean_48h_int2, m2i = ome_mean_48h_int1,
                    data = ome_48_ss) |>  mutate(sei = sqrt(vi))

# OME 72 Hours -> No observations


# Continuous Infusion Only- Overall
# Remove Studies with Single Shot Truncal Blocks
truncal_cont <- truncal2 |> 
  filter((dose_schedule_intervention_1 != "Single Shot") |
           (dose_schedule_intervention_2 != "Single Shot"))

# Create Dataframes for Each Outcome
dynamic_nrs_24_cont <- var_select(truncal_cont, "dynamic", "pain", "24h") 
dynamic_nrs_48_cont <- var_select(truncal_cont, "dynamic", "pain", "48h")
dynamic_nrs_72_cont <- var_select(truncal_cont, "dynamic", "pain", "72h")
static_nrs_24_cont <- var_select(truncal_cont, "static", "pain", "24h")
static_nrs_48_cont <- var_select(truncal_cont, "static", "pain", "48h")
static_nrs_72_cont <- var_select(truncal_cont, "static", "pain", "72h")
ome_24_cont <- var_select(truncal_cont, , "ome", "24h")
ome_48_cont <- var_select(truncal_cont, , "ome", "48h")
ome_72_cont <- var_select(truncal_cont, , "ome", "72h")

## Use the escalc() function to calculate individual study mean differences (yi) and variance (vi)
# Dynamic NRS 24 Hours
dynamic_nrs_24_cont <- metafor::escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = dynamic_pain_sd_24h_int2, 
                                       sd2i = dynamic_pain_sd_24h_int1, m1i = dynamic_pain_mean_24h_int2, m2i = dynamic_pain_mean_24h_int1,
                                       data = dynamic_nrs_24_cont) |>  mutate(sei = sqrt(vi))

# Dynamic NRS 48 Hours
dynamic_nrs_48_cont <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = dynamic_pain_sd_48h_int2, 
                              sd2i = dynamic_pain_sd_48h_int1, m1i = dynamic_pain_mean_48h_int2, m2i = dynamic_pain_mean_48h_int1,
                              data = dynamic_nrs_48_cont) |>  mutate(sei = sqrt(vi))

# Dynamic NRS 72 Hours
dynamic_nrs_72_cont <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = dynamic_pain_sd_72h_int2, 
                              sd2i = dynamic_pain_sd_72h_int1, m1i = dynamic_pain_mean_72h_int2, m2i = dynamic_pain_mean_72h_int1,
                              data = dynamic_nrs_72_cont) |>  mutate(sei = sqrt(vi))

# Static NRS 24 Hours
static_nrs_24_cont <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = static_pain_sd_24h_int2, 
                             sd2i = static_pain_sd_24h_int1, m1i = static_pain_mean_24h_int2, m2i = static_pain_mean_24h_int1,
                             data = static_nrs_24_cont) |>  mutate(sei = sqrt(vi))

# Static NRS 48 Hours
static_nrs_48_cont <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = static_pain_sd_48h_int2, 
                             sd2i = static_pain_sd_48h_int1, m1i = static_pain_mean_48h_int2, m2i = static_pain_mean_48h_int1,
                             data = static_nrs_48_cont) |>  mutate(sei = sqrt(vi))

# Static NRS 72 Hours
static_nrs_72_cont <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = static_pain_sd_72h_int2, 
                             sd2i = static_pain_sd_72h_int1, m1i = static_pain_mean_72h_int2, m2i = static_pain_mean_72h_int1,
                             data = static_nrs_72_cont) |>  mutate(sei = sqrt(vi))

# OME 24 Hours
ome_24_cont <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = ome_sd_24h_int2, 
                      sd2i = ome_sd_24h_int1, m1i = ome_mean_24h_int2, m2i = ome_mean_24h_int1,
                      data = ome_24_cont) |>  mutate(sei = sqrt(vi))

ome_48_cont <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = ome_sd_48h_int2, 
                      sd2i = ome_sd_48h_int1, m1i = ome_mean_48h_int2, m2i = ome_mean_48h_int1,
                      data = ome_48_cont) |>  mutate(sei = sqrt(vi))

ome_72_cont <- escalc(measure = "MD", n1i = n_int2, n2i = n_int1, sd1i = ome_sd_72h_int2, 
                      sd2i = ome_sd_72h_int1, m1i = ome_mean_72h_int2, m2i = ome_mean_72h_int1,
                      data = ome_72_cont) |>  mutate(sei = sqrt(vi))


# Continuous Infusion- Placebo

# Continuous Infusion- TEA