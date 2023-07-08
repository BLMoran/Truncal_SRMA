# Function to Draw Forest Plots

priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

bayes_forest <- function(data, outcome_mean_int1, outcome_mean_int2, outcome_sd_int1, outcome_sd_int2){
brm_fn <- function(data, subgroup_filter)  {
  dat_surg_type_fn <- data|>
    filter(subgroup == subgroup_filter)|>
    mutate(author1 = paste(author, year, sep = " ")) 
  
  # Run the brms model
  m.brm <- brm(yi | se(sei) ~ 1 + (1 | author1),
               data = dat_surg_type_fn,
               prior = priors,
               iter = 4000,
               cores = detectCores(),
               control = list(adapt_delta = 0.99),
               backend = "cmdstanr")
  return(m.brm)
}

# Now create the forest data function - this is used for density plots
forest.data_fn <- function(m.brm) {
  
  # Calculate the re-weighted estimated of each study
  study.draws <- spread_draws(m.brm, r_author1[author1, ], b_Intercept)|>
    mutate(b_Intercept = r_author1 + b_Intercept)
  
  # Establish the pooled result
  pooled.effect.draws <- spread_draws(m.brm, b_Intercept)|>
    mutate(author1 = "Pooled Effect")
  
  # Combine the pooled result with study estimates, then play around with the lexicon (brms mucks this up)
  forest.data <- bind_rows(study.draws,
                           pooled.effect.draws)|>
    ungroup()|>
    mutate(author1 = str_replace_all(author1, "[.]", " "),
           author1 = reorder(author1, b_Intercept))
  
  return(forest.data)
}


# Now create forest.data.summary function
forest.data.summary_fn <- function(data, subgroup_filter, forest.data) {
  dat_surg_type_fn <- data|>
    filter(subgroup == subgroup_filter)|>
    mutate(sei = sqrt(vi),
           author1 = paste(author, year, sep = " "))
  
  # Calculate mean qi based on author1
  forest.data.summary1 <- group_by(forest.data, author1)|>
    mean_qi(b_Intercept) 
  
  # Join the two dataframes based on similar author names
  forest.data.summary <- forest.data.summary1|>
    left_join(dat_surg_type_fn, by = "author1")|>
    mutate(n_int1_tot = as.character(ifelse(author1 == "Pooled Effect",
                                            sum(dat_surg_type_fn$n_int1),
                                            n_int1)),
           n_int2_tot = as.character(ifelse(author1 == "Pooled Effect",
                                            sum(dat_surg_type_fn$n_int2),
                                            n_int2)),
           author1 = as.factor(reorder(author1, b_Intercept)))
  
  return(forest.data.summary)
}


# Finally, make the res_plot function - this will be used for columns of means, SD's, etc.

res_plot_fn <- function(forest.data.summary, subgroup_filter, shrinkage_estimate, 
                        original_estimate, truncal_n_fn, truncal_mean_sd_fn, control_n_fn, control_mean_sd_fn, m.brm,
                        outcome_mean_int1, outcome_mean_int2, outcome_sd_int1, outcome_sd_int2) { 
  # Extract the estimates for mu and tau
  post.samples <- as_draws_df(m.brm, c("b_Intercept", "sd_author1__Intercept"))
  
  ## Create the values for shrinkage and actual effect estimates
  res_plot_pre <- forest.data.summary|>
    mutate(weighted_effect = paste0(sprintf('%.2f', b_Intercept), 
                                    ' [', sprintf('%.2f', .lower),
                                    ', ', sprintf('%.2f', .upper), ']'),
           unweighted_effect = paste0(sprintf('%.2f', yi), 
                                      ' [', sprintf('%.2f', yi - 1.96*sqrt(vi)),
                                      ', ', sprintf('%.2f', yi + 1.96*sqrt(vi)), ']'))|>
    mutate(unweighted_effect = ifelse(unweighted_effect == "NA [NA, NA]", 
                                      paste0("Mean τ = ", 
                                             sprintf('%.2f', mean(post.samples$sd_author1__Intercept))), unweighted_effect),
           truncal_mean_sd = paste0(sprintf('%.2f', data[[outcome_mean_int2]]), " (", sprintf('%.1f',data[[outcome_sd_int2]]), ")"),
           control_mean_sd = paste0(sprintf('%.2f', data[[outcome_mean_int1]]), " (", sprintf('%.1f',data[[outcome_sd_int1]]), ")"))|>
    mutate(truncal_mean_sd = ifelse(truncal_mean_sd == "NA (NA)", "", truncal_mean_sd),
           control_mean_sd = ifelse(control_mean_sd == "NA (NA)", "", control_mean_sd))
  
  # Add an additional row to the data for headings
  new_row <- data.frame(author1 = as.factor(subgroup_filter),
                        weighted_effect = shrinkage_estimate,
                        unweighted_effect = original_estimate,
                        n_int2_tot = truncal_n_fn,
                        truncal_mean_sd = truncal_mean_sd_fn,
                        n_int1_tot = control_n_fn,
                        control_mean_sd = control_mean_sd_fn)
  
  # Bind the new row with the previous dataframe and set the colours
  res_plot <- bind_rows(res_plot_pre, new_row)|>
    mutate(color = ifelse(author1 == "Pooled Effect", "grey60", "black"),
           font = ifelse(author1 == "Pooled Effect", "bold", "plain"))
  
  return(res_plot)
}

## Now we need to run each subgroup through all the above functions
# First, the control subgroup
# This one is a bit different because it will be the top plot, so requires a heading for each row

brm_control <- brm_fn(data, "Control vs Truncal Block")
## Check Rhat = 1 to ensure model convergence. Also check pp_check(m.brm_control) that posterior draws roughly approximate observed data
forest.data_control <- forest.data_fn(brm_control)
forest.data.summary_control <- forest.data.summary_fn(data, "Control vs Truncal Block", forest.data_control)
res_plot_control <- res_plot_fn(forest.data.summary_control, "Control vs Truncal Block", "[95%CrI]", "[95%CrI]",
                                "n", "Mean (SD)", "n", "Mean (SD)", brm_control)

## Add an extra row to res_plot_control because we need more space at the top to fit our headings in
new_row_res_plot_control <- data.frame(author1 = as.factor("Subgroup"),
                                       weighted_effect = "Shrinkage MD",
                                       unweighted_effect = "Observed MD",
                                       n_int2_tot = "Intervention",
                                       truncal_mean_sd = "",
                                       n_int1_tot = "Control",
                                       control_mean_sd = "",
                                       color = "black",
                                       font = "plain")

res_plot_control <- bind_rows(res_plot_control, new_row_res_plot_control)

# Now, the placebo subgroup
brm_placebo <- brm_fn(data, "Placebo Vs Truncal Block")
## Check Rhat = 1 to ensure model convergence. Also check pp_check(m.brm_placebo) that posterior draws roughly approximate observed data
forest.data_placebo <- forest.data_fn(brm_placebo)
forest.data.summary_placebo <- forest.data.summary_fn(data, "Placebo Vs Truncal Block", forest.data_placebo)
res_plot_placebo <- res_plot_fn(forest.data.summary_placebo, "Placebo Vs Truncal Block", "", "", "", "", "", "", brm_placebo)

# Now the TEA subgroup
brm_tea <- brm_fn(data, "TEA Vs Truncal Block")
## Check Rhat = 1 to ensure model convergence. Also check pp_check(m.brm_tea) that posterior draws roughly approximate observed data
forest.data_tea <- forest.data_fn(brm_tea)
forest.data.summary_tea <- forest.data.summary_fn(data, "TEA Vs Truncal Block", forest.data_tea)
res_plot_tea <- res_plot_fn(forest.data.summary_tea, "TEA Vs Truncal Block", "", "", "", "", "", "", brm_tea)

# Finally, the wound subgroup
brm_wound <- brm_fn(data, "Wound Vs Truncal Block")
forest.data_wound <- forest.data_fn(brm_wound)
forest.data.summary_wound <- forest.data.summary_fn(data, "Wound Vs Truncal Block", forest.data_wound)
## Check Rhat = 1 to ensure model convergence. Also check pp_check(m.brm_wound) that posterior draws roughly approximate observed data
res_plot_wound <- res_plot_fn(forest.data.summary_wound, "Wound Vs Truncal Block", "", "", "", "", "", "", brm_wound)


# Now - plotting!
## First, control subgroup plots
## The first plot is the forest plot itself with shrinkage densities for each study

p_forest_control <- ggplot(aes(b_Intercept, 
                               relevel(author1, "Pooled Effect", after=Inf)), 
                           data = forest.data_control) +
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(brm_control)[1, 1], 
             color = "grey60", size = 1) +
  geom_vline(xintercept = fixef(brm_control)[1, 3:4], 
             color = "grey60", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  # Add densities
  geom_density_ridges(fill = "#008cba", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 0.9,
                      alpha = 0.8) +
  geom_pointinterval(data = forest.data.summary_control, 
                     linewidth = 1, color = "blue",
                     aes(xmin = .lower, xmax = .upper)) +
  geom_pointinterval(aes(xmin = yi - 1.96 * sqrt(vi), 
                         xmax = yi + 1.96 * sqrt(vi), 
                         x = yi),
                     position = position_nudge(y = -0.11),
                     data = forest.data.summary_control, 
                     linewidth = 1, 
                     col = "purple") +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6), expand = c(0,0)) +
  coord_cartesian(xlim=c(-6,6), ylim=c(1,8)) +
  annotate("text", x = -3.5, y = 7.5, label = "Favours\ntruncal block", fontface = "bold") +
  annotate("text", x = 4, y = 7.5,  label = "Favours\ncontrol", fontface = "bold") +
  theme_light() +
  theme(axis.line = element_blank(),           # remove axis lines
        axis.text.x = element_blank(),         # remove x-axis tick labels
        axis.ticks.x = element_blank(),        # remove x-axis ticks
        axis.text.y = element_blank()) +
  xlab(NULL) +
  ylab(NULL)

# The second plot shows the mean + 95%CrI for observed + shrinkage effects for each study

p_estimates_control <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), color = color), 
                              data = res_plot_control) +
  geom_text(aes(x = 0, label = weighted_effect), hjust = 0,
            fontface = ifelse(grepl("Shrinkage MD|\\[95%CrI\\]", res_plot_control$weighted_effect), "bold", "plain")) + 
  geom_text(aes(x = 1, label = unweighted_effect), hjust = 0, 
            fontface = ifelse(grepl("Observed MD|\\[95%CrI\\]", res_plot_control$unweighted_effect), "bold", "plain")) +
  scale_color_identity() +
  theme_void() +
  coord_cartesian(xlim = c(0, 2), ylim=c(1,8))

# The third plot shows the study name, numbers in each group, and the relevant summary statistics
p_studies_control <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), color = color), 
                            data = res_plot_control) +
  geom_text(aes(x = 0, label = author1), hjust = 0, 
            fontface = ifelse(res_plot_control$author1 == "Control vs Truncal Block" | res_plot_control$author1 == "Subgroup", "bold", 
                              ifelse(res_plot_control$author1 == "Pooled Effect", "italic","plain"))) +
  geom_text(aes(x = 1.5, label = n_int2_tot), hjust = 0, 
            fontface = ifelse(res_plot_control$n_int2_tot == "Intervention" | res_plot_control$n_int2_tot == "n", "bold", "plain")) +
  geom_text(aes(x = 2, label = truncal_mean_sd), hjust = 0, 
            fontface = ifelse(res_plot_control$truncal_mean_sd == "Mean (SD)", "bold", "plain")) +
  geom_text(aes(x = 3, label = n_int1_tot), hjust = 0, 
            fontface = ifelse(res_plot_control$n_int1_tot == "Control" |  res_plot_control$n_int1_tot == "n", "bold", "plain")) +
  geom_text(aes(x = 3.5, label = control_mean_sd), hjust = 0, 
            fontface = ifelse(res_plot_control$control_mean_sd == "Mean (SD)", "bold", "plain")) +
  scale_color_identity() + # Use the specified colors directly for text color
  theme_void() +
  coord_cartesian(xlim = c(0, 4), ylim=c(1,8))


## Now, the placebo subgroup - plots are as above

p_forest_placebo <- ggplot(aes(b_Intercept, 
                               relevel(author1, "Pooled Effect", after=Inf)), 
                           data = forest.data_placebo) +
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(brm_placebo)[1, 1], 
             color = "grey60", size = 1) +
  geom_vline(xintercept = fixef(brm_placebo)[1, 3:4], 
             color = "grey60", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  # Add densities
  geom_density_ridges(fill = "#008cba", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 0.9,
                      alpha = 0.8) +
  geom_pointinterval(data = forest.data.summary_placebo, 
                     linewidth = 1, color = "blue",
                     aes(xmin = .lower, xmax = .upper)) +
  geom_pointinterval(aes(xmin = yi - 1.96 * sqrt(vi), 
                         xmax = yi + 1.96 * sqrt(vi), 
                         x = yi),
                     position = position_nudge(y = -0.11),
                     data = forest.data.summary_placebo, 
                     linewidth = 1, 
                     col = "purple") +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6), expand = c(0,0)) +
  coord_cartesian(xlim=c(-6,6), ylim=c(1,11)) +
  theme_light() +
  theme(axis.line = element_blank(),           
        axis.text.x = element_blank(),         
        axis.ticks.x = element_blank(),    
        axis.text.y = element_blank()) +
  xlab(NULL) +
  ylab(NULL)

p_estimates_placebo <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), color = color), 
                              data = res_plot_placebo) +
  geom_text(aes(x = 0, label = weighted_effect), hjust = 0) + 
  geom_text(aes(x = 1, label = unweighted_effect), hjust = 0) +
  theme_void() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0, 2), ylim=c(1,11))

p_studies_placebo <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), color = color), 
                            data = res_plot_placebo) +
  geom_text(aes(x = 0, label = author1), hjust = 0, fontface = ifelse(res_plot_placebo$author1 == "Placebo Vs Truncal Block", "bold", 
                                                                      ifelse(res_plot_placebo$author1 == "Pooled Effect", "italic","plain"))) +
  geom_text(aes(x = 1.5, label = n_int2_tot), hjust = 0) +
  geom_text(aes(x = 2, label = truncal_mean_sd), hjust = 0) +
  geom_text(aes(x = 3, label = n_int1_tot), hjust = 0) +
  geom_text(aes(x = 3.5, label = control_mean_sd), hjust = 0) +
  theme_void() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0,4))

## Now, the TEA subgroup - plot descriptions as above

p_forest_tea <- ggplot(aes(b_Intercept, 
                           relevel(author1, "Pooled Effect", after=Inf)), 
                       data = forest.data_tea) +
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(brm_tea)[1, 1], 
             color = "grey60", size = 1) +
  geom_vline(xintercept = fixef(brm_tea)[1, 3:4], 
             color = "grey60", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  # Add densities
  geom_density_ridges(fill = "#008cba", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 0.9,
                      alpha = 0.8) +
  geom_pointinterval(data = forest.data.summary_tea, 
                     linewidth = 1, color = "blue",
                     aes(xmin = .lower, xmax = .upper)) +
  geom_pointinterval(aes(xmin = yi - 1.96 * sqrt(vi), 
                         xmax = yi + 1.96 * sqrt(vi), 
                         x = yi),
                     position = position_nudge(y = -0.11),
                     data = forest.data.summary_tea, 
                     linewidth = 1, 
                     col = "purple") +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6), expand = c(0,0)) +
  coord_cartesian(xlim=c(-6,6), ylim=c(1,9)) +
  theme_light() +
  theme(axis.line = element_blank(),           
        axis.text.x = element_blank(),         
        axis.ticks.x = element_blank(),    
        axis.text.y = element_blank()) +
  xlab(NULL) +
  ylab(NULL)


p_estimates_tea <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), color = color), 
                          data = res_plot_tea) +
  geom_text(aes(x = 0, label = weighted_effect), hjust = 0) + 
  geom_text(aes(x = 1, label = unweighted_effect), hjust = 0) +
  theme_void() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0, 2), ylim=c(1,9))

p_studies_tea <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), color = color), 
                        data = res_plot_tea) +
  geom_text(aes(x = 0, label = author1), hjust = 0, fontface = ifelse(res_plot_tea$author1 == "TEA Vs Truncal Block", "bold", 
                                                                      ifelse(res_plot_tea$author1 == "Pooled Effect", "italic","plain"))) +
  geom_text(aes(x = 1.5, label = n_int2_tot), hjust = 0) +
  geom_text(aes(x = 2, label = truncal_mean_sd), hjust = 0) +
  geom_text(aes(x = 3, label = n_int1_tot), hjust = 0) +
  geom_text(aes(x = 3.5, label = control_mean_sd), hjust = 0) +
  theme_void() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0,4))

## Then, the wound subgroup - again, plots are as above

p_forest_wound <- ggplot(aes(b_Intercept, 
                             relevel(author1, "Pooled Effect", after=Inf)), 
                         data = forest.data_wound) +
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(brm_wound)[1, 1], 
             color = "grey60", size = 1) +
  geom_vline(xintercept = fixef(brm_wound)[1, 3:4], 
             color = "grey60", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  # Add densities
  geom_density_ridges(fill = "#008cba", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 0.9,
                      alpha = 0.8) +
  geom_pointinterval(data = forest.data.summary_wound, 
                     linewidth = 1, color = "blue",
                     aes(xmin = .lower, xmax = .upper)) +
  geom_pointinterval(aes(xmin = yi - 1.96 * sqrt(vi), 
                         xmax = yi + 1.96 * sqrt(vi), 
                         x = yi),
                     position = position_nudge(y = -0.11),
                     data = forest.data.summary_wound, 
                     linewidth = 1, 
                     col = "purple") +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6), expand = c(0,0)) +
  coord_cartesian(xlim=c(-6,6), ylim=c(1,5)) +
  theme_light() +
  theme(axis.line = element_blank(),           
        axis.text.x = element_blank(),         
        axis.ticks.x = element_blank(),    
        axis.text.y = element_blank()) +
  xlab(NULL) +
  ylab(NULL)


p_estimates_wound <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), color = color), 
                            data = res_plot_wound) +
  geom_text(aes(x = 0, label = weighted_effect), hjust = 0) + 
  geom_text(aes(x = 1, label = unweighted_effect), hjust = 0) +
  theme_void() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0, 2), ylim=c(1,5))

p_studies_wound <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), color = color), 
                          data = res_plot_wound) +
  geom_text(aes(x = 0, label = author1), hjust = 0, fontface = ifelse(res_plot_wound$author1 == "Wound Vs Truncal Block", "bold", 
                                                                      ifelse(res_plot_wound$author1 == "Pooled Effect", "italic","plain"))) +
  geom_text(aes(x = 1.5, label = n_int2_tot), hjust = 0) +
  geom_text(aes(x = 2, label = truncal_mean_sd), hjust = 0) +
  geom_text(aes(x = 3, label = n_int1_tot), hjust = 0) +
  geom_text(aes(x = 3.5, label = control_mean_sd), hjust = 0) +
  theme_void() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0,4))

## Finally, overall effect size 
## We must construct this separately as the functions above won't work
## I've essentially re-written the functions but scrapped all the stuff above individual studies

dat_overall <- dynamic_nrs_24|>
  mutate(author1 = paste(author,year, sep=" "))

brm_overall <- brm(yi|se(sei) ~ 1 + (1|author1),
                   data = dat_overall,
                   prior = priors,
                   iter = 4000,
                   control = list(adapt_delta = 0.99),
                   cores = parallel::detectCores(),
                   backend = "cmdstanr")

post.samples_overall <- as_draws_df(brm_overall, c("b_Intercept", "sd_author1__Intercept"))

study.draws_overall <- spread_draws(brm_overall, r_author1[author1,], b_Intercept)|> 
  mutate(b_Intercept = r_author1 + b_Intercept)

pooled.effect.draws_overall <- spread_draws(brm_overall, b_Intercept)|> 
  mutate(author1 = "Pooled Effect")

forest.data_overall <- pooled.effect.draws_overall|> 
  ungroup()|>
  mutate(author1 = str_replace_all(author1, "[.]", " "))|> 
  mutate(author1 = reorder(author1, b_Intercept))

forest.data.summary_overall <- group_by(forest.data_overall, author1)|> 
  mean_qi(b_Intercept) 

res_plot <- forest.data.summary_overall|>
  mutate(weighted_effect = paste0(sprintf('%.2f', b_Intercept), 
                                  ' [', sprintf('%.2f', .lower),
                                  ', ', sprintf('%.2f', .upper), ']'),
         unweighted_effect = paste0("Mean τ = ", 
                                    sprintf('%.2f', mean(post.samples_overall$sd_author1__Intercept))),
         n_int1_tot = as.character(sum(data$n_int1)),
         n_int2_tot = as.character(sum(data$n_int2)),
         truncal_mean_sd = "",
         control_mean_sd = "",
         colour = "grey60")

new_row <- data.frame(author1 = as.factor("Overall"),
                      weighted_effect = "",
                      unweighted_effect = "",
                      n_int1_tot = "",
                      n_int2_tot = "",
                      truncal_mean_sd = "",
                      control_mean_sd = "",
                      colour = "black")

res_plot_overall <- bind_rows(res_plot, new_row)

## Now make the overall plot

p_forest_overall <- ggplot(aes(b_Intercept, 
                               relevel(author1, "Pooled Effect", after=Inf)), 
                           data = forest.data_overall) +
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(brm_overall)[1, 1], 
             color = "grey60", size = 1) +
  geom_vline(xintercept = fixef(brm_overall)[1, 3:4], 
             color = "grey60", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  # Add densities
  geom_density_ridges(fill = "#008cba", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 0.9,
                      alpha = 0.8) +
  geom_pointinterval(data = forest.data.summary_overall, 
                     linewidth = 1, color = "blue",
                     position = position_nudge(y = -0.11),
                     aes(xmin = .lower, xmax = .upper)) +
  scale_x_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6), expand = c(0,0)) +
  coord_cartesian(xlim=c(-6,6), ylim=c(1,3)) +
  theme_light() +
  theme(axis.text.y = element_blank()) +
  labs(x="Mean difference") +
  ylab(NULL)

p_estimates_overall <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), colour = colour), 
                              data = res_plot_overall) +
  geom_text(aes(x = 0, label = weighted_effect), hjust = 0) + 
  geom_text(aes(x = 1, label = unweighted_effect), hjust = 0) +
  theme_void() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0, 2), ylim=c(1,3))

p_studies_overall <- ggplot(aes(y = relevel(author1, "Pooled Effect", after=Inf), colour = colour), 
                            data = res_plot_overall) +
  geom_text(aes(x = 0, label = author1), hjust = 0, fontface = ifelse(res_plot_overall$author1 == "Overall", "bold",
                                                                      ifelse(res_plot_overall$author1 == "Pooled Effect", "italic","plain"))) +
  geom_text(aes(x = 1.5, label = n_int2_tot), hjust = 0) +
  geom_text(aes(x = 2, label = truncal_mean_sd), hjust = 0) +
  geom_text(aes(x = 3, label = n_int1_tot), hjust = 0) +
  geom_text(aes(x = 3.5, label = control_mean_sd), hjust = 0) +
  theme_void() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0,4))

}

library(patchwork)

## Create a layout that we will pass to the patchwork package to size all our plots
## The vertical axis needs to be adjusted depending on the number of studies in each subgroup
layout <- c(
  area(t = 0, l = 0, b = 16, r = 45),
  area(t = 0, l = 45, b = 16, r = 65), 
  area(t = 0, l = 67, b = 16, r = 88),
  area(t = 17, l = 0, b = 36, r = 45),
  area(t = 17, l = 45, b = 36, r = 65), 
  area(t = 17, l = 67, b = 36, r = 88),
  area(t = 37, l = 0, b = 52, r = 45),
  area(t = 37, l = 45, b = 52, r = 65), 
  area(t = 37, l = 67, b = 52, r = 88),
  area(t = 53, l = 0, b = 61, r = 45),
  area(t = 53, l = 45, b = 61, r = 65), 
  area(t = 53, l = 67, b = 61, r = 88),
  area(t = 62, l = 0, b = 64, r = 45),
  area(t = 62, l = 45, b = 64, r = 65), 
  area(t = 62, l = 67, b = 64, r = 88))


# Finally, combine all the plots together
final_plot <- function(df){
  
  df$plot$p_studies_control + df$plot$p_forest_control + df$plot$p_estimates_control + 
    df$plot$p_studies_placebo + df$plot$p_forest_placebo + df$plot$p_estimates_placebo + 
    df$plot$p_studies_tea + df$plot$p_forest_tea + df$plot$p_estimates_tea +
    df$plot$p_studies_wound + df$plot$p_forest_wound + df$plot$p_estimates_wound +
    df$plot$p_studies_overall + df$plot$p_forest_overall + df$plot$p_estimates_overall + plot_layout(design = layout)
}

# Dynamic Pain at 24 Hours
forest_dyn_NRS_24h <- bayes_forest(dynamic_nrs_24, "dynamic_pain_mean_24h_int1", "dynamic_pain_mean_24h_int2", "dynamic_pain_sd_24h_int1", "dynamic_pain_sd_24h_int2")
final_plot(forest_dyn_NRS_24h)

# Dynamic Pain at 48 Hours
forest_dyn_NRS_48h <- bayes_forest(dynamic_nrs_48, "dynamic_pain_mean_48h_int1", "dynamic_pain_mean_48h_int2", "dynamic_pain_sd_48h_int1", "dynamic_pain_sd_48h_int2")
final_plot(forest_dyn_NRS_48h)

# OME at 24 Hours
forest_ome_24h <- bayes_forest(ome_24, "ome_mean_24h_int1", "ome_mean_24h_int2", "ome_sd_24h_int1", "ome_sd_24h_int2")
final_plot(forest_ome_24h)

forest_ome_48h <- bayes_forest(ome_48, "ome_mean_48h_int1", "ome_mean_48h_int2", "ome_sd_48h_int1", "ome_sd_48h_int2")
final_plot(forest_ome_48h)

brm(yi | se(sei) ~ 1 + (1 | author),
                  data = dynamic_nrs_48,
                  prior = priors,
                  iter = 4000,
                  cores = detectCores(),
                  control = list(adapt_delta = 0.99),
                  backend = "cmdstanr")

forest_dyn_nrs_24h <- bayes_forest(dynamic_nrs_24_ss, "dynamic_pain_mean_24h_int1", "dynamic_pain_mean_24h_int2", "dynamic_pain_sd_24h_int1", "dynamic_pain_sd_24h_int2")
