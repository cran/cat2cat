## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  collapse = TRUE, comment = "#>"
)

## ----load-data, echo=FALSE----------------------------------------------------
library(cat2cat)
data(trans, package = "cat2cat")

## ----show-problem-------------------------------------------------------------
# Old occupation code "1111" became three different codes in the new system
trans[trans$old == "1111", ]

## ----use-case-agg-------------------------------------------------------------
library(dplyr)
data(verticals, package = "cat2cat")

agg_old <- verticals[verticals$v_date == "2020-04-01", ]
agg_new <- verticals[verticals$v_date == "2020-05-01", ]

agg_result <- cat2cat_agg(
  data = list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "v_date",
    freq_var = "counts"
  ),
  # Backward mapping: old Automotive split into Automotive1 + Automotive2
  Automotive %<% c(Automotive1, Automotive2),
  # Forward mapping: Kids1 + Kids2 merged into Kids  
  c(Kids1, Kids2) %>% c(Kids)
)

agg_result$old[c("vertical", "prop_c2c", "counts")]

## ----direction-comparison-----------------------------------------------------
# Setup for comparison
occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]

# Backward: old period gets replicated onto new codes
backward <- cat2cat(
  data = list(old = occup_2008, new = occup_2010, 
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

# Forward: new period gets replicated onto old codes
forward <- cat2cat(
  data = list(old = occup_2008, new = occup_2010, 
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "forward")
)

# Which period gets replicated depends on direction
cat("Backward: old period replicated from", nrow(occup_2008), "to", nrow(backward$old), "rows\n")
cat("Forward: new period replicated from", nrow(occup_2010), "to", nrow(forward$new), "rows")

## ----quick-example------------------------------------------------------------
data(occup, package = "cat2cat")

occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]

result_back <- cat2cat(
  data = list(
    old = occup_2008,
    new = occup_2010,
    cat_var = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward")
)

## ----show-replication---------------------------------------------------------
# A replicated observation (rep_c2c > 1 means replicated)
result_back$old[result_back$old$rep_c2c > 1, ][1:3,
                c("code", "g_new_c2c", "wei_freq_c2c", "rep_c2c")]

## ----forward-example----------------------------------------------------------
result_forward <- cat2cat(
  data = list(
    old = occup_2008,
    new = occup_2010,
    cat_var = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans, direction = "forward")
)

result_forward$new[result_forward$new$rep_c2c > 1, ][1:3,
                   c("code", "g_new_c2c", "wei_freq_c2c", "rep_c2c")]

## ----naive-vs-freq------------------------------------------------------------
# Compare weights for a replicated observation
result_back$old[result_back$old$rep_c2c > 1, ][1:3,
                c("g_new_c2c", "wei_naive_c2c", "wei_freq_c2c")]

# Same analysis with naive weights (robustness check)
c(freq_mean = weighted.mean(result_back$old$salary, result_back$old$wei_freq_c2c),
  naive_mean = weighted.mean(result_back$old$salary, result_back$old$wei_naive_c2c))

## ----pooled-regression--------------------------------------------------------
harmonised_two_period <- dplyr::bind_rows(result_back$old, result_back$new)

harmonised_two_period <- harmonised_two_period %>%
  dplyr::group_by(g_new_c2c) %>%
  dplyr::mutate(avg_age_g_new_c2c = mean(age, na.rm = TRUE)) %>%
  dplyr::ungroup()

pooled_model <- lm(
  log(salary) ~ factor(year) + age + exp + avg_age_g_new_c2c,
  data = harmonised_two_period,
  weights = multiplier * wei_freq_c2c
)

pooled_summary <- summary_c2c(
  pooled_model,
  df_old = nrow(occup_2008) + nrow(occup_2010) - length(coef(pooled_model))
)

pooled_summary[c("factor(year)2010", "age", "avg_age_g_new_c2c"),
               c("Estimate", "std.error_c", "p.value_c")]

## ----plot, fig.width=7, fig.height=3.5, fig.alt="Diagnostic plot displaying weight histograms and replication statistics for mapped observations"----
plot_c2c(result_back$old, type = "both")

## ----hierarchical-codes-------------------------------------------------------
head(trans, 5)

# Build a 3-digit mapping from the full codes
trans_3digit <- data.frame(
  old = substr(trans$old, 1, 3),
  new = substr(trans$new, 1, 3)
)
trans_3digit <- unique(trans_3digit)
cat("3-digit mapping rows:", nrow(trans_3digit),
    "vs full mapping rows:", nrow(trans))

