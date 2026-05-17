## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  collapse = TRUE, comment = "#>"
)

## ----load-data----------------------------------------------------------------
library(cat2cat)
library(dplyr)
library(tidyr)
library(e1071)
library(randomForest)

data(occup, package = "cat2cat")
data(trans, package = "cat2cat")

occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]

## ----mixed-ml-weights---------------------------------------------------------
occup_2_mix <- cat2cat(
  data = list(
    old = occup_2008, new = occup_2010,
    cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_2010,
    cat_var = "code",
    method = c("knn", "rf", "lda", "nb"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 50),
    on_fail = "na"
  )
)

## ----weight-correlations------------------------------------------------------
occup_2_mix$old %>%
  select(wei_knn_c2c, wei_rf_c2c, wei_lda_c2c, wei_nb_c2c, wei_freq_c2c, wei_naive_c2c) %>%
    cor(use = "pairwise.complete.obs")

## ----ml-failure-policy, eval=FALSE--------------------------------------------
# ml_setup <- list(
#   data = bind_rows(occup_2010, occup_2012),
#   cat_var = "code",
#   method = c("knn", "rf", "lda"),
#   features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#   args = list(k = 10, ntree = 50),
#   on_fail = "freq",   # default policy
#   fail_warn = TRUE     # default reporting
# )
# 
# # strict mode for QA pipelines
# ml_strict <- ml_setup
# ml_strict$on_fail <- "error"
# 
# # diagnostic mode to inspect failures directly
# ml_diag <- ml_setup
# ml_diag$on_fail <- "na"
# ml_diag$fail_warn <- FALSE

## ----ensemble-prune-----------------------------------------------------------
occup_old_mix <- occup_2_mix$old %>%
  cross_c2c(.) %>%
  prune_c2c(., column = "wei_cross_c2c", method = "nonzero")

## ----sensitivity-result-------------------------------------------------------
result_all <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_2010, cat_var = "code",
    method = c("knn", "rf", "lda", "nb"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 50)
  )
)

## ----sensitivity-counts-------------------------------------------------------
weight_cols <- c("wei_naive_c2c", "wei_freq_c2c", "wei_knn_c2c", "wei_rf_c2c", "wei_lda_c2c", "wei_nb_c2c")

# Pick groups with high replication
top_groups <- result_all$old %>%
  filter(rep_c2c > 1) %>%
  count(g_new_c2c, sort = TRUE) %>%
  head(6) %>%
  pull(g_new_c2c)

# Weighted counts from OLD period (replicated)
old_counts <- lapply(weight_cols, function(wcol) {
  result_all$old %>%
    filter(g_new_c2c %in% top_groups) %>%
    group_by(g_new_c2c) %>%
    summarise(n = sum(.data[[wcol]]), .groups = "drop")
}) %>%
  setNames(gsub("wei_|_c2c", "", weight_cols)) %>%
  bind_rows(.id = "method") %>%
  tidyr::pivot_wider(names_from = method, values_from = n)

# Counts from NEW period (no replication, exact)
new_counts <- result_all$new %>%
  filter(code %in% top_groups) %>%
  count(code, name = "new_period") %>%
  rename(g_new_c2c = code)

# Combine for comparison
left_join(old_counts, new_counts, by = "g_new_c2c")

## ----sensitivity-weights------------------------------------------------------
# New-period counts per category (no replication, so plain tally)
new_counts_all <- result_all$new %>%
  count(code, name = "n_new") %>%
  rename(g_new_c2c = code)

# Old-period weighted counts, joined to new-period counts
group_sizes <- result_all$old %>%
  group_by(g_new_c2c) %>%
  summarise(n_old = sum(wei_freq_c2c), .groups = "drop") %>%
  left_join(new_counts_all, by = "g_new_c2c") %>%
  filter(n_old >= 10, n_new >= 10) %>%
  arrange(desc(n_old))

# Pick a group for regression analysis
target_group <- group_sizes$g_new_c2c[1]
cat("Analysing occupation group:", target_group, "\n")

## ----sensitivity-group-reg----------------------------------------------------
# Subset old period to target group (with weights)
old_subset <- result_all$old %>%
  filter(g_new_c2c == target_group)

# Subset new period to target group (no replication, weight = 1)
new_subset <- result_all$new %>%
  filter(code == target_group) %>%
  mutate(
    wei_naive_c2c = 1, wei_freq_c2c = 1, wei_knn_c2c = 1,
    wei_rf_c2c = 1, wei_lda_c2c = 1, wei_nb_c2c = 1
  )

# Combine both periods
d <- bind_rows(old_subset, new_subset)

# Compare all regression coefficients across weight methods
f <- I(log(salary)) ~ age + sex + factor(edu) + exp + parttime

coefs <- sapply(weight_cols, function(wcol) {
  d$w <- d$multiplier * d[[wcol]]
  coef(lm(f, data = d, weights = w))
})
colnames(coefs) <- gsub("wei_|_c2c", "", weight_cols)
round(coefs, 4)

## ----sensitivity-pruning------------------------------------------------------
# Compare regression coefficients under different pruning strategies
prune_methods <- c("nonzero", "highest", "highest1")

prune_coefs <- sapply(prune_methods, function(pm) {
  old_pruned <- result_all$old %>%
    prune_c2c(method = pm) %>%
    filter(g_new_c2c == target_group)
  
  d <- bind_rows(old_pruned, new_subset)
  d$w <- d$multiplier * d$wei_freq_c2c
  coef(lm(f, data = d, weights = w))
})
round(prune_coefs, 4)

## ----sensitivity-ensemble-----------------------------------------------------
configs <- list(
  equal      = c(1, 1) / 2,
  freq_heavy = c(3, 1) / 4,
  ml_heavy   = c(1, 3) / 4
)

ens_coefs <- sapply(names(configs), function(nm) {
  old_ens <- result_all$old %>%
    cross_c2c(c("wei_freq_c2c", "wei_knn_c2c"), configs[[nm]]) %>%
    filter(g_new_c2c == target_group)
  
  new_ens <- new_subset %>% mutate(wei_cross_c2c = 1)
  d <- bind_rows(old_ens, new_ens)
  d$w <- d$multiplier * d$wei_cross_c2c
  coef(lm(f, data = d, weights = w))
})
round(ens_coefs, 4)

## ----cv-basic-----------------------------------------------------------------
cv_knn <- cat2cat_ml_run(
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = bind_rows(occup_2010, occup_2012),
    cat_var = "code",
    method = "knn",
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)
print(cv_knn)

## ----cv-multiple--------------------------------------------------------------
cv_all <- cat2cat_ml_run(
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = bind_rows(occup_2010, occup_2012),
    cat_var = "code",
    method = c("knn", "lda", "rf", "nb"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 50)
  )
)
print(cv_all)

## ----cv-inspect---------------------------------------------------------------
# Pick a group with multiple candidates
group_names <- names(cv_all)
example_group <- group_names[
  which(vapply(cv_all, function(g) !is.na(g$freq) && g$naive < 1, logical(1)))[1]
]
cv_all[[example_group]]

