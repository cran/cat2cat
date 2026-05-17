## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  collapse = TRUE, comment = "#>"
)

## ----load-data----------------------------------------------------------------
library(cat2cat)
library(dplyr)
library(tidyr)
library(fixest)

data(occup, package = "cat2cat")
data(occup_panel, package = "cat2cat")
data(trans, package = "cat2cat")
data(verticals, package = "cat2cat")
data(verticals2, package = "cat2cat")

occup_2006 <- occup[occup$year == 2006, ]
occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]

## ----ml-minimal, eval=FALSE---------------------------------------------------
# ml_setup <- list(
#   data = bind_rows(occup_2010, occup_2012),
#   cat_var = "code",
#   method = c("knn", "rf", "lda"),
#   features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#   args = list(k = 10, ntree = 50),
#   on_fail = "freq",
#   fail_warn = TRUE
# )
# 
# result_ml <- cat2cat(
#   data = list(old = occup_2008, new = occup_2010,
#               cat_var = "code", time_var = "year"),
#   mappings = list(trans = trans, direction = "backward"),
#   ml = ml_setup
# )

## ----ml-validate, eval=FALSE--------------------------------------------------
# cv <- cat2cat_ml_run(
#   mappings = list(trans = trans, direction = "backward"),
#   ml = ml_setup
# )
# print(cv)

## ----baseline-only-validate, eval=FALSE---------------------------------------
# ml_baseline <- list(
#   data = bind_rows(occup_2010, occup_2012),
#   cat_var = "code",
#   method = character(0),
#   features = character(0)
# )
# 
# cv_baseline <- cat2cat_ml_run(
#   mappings = list(trans = trans, direction = "backward"),
#   ml = ml_baseline
# )
# print(cv_baseline)

## ----trans-stability----------------------------------------------------------
max_digits <- max(nchar(as.character(trans[[1]])), nchar(as.character(trans[[2]])))

stability <- sapply(1:max_digits, function(d) {
  old_trunc <- substr(as.character(trans[[1]]), 1, d)
  new_trunc <- substr(as.character(trans[[2]]), 1, d)
  mean(old_trunc != new_trunc) * 100
})

data.frame(
  digits = 1:max_digits,
  pct_changed = round(stability, 1)
)

## ----truncation-diagnostic----------------------------------------------------
occup_2008_trunc <- occup_2008
occup_2008_trunc$code <- substr(occup_2008_trunc$code, 1, 4)
occup_2010_trunc <- occup_2010
occup_2010_trunc$code <- substr(occup_2010_trunc$code, 1, 4)
trans_trunc <- unique(data.frame(
  old = substr(trans$old, 1, 4),
  new = substr(trans$new, 1, 4)
))

back_full <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)
back_trunc <- cat2cat(
  data = list(old = occup_2008_trunc, new = occup_2010_trunc,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans_trunc, direction = "backward")
)

fwd_full <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "forward")
)
fwd_trunc <- cat2cat(
  data = list(old = occup_2008_trunc, new = occup_2010_trunc,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans_trunc, direction = "forward")
)

data.frame(
  mapping = c("backward full (4->6)", "backward trunc (4->4)",
              "forward full (6->4)", "forward trunc (4->4)"),
  mean_rep = c(mean(back_full$old$rep_c2c), mean(back_trunc$old$rep_c2c),
               mean(fwd_full$new$rep_c2c), mean(fwd_trunc$new$rep_c2c))
)

## ----backward-chain-----------------------------------------------------------
step1 <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

step2 <- cat2cat(
  data = list(old = occup_2006, new = step1$old,
              cat_var_old = "code", cat_var_new = "g_new_c2c",
              time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

harmonised_back <- bind_rows(
  step2$old,
  step1$old,
  step1$new,
  dummy_c2c(occup_2012, "code")
)

## ----validate-backward-counts-------------------------------------------------
harmonised_back %>%
  group_by(year) %>%
  summarise(weighted_n = round(sum(wei_freq_c2c)), .groups = "drop") %>%
  left_join(count(occup, year), by = "year")

## ----forward-chain------------------------------------------------------------
trans_fwd <- rbind(
  trans,
  data.frame(old = "no_cat",
             new = setdiff(c(occup_2010$code, occup_2012$code), trans$new))
)

fwd1 <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans_fwd, direction = "forward")
)

fwd2 <- cat2cat(
  data = list(old = fwd1$new, new = occup_2012,
              cat_var_old = "g_new_c2c", cat_var_new = "code",
              time_var = "year"),
  mappings = list(trans = trans_fwd, direction = "forward")
)

harmonised_fwd <- bind_rows(
  dummy_c2c(occup_2006, "code"),
  fwd1$old,
  fwd1$new,
  fwd2$new
)

## ----ml-chain, eval=FALSE-----------------------------------------------------
# step1_ml <- cat2cat(
#   data = list(old = occup_2008, new = occup_2010,
#               cat_var = "code", time_var = "year"),
#   mappings = list(trans = trans, direction = "backward"),
#   ml = ml_setup
# )
# 
# step2_ml <- cat2cat(
#   data = list(old = occup_2006, new = step1_ml$old,
#               cat_var_old = "code", cat_var_new = "g_new_c2c",
#               time_var = "year"),
#   mappings = list(trans = trans, direction = "backward"),
#   ml = ml_setup
# )

## ----panel-load---------------------------------------------------------------
panel_old <- occup_panel[occup_panel$quarter == "2009Q4", ]
panel_new <- occup_panel[occup_panel$quarter == "2010Q1", ]
shared_ids <- intersect(panel_old$panel_id, panel_new$panel_id)
length(shared_ids)

## ----idvar-mapping------------------------------------------------------------
result_id <- cat2cat(
  data = list(
    old = panel_old,
    new = panel_new,
    id_var = "panel_id",
    cat_var = "code",
    time_var = "quarter"
  ),
  mappings = list(trans = trans, direction = "backward")
)

## ----idvar-inspect------------------------------------------------------------
table(result_id$old$rep_c2c)
sum(result_id$old$wei_freq_c2c)
nrow(panel_old)

## ----idvar-compare------------------------------------------------------------
result_no_id <- cat2cat(
  data = list(
    old = panel_old,
    new = panel_new,
    cat_var = "code",
    time_var = "quarter"
  ),
  mappings = list(trans = trans, direction = "backward")
)

cat("WITH id_var average replication:", round(mean(result_id$old$rep_c2c), 2), "\n")
cat("WITHOUT id_var average replication:", round(mean(result_no_id$old$rep_c2c), 2), "\n")

## ----agg-data-----------------------------------------------------------------
agg_old <- verticals[verticals$v_date == "2020-04-01", ]
agg_new <- verticals[verticals$v_date == "2020-05-01", ]

## ----agg-mapping--------------------------------------------------------------
agg <- cat2cat_agg(
  data = list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "v_date",
    freq_var = "counts"
  ),
  Automotive %<% c(Automotive1, Automotive2),
  c(Kids1, Kids2) %>% c(Kids),
  Home %>% c(Home, Supermarket)
)

## ----agg-inspect--------------------------------------------------------------
agg$old[agg$old$vertical %in% c("Automotive1", "Automotive2"), ]
agg$new[agg$new$vertical %in% c("Kids1", "Kids2"), ]

## ----hierarchical-codes-------------------------------------------------------
trans_2digit <- data.frame(
  old = substr(trans$old, 1, 2),
  new = substr(trans$new, 1, 2)
)
trans_2digit <- unique(trans_2digit)

cat("2-digit mapping rows:", nrow(trans_2digit),
    "vs full mapping rows:", nrow(trans))

## ----build-panel--------------------------------------------------------------
cat2cat_data_back <- bind_rows(
  step2$old,
  step1$old,
  step1$new,
  dummy_c2c(occup_2012, "code")
)

## ----regression-neutral-------------------------------------------------------
lms_orig <- lm(
  I(log(salary)) ~ age + sex + factor(edu) + parttime + exp,
  data = occup,
  weights = multiplier
)

lms_harmonised <- lm(
  I(log(salary)) ~ age + sex + factor(edu) + parttime + exp,
  data = cat2cat_data_back,
  weights = multiplier * wei_freq_c2c
)

summary_c2c(lms_harmonised, df_old = nrow(occup))

## ----regression-fe------------------------------------------------------------
harmonised_fe <- cat2cat_data_back %>%
  prune_c2c(method = "nonzero") %>%
  mutate(orig_obs_id = interaction(year, index_c2c, drop = TRUE, lex.order = TRUE)) %>%
  filter(!is.na(g_new_c2c), !is.na(salary), salary > 0)

fe_model_cluster <- feols(
  log(salary) ~ age + sex + factor(edu) + parttime + exp | g_new_c2c + year,
  data = harmonised_fe,
  weights = ~multiplier * wei_freq_c2c,
  cluster = ~orig_obs_id
)
summary(fe_model_cluster)

