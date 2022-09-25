## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = T)
knitr::opts_chunk$set(size = "tiny")
knitr::opts_chunk$set(message = F)
knitr::opts_chunk$set(warning = F)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library("cat2cat")
library("dplyr")

data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_2006 <- occup[occup$year == 2006, ]
occup_2008 <- occup_old <- occup[occup$year == 2008, ]
occup_2010 <- occup_new <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]

## -----------------------------------------------------------------------------
## cat2cat
occup_simple <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward")
)

## with informative features it might be usefull to run ml algorithm
## currently knn, lda and rf (randomForest), could be a few at once
## where probability will be assessed as fraction of closest points.
occup_2 <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_new,
    cat_var = "code",
    method = "knn",
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

## -----------------------------------------------------------------------------
# summary_plot
plot_c2c(occup_2$old, type = c("both"))

## -----------------------------------------------------------------------------
# mix of methods
occup_2_mix <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_new,
    cat_var = "code",
    method = c("knn", "rf", "lda"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 50)
  )
)
# cross all methods and subset one highest probability category for each subject
occup_old_mix_highest1 <- occup_2_mix$old %>%
  cross_c2c(.) %>%
  prune_c2c(., column = "wei_cross_c2c", method = "highest1")

## -----------------------------------------------------------------------------
# correlation between ml models and simple fequencies
occup_2_mix$old %>%
  select(wei_knn_c2c, wei_rf_c2c, wei_lda_c2c, wei_freq_c2c) %>%
  cor()

## -----------------------------------------------------------------------------
# 2010 -> 2008
occup_back_2008_2010 <- cat2cat(
  data = list(
    old = occup_2008, new = occup_2010, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward")
)

# optional, give more control
# the counts could be any of wei_* or their combination
freqs_df <- occup_back_2008_2010$old[, c("g_new_c2c", "wei_freq_c2c")] %>%
  group_by(g_new_c2c) %>%
  summarise(counts = round(sum(wei_freq_c2c)))

# 2008 -> 2006
occup_back_2006_2008 <- cat2cat(
  data = list(
    old = occup_2006,
    new = occup_back_2008_2010$old,
    cat_var_new = "g_new_c2c",
    cat_var_old = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward", freqs_df = freqs_df)
)

occup_2006_new <- occup_back_2006_2008$old
occup_2008_new <- occup_back_2008_2010$old # or occup_back_2006_2008$new
occup_2010_new <- occup_back_2008_2010$new
# use ml argument when applied ml models
occup_2012_new <- dummy_c2c(occup_2012, "code")

final_data_back <- do.call(
  rbind, 
  list(occup_2006_new, occup_2008_new, occup_2010_new, occup_2012_new)
)

## -----------------------------------------------------------------------------
# We persist the number of observations
counts_new <- final_data_back %>%
  cross_c2c() %>%
  group_by(year) %>%
  summarise(
    n = as.integer(round(sum(wei_freq_c2c))),
    n2 = as.integer(round(sum(wei_cross_c2c)))
  )

counts_old <- occup %>%
  group_by(year) %>%
  summarise(n = n(), n2 = n(), .groups = "drop")

identical(counts_new, counts_old)

# counts per each level
counts_per_level <- final_data_back %>%
  group_by(year, g_new_c2c) %>%
  summarise(n = sum(wei_freq_c2c), .groups = "drop") %>%
  arrange(g_new_c2c, year)

## -----------------------------------------------------------------------------
trans2 <- rbind(trans, 
                data.frame(
                  old = "no_cat",
                  new = setdiff(c(occup_2010$code, occup_2012$code), trans$new)
                ))

## -----------------------------------------------------------------------------
# 2008 -> 2010
occup_for_2008_2010 <- cat2cat(
  data = list(
    old = occup_2008, new = occup_2010, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans2, direction = "forward")
)

# optional, give more control
# the counts could be any of wei_* or their combination
freqs_df <- occup_for_2008_2010$new[, c("g_new_c2c", "wei_freq_c2c")] %>%
  group_by(g_new_c2c) %>%
  summarise(counts = round(sum(wei_freq_c2c)))

# 2010 -> 2012
occup_for_2010_2012 <- cat2cat(
  data = list(
    old = occup_for_2008_2010$new,
    new = occup_2012,
    cat_var_old = "g_new_c2c",
    cat_var_new = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans2, direction = "forward", freqs_df = freqs_df)
)

# use ml argument when applied ml models
occup_2006_new <- dummy_c2c(occup_2006, "code")
occup_2008_new <- occup_for_2008_2010$old
occup_2010_new <- occup_for_2008_2010$new # or occup_for_2010_2012$old
occup_2012_new <- occup_for_2010_2012$new

final_data_for <- do.call(
  rbind, 
  list(occup_2006_new, occup_2008_new, occup_2010_new, occup_2012_new)
)

## -----------------------------------------------------------------------------
# We persist the number of observations
counts_new <- final_data_for %>%
  cross_c2c() %>%
  group_by(year) %>%
  summarise(
    n = as.integer(round(sum(wei_freq_c2c))),
    n2 = as.integer(round(sum(wei_cross_c2c)))
  )

counts_old <- occup %>%
  group_by(year) %>%
  summarise(n = n(), n2 = n(), .groups = "drop")

identical(counts_new, counts_old)

# counts per each level
counts_per_level <- final_data_for %>%
  group_by(year, g_new_c2c) %>%
  summarise(n = sum(wei_freq_c2c), .groups = "drop") %>%
  arrange(g_new_c2c, year)

## -----------------------------------------------------------------------------
# 2010 -> 2008
occup_back_2008_2010 <- cat2cat(
  data = list(
    old = occup_2008, new = occup_2010, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = dplyr::bind_rows(occup_2010, occup_2012),
    cat_var = "code",
    method = c("knn"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

# 2008 -> 2006
occup_back_2006_2008 <- cat2cat(
  data = list(
    old = occup_2006,
    new = occup_back_2008_2010$old,
    cat_var_new = "g_new_c2c",
    cat_var_old = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = dplyr::bind_rows(occup_2010, occup_2012),
    cat_var = "code",
    method = c("knn"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

occup_2006_new <- occup_back_2006_2008$old
occup_2008_new <- occup_back_2008_2010$old # or occup_back_2006_2008$new
occup_2010_new <- occup_back_2008_2010$new
occup_2012_new <- dummy_c2c(occup_2012, cat_var = "code", ml = c("knn"))

final_data_back_ml <- do.call(
  rbind, 
  list(occup_2006_new, occup_2008_new, occup_2010_new, occup_2012_new)
)

## -----------------------------------------------------------------------------
counts_new <- final_data_back_ml %>%
  cross_c2c() %>%
  group_by(year) %>%
  summarise(
    n = as.integer(round(sum(wei_freq_c2c))),
    n2 = as.integer(round(sum(wei_cross_c2c))),
    .groups = "drop"
  )

counts_old <- occup %>%
  group_by(year) %>%
  summarise(n = n(), n2 = n(), .groups = "drop")

identical(counts_new, counts_old)

# counts per each level
counts_per_level <- final_data_back_ml %>%
  group_by(year, g_new_c2c) %>%
  summarise(n = sum(wei_freq_c2c), .groups = "drop") %>%
  arrange(g_new_c2c, year)

## -----------------------------------------------------------------------------
ff <- final_data_back_ml %>%
  split(.$year) %>%
  lapply(function(x) {
    x %>%
      cross_c2c() %>%
      prune_c2c(column = "wei_cross_c2c", method = "highest1")
  }) %>%
  bind_rows()
all.equal(nrow(ff), sum(final_data_back_ml$wei_freq_c2c))

## -----------------------------------------------------------------------------
## orginal dataset
lms2 <- lm(
  I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old,
  weights = multiplier
)
summary(lms2)

## using one highest cross weights
## cross_c2c to cross differen methods weights
## prune_c2c
## highest1 leave only one the highest probability obs for each subject
occup_old_2 <- occup_2$old %>%
  cross_c2c(., c("wei_freq_c2c", "wei_knn_c2c"), c(1 / 2, 1 / 2)) %>%
  prune_c2c(., column = "wei_cross_c2c", method = "highest1")
lms <- lm(
  I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old_2,
  weights = multiplier
)
summary(lms)

## we have to adjust size of stds as we artificialy enlarge degrees of freedom
occup_old_3 <- occup_2$old %>%
  prune_c2c(method = "nonzero") # many prune methods like highest
lms_replicated <- lm(
  I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old_3,
  weights = multiplier * wei_freq_c2c
)
# Adjusted R2 is meaningless here
lms_replicated$df.residual <- nrow(occup_old) - length(lms_replicated$assign)
suppressWarnings(summary(lms_replicated))

## -----------------------------------------------------------------------------
formula_oo <- formula(
  I(log(salary)) ~ age + sex + factor(edu) + parttime + exp + factor(year)
)
oo <- final_data_back %>%
  prune_c2c(method = "nonzero") %>% # many prune methods like highest
  group_by(g_new_c2c) %>%
  filter(n() >= 15) %>% 
  do(
    lm = tryCatch(
      summary(lm(formula_oo, ., weights = multiplier * wei_freq_c2c)),
      error = function(e) NULL
    )
  ) %>%
  filter(!is.null(lm))

head(oo)

oo$lm[[2]]

## -----------------------------------------------------------------------------
library("cat2cat")
data("verticals", package = "cat2cat")

agg_old <- verticals[verticals$v_date == "2020-04-01", ]
agg_new <- verticals[verticals$v_date == "2020-05-01", ]

## cat2cat_agg - could map in both directions at once although
## usually we want to have old or new representation
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

## possible processing
library("dplyr")
agg %>%
    bind_rows() %>%
    group_by(v_date, vertical) %>%
    summarise(sales = sum(sales * prop_c2c),
              counts = sum(counts * prop_c2c),
              v_date = first(v_date), 
              .groups = "drop")

## -----------------------------------------------------------------------------
library(cat2cat)
## the ean variable is a unique identifier
data("verticals2", package = "cat2cat")

vert_old <- verticals2[verticals2$v_date == "2020-04-01", ]
vert_new <- verticals2[verticals2$v_date == "2020-05-01", ]

## get mapping (transition) table
trans_v <- vert_old %>%
  inner_join(vert_new, by = "ean") %>%
  select(vertical.x, vertical.y) %>%
  distinct()

## -----------------------------------------------------------------------------
## cat2cat
## it is important to set id_var as then we merging categories 1 to 1
## for this identifier which exists in both periods.
verts <- cat2cat(
  data = list(old = vert_old, new = vert_new, id_var = "ean",
              cat_var = "vertical", time_var = "v_date"),
  mappings = list(trans = trans_v, direction = "backward")
)

