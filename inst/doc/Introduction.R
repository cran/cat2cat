## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = T)
knitr::opts_chunk$set(size = "tiny")
knitr::opts_chunk$set(message = F)
knitr::opts_chunk$set(warning = F)

## -----------------------------------------------------------------------------
pacman::p_load(cat2cat, dplyr, igraph)

## -----------------------------------------------------------------------------
data(occup)

data(trans)

## -----------------------------------------------------------------------------
occup %>% glimpse()

## -----------------------------------------------------------------------------
trans %>% glimpse()

## -----------------------------------------------------------------------------
gg = graph_from_data_frame(trans[1:40, ])
plot.igraph(gg)

## -----------------------------------------------------------------------------
mappings <- get_mappings(trans)

mappings$to_old[1:4]

mappings$to_new[1:4]

# occup$multiplier part inside get_freqs is optional
# this variable specifying how many times to replicate each observation to get reliable population
mapp_p <- cat_apply_freq(mappings$to_old, get_freqs(occup$code4[occup$year == "2008"], occup$multiplier[occup$year == "2008"]))

data.frame(I(mappings$to_old), I(mapp_p)) %>% head()

mapp_p <- cat_apply_freq(mappings$to_new, get_freqs(occup$code4[occup$year == "2010"], occup$multiplier[occup$year == "2010"]))

data.frame(I(mappings$to_new), I(mapp_p)) %>% head()

## -----------------------------------------------------------------------------
occup_old = occup[occup$year == 2008,]
occup_new = occup[occup$year == 2010,]

## -----------------------------------------------------------------------------
cat2cat(
  data = list(old = occup_old ,new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "forward")
  )

## -----------------------------------------------------------------------------
occup_2 = cat2cat(
  data = list(old = occup_old ,new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "forward"),
  ml = list(method = "knn", features = c("age", "sex", "edu", "exp", "parttime", "salary"), args = list(k = 10))
  )

## -----------------------------------------------------------------------------
occup_2 %>% glimpse()

## ---- size="tiny"-------------------------------------------------------------
occup_3 <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = "knn", features = c("age", "sex", "edu", "exp", "parttime", "salary"), args = list(k = 10))
)

## -----------------------------------------------------------------------------
cor(occup_3$old[, grepl("wei.*c2c", colnames(occup_3$old))], use = "complete.obs")

## -----------------------------------------------------------------------------
plot_c2c(occup_3$old, type = c("both"))

## -----------------------------------------------------------------------------
occup_3 %>% glimpse()

## -----------------------------------------------------------------------------
get_freqs(x = occup_3$new$g_new_c2c, multiplier = floor(occup_3$new$multiplier  * occup_3$new$wei_freq_c2c)) %>% head()

## -----------------------------------------------------------------------------
get_freqs(x = occup_2$old$g_new_c2c, multiplier  = floor(occup_2$old$multiplier  * occup_2$old$wei_freq_c2c))  %>% head()

## -----------------------------------------------------------------------------
## orginal dataset 
lms2 <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old, weights = multiplier)
summary(lms2)
## using one highest cross weights
## cross_c2c to cross different methods weights
## prune_c2c - highest1 leave only one the highest probability obs for each subject
occup_old_3 <- occup_3$old %>% 
                cross_c2c(., c("wei_freq_c2c", "wei_knn_c2c"), c(1/2,1/2)) %>% 
                prune_c2c(.,column = "wei_cross_c2c", method = "highest1") 
lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old_3, weights = multiplier)
summary(lms)

## we have to adjust size of stds as we artificially enlarge degrees of freedom
occup_old_3 <- occup_3$old %>% 
                prune_c2c(method = "nonzero") # many different prune methods like highest
lms_replicated <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old_3, weights = multiplier * wei_freq_c2c)
## summary_c2c
summary_c2c(lms_replicated, df_old = nrow(occup_old))
# or
# Adjusted R2 is meaningless here
lms_replicated$df.residual <- nrow(occup_old) - length(lms_replicated$assign)
suppressWarnings(summary(lms_replicated))

