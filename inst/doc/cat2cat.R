## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = T)
knitr::opts_chunk$set(size = "tiny")
knitr::opts_chunk$set(message = F)
knitr::opts_chunk$set(warning = F)

## -----------------------------------------------------------------------------
library(cat2cat)
library(dplyr)

data(verticals)
agg_old <- verticals[verticals$v_date == "2020-04-01", ]
agg_new <- verticals[verticals$v_date == "2020-05-01", ]

## cat2cat_agg - could map in both directions at once although 
## usually we want to have old or new representation

agg <- cat2cat_agg(data = list(old = agg_old, 
                              new = agg_new, 
                              cat_var = "vertical", 
                              time_var = "v_date",
                              freq_var = "counts"), 
                  Automotive %<% c(Automotive1, Automotive2),
                  c(Kids1, Kids2) %>% c(Kids),
                  Home %>% c(Home, Supermarket))
            
## possible processing
  
agg$old %>% 
group_by(vertical) %>% 
summarise(sales = sum(sales*prop_c2c), counts = sum(counts*prop_c2c), v_date = first(v_date))

agg$new %>% 
group_by(vertical) %>%
summarise(sales = sum(sales*prop_c2c), counts = sum(counts*prop_c2c), v_date = first(v_date))

## -----------------------------------------------------------------------------
## the ean variable is a unique identifier
data(verticals2)

vert_old <- verticals2[verticals2$v_date == "2020-04-01", ]
vert_new <- verticals2[verticals2$v_date == "2020-05-01", ]

## get transitions table
trans_v <- vert_old %>% 
inner_join(vert_new, by = "ean") %>%
select(vertical.x, vertical.y) %>% distinct()

## -----------------------------------------------------------------------------
# 
## cat2cat
## it is important to set id_var as then we merging categories 1 to 1 
## for this identifier which exists in both periods.
verts <- cat2cat(
  data = list(old = vert_old, new = vert_new, id_var = "ean", cat_var = "vertical", time_var = "v_date"),
  mappings = list(trans = trans_v, direction = "backward")
)

## -----------------------------------------------------------------------------
data(occup)
data(trans)

occup_old <- occup[occup$year == 2008,]
occup_new <- occup[occup$year == 2010,]

## -----------------------------------------------------------------------------
## cat2cat
occup_simple <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

## with informative features it might be usefull to run ml algorithm
## currently only knn, lda or rf (randomForest),  a few methods could be specified at once 
## where probability will be assessed as fraction of closest points.
occup_2 <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = "knn", features = c("age", "sex", "edu", "exp", "parttime", "salary"), 
            args = list(k = 10))
)

# summary_plot
plot_c2c(occup_2$old, type = c("both"))

# mix of methods
occup_2_mix <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = c("knn", "rf", "lda"), features = c("age", "sex", "edu", "exp", "parttime", "salary"), 
            args = list(k = 10, ntree = 50))
)
# correlation between ml models and simple fequencies
occup_2_mix$old %>% select(wei_knn_c2c, wei_rf_c2c, wei_lda_c2c, wei_freq_c2c) %>% cor()
# cross all methods and subset one highest probability category for each subject
occup_old_mix_highest1 <- occup_2_mix$old %>% 
                cross_c2c(.) %>% 
                prune_c2c(.,column = "wei_cross_c2c", method = "highest1") 

## -----------------------------------------------------------------------------
## orginal dataset 
lms2 <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old, weights = multiplier)
summary(lms2)

## using one highest cross weights
## cross_c2c to cross differen methods weights
## prune_c2c - highest1 leave only one the highest probability obs for each subject
occup_old_2 <- occup_2$old %>% 
                cross_c2c(., c("wei_freq_c2c", "wei_knn_c2c"), c(1/2,1/2)) %>% 
                prune_c2c(.,column = "wei_cross_c2c", method = "highest1") 
lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old_2, weights = multiplier)
summary(lms)

## we have to adjust size of stds as we artificialy enlarge degrees of freedom
occup_old_3 <- occup_2$old %>% 
                prune_c2c(method = "nonzero") #many prune methods like highest
lms_replicated <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old_3, weights = multiplier * wei_freq_c2c)
# Adjusted R2 is meaningless here
lms_replicated$df.residual <- nrow(occup_old) - length(lms_replicated$assign)
suppressWarnings(summary(lms_replicated))

## -----------------------------------------------------------------------------
occup_old_4 <- occup_2$old %>% 
                prune_c2c(method = "nonzero") #many prune methods like highest
formula_oo <- formula(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp + factor(year))
oo <- rbind(occup_old_4, occup_2$new) %>% 
  group_by(g_new_c2c) %>% 
  do(
    lm = tryCatch(
      summary(lm(formula_oo, ., weights = multiplier * wei_freq_c2c)), 
      error = function(e) NULL
    )
  ) %>%
  filter(!is.null(lm))

head(oo)

oo$lm[[1]]

