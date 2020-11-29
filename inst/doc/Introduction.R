## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = T)
knitr::opts_chunk$set(size = "tiny")
knitr::opts_chunk$set(message = F)
knitr::opts_chunk$set(warning = F)

## -----------------------------------------------------------------------------
options(scipen = 999)
pacman::p_load(cat2cat, dplyr)

## -----------------------------------------------------------------------------
data(occup)

data(trans)

## -----------------------------------------------------------------------------
occup %>% glimpse()

## -----------------------------------------------------------------------------
trans %>% glimpse()

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
cor(occup_3$old$wei_knn_c2c, occup_3$old$wei_freq_c2c, use = "complete.obs")

## -----------------------------------------------------------------------------
occup_3 %>% glimpse()

## -----------------------------------------------------------------------------
get_freqs(x = occup_3$new$g_new_c2c, multiplier = floor(occup_3$new$multiplier  * occup_3$new$wei_freq_c2c)) %>% head()

## -----------------------------------------------------------------------------
get_freqs(x = occup_2$old$g_new_c2c, multiplier  = floor(occup_2$old$multiplier  * occup_2$old$wei_freq_c2c))  %>% head()

