% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat2cat_utils.R
\name{cross_c2c}
\alias{cross_c2c}
\title{Make a combination of weights from different methods}
\usage{
cross_c2c(
  df,
  cols = colnames(df)[grepl("^wei_.*_c2c$", colnames(df))],
  weis = rep(1/length(cols), length(cols)),
  na.rm = TRUE
)
}
\arguments{
\item{df}{`data.frame` like result of the `cat2cat` function
for a specific period.}

\item{cols}{`character` vector default all columns under
the regex "wei_.*_c2c".}

\item{weis}{`numeric` vector weighs for columns in the `cols` argument.
By default a vector of the same length as `cols` argument and with equally
spaced probability (summing to 1).}

\item{na.rm}{`logical(1)` if `NA` values should be omitted, default TRUE.}
}
\value{
`data.frame` with the additional column `wei_cross_c2c`.
}
\description{
adding the additional column which is a mix of weights columns
by each row.
Ensemble of a few methods usually produces more accurate solutions
than a single model would.
}
\examples{
\dontrun{
data("occup_small", package = "cat2cat")
data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_old <- occup_small[occup_small$year == 2008, ]
occup_new <- occup_small[occup_small$year == 2010, ]

# mix of methods - forward direction, try out backward too
occup_mix <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_new,
    cat_var = "code",
    method = c("knn"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 20)
  )
)
# correlation between ml model
occup_mix_old <- occup_mix$old
cor(
  occup_mix_old[occup_mix_old$rep_c2c != 1, c("wei_knn_c2c", "wei_freq_c2c")]
)
# cross all methods and subset one highest probability category for each obs
occup_old_highest1_mix <- prune_c2c(cross_c2c(occup_mix$old),
  column = "wei_cross_c2c", method = "highest1"
)
}

}
