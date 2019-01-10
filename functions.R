rescale01 <- function(num){
  rng <- range(num, na.rm = T, finite = T)
  (num - rng[[1]])/(rng[[2]] - rng[[1]])
}

rescale01(c(100, 1000, 10000, 100000))

library(nigr)
library(ggplot2)
names(diamonds)

strct <- function(...){
  dots <- enquos(...)
  dot <- quos(...)
  !!!dots
  !!!dot
}

grouped_mean <- function(data, group_var, summary_var) {
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  data %>%
    group_by(!!group_var) %>%
    summarise(mean = mean(!!summary_var))
}


loadlibs <- function(...){
  dots <- quote(...)
  for (i in length(dots)) {
    !!dots
  }
}


runtwice <- function(f){
  function(...){
    f(...)
    f(...)
  }
}


