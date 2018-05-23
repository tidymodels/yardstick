#### Following the example from
#### https://topepo.github.io/rsample/articles/Applications/Survival_Analysis.html

library(dplyr)
library(tidyr)
library(tidyposterior)
library(rsample)
theme_set(theme_bw())

###Load data
library(survival)
str(lung)

#library(yardstick)
devtools::document()

#Need to be fixed 1 event / 0 censored
lung <- lung %>%
  mutate(status = (status == 2))


set.seed(9666)
mc_samp <- mc_cv(lung, strata = "status", times = 100)

library(purrr)
cens_rate <- function(x) mean(analysis(x)$status == 1)
summary(map_dbl(mc_samp$splits, cens_rate))


################ Create formulas
three_fact <- as.formula(Surv(time, status) ~ ph.ecog + age + sex)
rm_ph.ecog <- as.formula(Surv(time, status) ~           age + sex)
rm_age     <- as.formula(Surv(time, status) ~ ph.ecog + sex)
rm_sex     <- as.formula(Surv(time, status) ~ ph.ecog + age              )

############### Create models
mod_fit <- function(x, form, ...)
  coxph(form, data = analysis(x), ...)

mc_samp$mod_full    <- map(mc_samp$splits, mod_fit, form = three_fact)
mc_samp$mod_ph.ecog <- map(mc_samp$splits, mod_fit, form = rm_ph.ecog)
mc_samp$mod_age     <- map(mc_samp$splits, mod_fit, form = rm_age)
mc_samp$mod_sex     <- map(mc_samp$splits, mod_fit, form = rm_sex)

############### Get Brier

mc_samp$brier_full <- pmap(list(mc_samp$splits, mc_samp$mod_full),
                             function(data, model){
                               get_tdbrier(data = data,
                                           mod = model
                               )
                             })
mc_samp$brier_ph.ecog <- pmap(list(mc_samp$splits, mc_samp$mod_ph.ecog),
                              function(data, model){
                                get_tdbrier(data = data,
                                            mod = model)
                              })
mc_samp$brier_age <- pmap(list(mc_samp$splits, mc_samp$mod_age),
                              function(data, model){
                                get_tdbrier(data = data,
                                            mod = model
                                )
                              })
mc_samp$brier_sex <- pmap(list(mc_samp$splits, mc_samp$mod_sex),
                              function(data, model){
                                get_tdbrier(data = data,
                                            mod = model
                                )
                              })

###integrate Brier
mc_samp$ibrier_full <- map_dbl(mc_samp$brier_full, integrate.tdbrier)
mc_samp$ibrier_ph.ecog <- map_dbl(mc_samp$brier_ph.ecog, integrate.tdbrier)
mc_samp$ibrier_age <- map_dbl(mc_samp$brier_age, integrate.tdbrier)
mc_samp$ibrier_sex <- map_dbl(mc_samp$brier_sex, integrate.tdbrier)


int_brier <- mc_samp %>%
  select(-matches("^mod"), -starts_with("brier"),  -starts_with("cindex"), -starts_with("roc"), -starts_with("iroc"))

int_brier %>%
  select(-splits) %>%
  gather() %>%
  ggplot(aes(x = statistic, col = model)) +
  geom_line(stat = "density") +
  theme_bw() +
  theme(legend.position = "top")

int_brier <- perf_mod(int_brier, seed = 6507, iter = 5000)

ggplot(tidy(int_brier)) +
  theme_bw()

ibrier_tab <- summary(tidy(int_brier))

my_round <- function(x){
  if(is.character(x)){
    x
  }else{
    round(x,3)
  }
}

as.data.frame(ibrier_tab) %>% mutate_all(my_round)

require(stargazer)
stargazer(ibrier_tab, type = "latex", summary = FALSE, digits.extra = 3,
          digits = 3, digit.separator = ".",
          title = "Bayesian analysis of resampling iBrier")


comparisons <- contrast_models(
  int_brier,
  list_1 = rep("ibrier_full", 3),
  list_2 = c("ibrier_ph.ecog", "ibrier_age", "ibrier_sex"),
  seed = 4654
)

ggplot(comparisons, size = 0.05) +
  theme_bw()

summary(comparisons, size = 0.05) %>%
  select(contrast, starts_with("pract"))


############### Get ROC

mc_samp$roc_full <- pmap(list(mc_samp$splits, mc_samp$mod_full),
                           function(data, model){
                             get_tdroc(data = data,
                                         mod = model
                             )
                           })
mc_samp$roc_ph.ecog <- pmap(list(mc_samp$splits, mc_samp$mod_ph.ecog),
                              function(data, model){
                                get_tdroc(data = data,
                                            mod = model)
                              })
mc_samp$roc_age <- pmap(list(mc_samp$splits, mc_samp$mod_age),
                          function(data, model){
                            get_tdroc(data = data,
                                        mod = model
                            )
                          })
mc_samp$roc_sex <- pmap(list(mc_samp$splits, mc_samp$mod_sex),
                          function(data, model){
                            get_tdroc(data = data,
                                        mod = model
                            )
                          })

###integrate roc
mc_samp$iroc_full <- map_dbl(mc_samp$roc_full, integrate.tdroc)
mc_samp$iroc_ph.ecog <- map_dbl(mc_samp$roc_ph.ecog, integrate.tdroc)
mc_samp$iroc_age <- map_dbl(mc_samp$roc_age, integrate.tdroc)
mc_samp$iroc_sex <- map_dbl(mc_samp$roc_sex, integrate.tdroc)


int_roc <- mc_samp %>%
  select(-matches("^mod"), -starts_with("roc"),  -starts_with("cindex"), -starts_with("brier"), -starts_with("ibrier"))

int_roc %>%
  select(-splits) %>%
  gather() %>%
  ggplot(aes(x = statistic, col = model)) +
  geom_line(stat = "density") +
  theme_bw() +
  theme(legend.position = "top")

int_roc <- perf_mod(int_roc, seed = 6507, iter = 5000)

ggplot(tidy(int_roc)) +
  theme_bw()

iroc_tab <- summary(tidy(int_roc))

as.data.frame(iroc_tab) %>% mutate_all(my_round)

require(stargazer)
stargazer(iroc_tab, type = "latex", summary = FALSE, digits.extra = 3,
          digits = 3, digit.separator = ".",
          title = "Bayesian analysis of resampling iroc")


comparisons <- contrast_models(
  int_roc,
  list_1 = rep("iroc_full", 3),
  list_2 = c("iroc_ph.ecog", "iroc_age", "iroc_sex"),
  seed = 4654
)

ggplot(comparisons, size = 0.05) +
  theme_bw()

summary(comparisons, size = 0.05) %>%
  select(contrast, starts_with("pract"))

######Concordance Index

mc_samp$cindex_full <- pmap_dbl(list(mc_samp$splits, mc_samp$mod_full),
                         function(data, model){
                           get_cindex(data = data,
                                     mod = model
                           )
                         })
mc_samp$cindex_ph.ecog <- pmap_dbl(list(mc_samp$splits, mc_samp$mod_ph.ecog),
                            function(data, model){
                              get_cindex(data = data,
                                        mod = model)
                            })
mc_samp$cindex_age <- pmap_dbl(list(mc_samp$splits, mc_samp$mod_age),
                        function(data, model){
                          get_cindex(data = data,
                                    mod = model
                          )
                        })
mc_samp$cindex_sex <- pmap_dbl(list(mc_samp$splits, mc_samp$mod_sex),
                        function(data, model){
                          get_cindex(data = data,
                                    mod = model
                          )
                        })


cindex <- mc_samp %>%
  select(-matches("^mod"), -starts_with("iroc"),  -starts_with("roc"), -starts_with("brier"), -starts_with("ibrier"))

cindex %>%
  select(-splits) %>%
  gather() %>%
  ggplot(aes(x = statistic, col = model)) +
  geom_line(stat = "density") +
  theme_bw() +
  theme(legend.position = "top")

cindex <- perf_mod(cindex, seed = 6507, iter = 5000)

ggplot(tidy(cindex)) +
  theme_bw()

cindex_tab <- summary(tidy(cindex))

as.data.frame(cindex_tab) %>% mutate_all(my_round)

require(stargazer)
stargazer(cindex_tab, type = "latex", summary = FALSE, digits.extra = 3,
          digits = 3, digit.separator = ".",
          title = "Bayesian analysis of resampling cindex")


comparisons <- contrast_models(
  cindex,
  list_1 = rep("cindex_full", 3),
  list_2 = c("cindex_ph.ecog", "cindex_age", "cindex_sex"),
  seed = 4654
)

ggplot(comparisons, size = 0.05) +
  theme_bw()

summary(comparisons, size = 0.05) %>%
  select(contrast, starts_with("pract"))




