devtools::load_all()
library(tidyverse)
load("inst/debug_data.RData")

business_data <- prepare_data(data = data, h = 6, h.test = 12, response = "umsatz",
                              reference = "budget", k = 24, no_lags = c("workdays", "holiday"), baseline_model = c("workdays_lag_0"))

# get for each predictor the 10 with the highest absolute correlations
promising <- lapply(business_data@correlations, function(c) {
  indices_greater <- which(abs(c) > 0.3)
  indices <- indices_greater[order(abs(c[indices_greater]), decreasing = T)]
  # if(length(sorted) >= 10) {first_n <- 1:10} else {first_n <- 1:length(sorted)}
  # sorted[first_n]
  sort(head(indices, 10) - 1)
})

predictors <- promising[str_detect(names(promising), "auftragseingang", negate = T)]
predictors$holiday <- 0
predictors$workdays <- 0
predictors[1:(length(predictors)-2)] <- lapply(predictors[1:(length(predictors)-2)], function(p) {
  p[p != 0]
})

predictors$auftragseingang_1 <- 0
predictors$auftragseingang_2 <- 0
predictors$auftragseingang_3 <- 0
predictors$auftragseingang_4 <- 0
predictors$auftragseingang_5 <- 0
predictors$auftragseingang_6 <- 0

specific = paste0("auftragseingang_", 1:6)
response <- business_data@response

modell <- identify_model(data = business_data, predictors = predictors, specific = specific, spare_cores = 1)

save(modell, file = "just_for_dev/model.RData")

load("just_for_dev/model.RData")

# Forecast für nächste 6 Monate berechnen
forecast <- predict(modell)
sales.forecast::plot_fc(forecast)
sales.forecast:::plot_train(modell)
