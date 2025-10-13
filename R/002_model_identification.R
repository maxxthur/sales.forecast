
#' Title
#'
#' @param data
#' @param predictors
#' @param specific variables that are availaible only for specific prediction steps
#' @param spare_cores
#'
#' @importFrom foreach %dopar%
#' @return
#' @export
#'
#' @examples
identify_model <- function(data, predictors, specific = paste0("auftragseingang_", 1:6), spare_cores = 1) {

  baseline_model <- data@baseline_model
  # check data
  if(class(data) != "business.data") stop("Provided data is not an object of class 'business.data'.")
  # check predictors
  if(any(names(predictors) == "") | is.null(names(predictors))) stop("Every element of 'predictors' must be named appropriately.")
  # identify which lags are to be used for the global model and which for extension

  if(is.null(specific)) {
    global_predictors <- lapply(predictors, function(p) {
      p[p >= data@h | p == 0]
    }
    )
  } else {
  global_predictors <- lapply(predictors[-which(names(predictors) %in% specific)], function(p) {
      p[p >= data@h | p == 0]
    }
    )
  }
  #global_predictors <- append(predictors[which(names(predictors) %in% specific)], global_predictors)
  if(is.null(specific)) {
    extend_predictors <- lapply(predictors, function(p) {
      p[p < data@h & p != 0]
    }
    )
  } else {

  extend_predictors <- lapply(predictors[-which(names(predictors) %in% specific)], function(p) {
    p[p < data@h & p != 0]
  }
  )
  }
  # join the lags
  lags_renamed <- mapply(FUN = function(name, df) {
    names(df)[!names(df) == "date"] <- paste(name, names(df)[!names(df) == "date"], sep = "_")
    df
  }, name = names(data@lagged_predictors), df = data@lagged_predictors, SIMPLIFY = F)

  # delete those variables where there is a special variable for each step
  for(i in length(lags_renamed):1) if(names(lags_renamed[i]) %in% specific) lags_renamed[i] <- NULL

  joined_lags <- data@data[, c("date", data@response)]
  for(i in 1:length(lags_renamed)) {
    joined_lags <- merge(joined_lags, lags_renamed[[i]], by = "date")
  }

  # shorten such that the data starts with the min date of the response and such
  # the max date corresponds to the max date of the response
  min_response <- min(na.omit(joined_lags[, c("date", data@response)])$date)
  max_response <- max(na.omit(joined_lags[, c("date", data@response)])$date)

  # identify for each forecast step which lags are actually available
  # in a forecasting scenario
  exclude_var <- vector(mode = "list", length = data@h)
  for(i in 1:data@h) {
    ex <- names(joined_lags[, which(is.na(joined_lags[joined_lags$date == max_response + months(i), ]))])
    exclude_var[[i]] <- if(is.null(ex)) character() else ex
  }

  training_data <- with(joined_lags, joined_lags[date >= min_response & date <= max_response, ])
  if(!is.null(specific)) training_data <- merge(training_data, data@data[, c("date", specific)], by = "date", all.x = T) # maybe NAs have to be deleted here...

  # set up all models for identification of the global model
  global_names <- c()
  for(i in 1:length(global_predictors)) {
    global_names <- c(global_names, paste(names(global_predictors[i]), "lag", global_predictors[[i]], sep = "_"))
  }

  # add names of model extension predictors
  extension_names <- c()
  for(i in 1:length(global_predictors)) {
    extension_names <- c(extension_names, paste(names(extend_predictors[i]), "lag", extend_predictors[[i]], sep = "_"))
  }

  extension_names <- c(extension_names, specific)

  # kick all entries that are not relevant for the global model
  global_names <- global_names[grepl("\\d", global_names)]

  extended_df <- training_data[, which(names(training_data) %in% c(global_names, extension_names, specific) | names(training_data) %in% c("date", data@response))]

  # make train and test data
  traintest <- make_train_test(df = extended_df, h.test = data@h.test, h = data@h)

  # determine best global model
  message("Determining Global Model")
  best_global <- reduce_mspace(traintest = traintest, xreg_candidates = global_names, response = data@response, exclude = exclude_var, spare_cores = spare_cores, baseline_m = baseline_model)

  # continue here with the procedure to improve individual steps with remaining predictors
  message("Extending Individual Forecast Steps")
  best_extended <- enhance_step(traintest = traintest, response = data@response, global = best_global, xreg_candidates = extension_names, h = data@h, exclude = exclude_var, spare_cores = spare_cores)

  # get actual, prediction and reference for the identified models
  global_fc <- make_test_fc(traintest = traintest, response = response, xreg = strsplit(best_global$model, ", ")[[1]])
  global_fc <- lapply(global_fc, function(gfc) {
    ref_dat <- data@data[, c("date", data@reference)]
    out <- merge(gfc[, c("date", "fc", "actual")], ref_dat, by = "date", all.x = T)
    out
  })

  best_global$results <- global_fc

  extended_fc <- lapply(best_extended, function(be) {
    make_test_fc(traintest = traintest, response = response, xreg = strsplit(be$model, ", ")[[1]])
  })

  extended_ordered <- vector(mode = "list", length = length(extended_fc) + 1)
  for(i in 1:length(extended_fc)) {
    for(j in 1:length(extended_ordered))
    if(i == 1) {
      extended_ordered[[j]] <- extended_fc[[i]][[j]][i, ]
    } else {
      extended_ordered[[j]] <- rbind(extended_ordered[[j]], extended_fc[[i]][[j]][i, ])
    }
  }

  extended_final <- lapply(extended_ordered, function(gfc) {
    ref_dat <- data@data[, c("date", data@reference)]
    out <- merge(gfc[, c("date", "fc", "actual")], ref_dat, by = "date", all.x = T)
    out
  })

  best_extended$results <- extended_final

  save(best_global, best_extended, file = "intermediate.RData")

  # output best models for each step
  output <- business.model(global = best_global,
                 extended = best_extended,
                 data = data)

  return(output)
}


#' Title
#'
#' @param traintest
#' @param response
#' @param global
#' @param xreg_candidates
#' @param h
#' @param exclude
#' @param spare_cores
#'
#' @importFrom foreach %dopar%
#' @return
#'
#'
#' @examples
enhance_step <- function(traintest, response, global, xreg_candidates, h, exclude = exclude_var, spare_cores) {
  # make a list of candidates for each step
  candidates_list <- lapply(1:h, function(i) {
    xreg_candidates[as.integer(substr(xreg_candidates, nchar(xreg_candidates), nchar(xreg_candidates))) >= i & !xreg_candidates %in% exclude[[i]]]
  })

  gmod <- unlist(strsplit(global$model, ", "))

  # make forecasts for each step with the global model to find benchmarks
  results_global <- make_test_fc(traintest = traintest, response = response, xreg = gmod) %>%
    purrr::map(~ dplyr::mutate(., step = dplyr::row_number())) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(step) %>%
    dplyr::reframe(rmse = sqrt(mean(error^2))) %>%
    dplyr::group_split(step)

  best <- vector(mode = "list", length = length(results_global))
  for(i in 1:length(best)) best[[i]] <- list(RMSE = results_global[[i]]$rmse, model = paste(gmod, collapse = ", "))

  # set up cluster for parellel processing
  cores <- parallel::detectCores() - spare_cores
  cl <- parallel::makeCluster(cores)
  parallel::clusterExport(cl = cl, c("make_test_fc", "%>%", "eval_fc"))
  doSNOW::registerDoSNOW(cl)

  # sequentially enhance the global model for each forecast step separately with the available predictors
  for(s in 1:h) {
    cat("Testing enhanced models for step", s, "\n")
    xreg_step <- xreg_candidates[]
    n_pred <- 1
    improved <- data.frame(RMSE = NA, model = NA)
    if(length(candidates_list[[s]]) > 0) {
    while(n_pred == 1 | length(improved$model) > 0) {
      cat("Testing models with", n_pred, "additional predictors.\n")
      candidate_models <- make_candidates_list(n = n_pred, existing = improved$model, new = candidates_list[[s]], baseline = gmod)
      cat("Number of models:", length(candidate_models), "in step.\n")

      results <- run_models(candidate_models = candidate_models, traintest = traintest, response = response, s = s)

      # get models that were better than the previous best
      if(is.null(results)) {
        improved <- data.frame(RMSE = NA, model = NA)
        improved <- improved[-1, ]
      } else {
      improved <- with(results, results[RMSE < best[[s]]$RMSE, ])
      }
      # update best
      if(nrow(improved) > 0) {
        best[[s]]$RMSE <- with(improved, improved[which.min(RMSE), ]$RMSE)
        best[[s]]$model <- with(improved, improved[which.min(RMSE), ]$model)
      }
      n_pred <- n_pred + 1
      }
    }
  }
  parallel::stopCluster(cl)
  return(best)
}

#' run test models
#'
#' @param candidate_models
#' @param traintest
#' @param response
#' @param s
#' @importFrom foreach %dopar%
#' @return
#' @export
#'
#' @examples
run_models <- function(candidate_models, traintest, response, s = NULL) {
  iterations <- length(candidate_models)
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  results <- foreach::foreach(i = candidate_models, .combine = "rbind", .options.snow = opts) %dopar% {
    forecasts <- try(make_test_fc(traintest = traintest, response = response, xreg = unlist(i)))
    if(class(forecasts) == "try-error") {
      cat("Model", i, "failed.\n")
    } else {
      if(is.null(s)) {
        rmse <- eval_fc(forecasts)
        model <- unique(forecasts[[1]]$model)
        data.frame(RMSE = rmse, model = model)
      } else {
      forecasts <- lapply(forecasts, function(f) f[s, ])
      rmse <- dplyr::bind_rows(forecasts) %>%
        dplyr::summarise(rmse = sqrt(mean(error^2))) %>%
        dplyr::pull(rmse)
      model <- unique(forecasts[[1]]$model)
      data.frame(RMSE = rmse, model = model)
      }
    }
  }
  close(pb)
  results
}

#' create models to test
#'
#' @param n
#' @param existing
#' @param new
#' @param baseline
#'
#' @return
#'
#' @examples
make_candidates_list <- function(n, existing, new, baseline = NULL) {
  if(n == 1) {
    if(is.null(baseline)) {
      candidate_models <- new
    } else {
      candidate_models <- vector(mode = "list", length = length(new))
      for(i in 1:length(new)) candidate_models[[i]] <- c(baseline, new[i])
    }
  } else {
    candidate_models <- vector(mode = "list", length = length(existing) * length(new))
    candidate_models <- lapply(candidate_models, function(cm) rep(NA, n))
    k = 1
    for(i in strsplit(existing, ", ")) {
      for(j in new) {
        if(j %in% i) {

        } else {
          candidate_models[[k]] <- c(i, j)
          k <- k + 1
        }
      }
    }

    # kick empty list elements
    for(i in length(candidate_models):1) if(all(is.na(candidate_models[[i]]))) candidate_models[[i]] <- NULL

    candidate_models <- candidate_models[!duplicated(lapply(candidate_models, sort))]
  }
  candidate_models
}



#' Title
#'
#' @param traintest data for training and testing models.
#' @param xreg_candidates names of variables that should be used as predictors
#' @param exclude
#' @param response response variable
#' @param spare_cores
#' @param baseline_m
#'
#' @importFrom foreach %dopar%
#' @return
#'
#'
#' @examples
reduce_mspace <- function(traintest, xreg_candidates, response, exclude = exclude_var, spare_cores, baseline_m = baseline_model) {
  # maybe make more modules from this function as it is quite long
  # set model without external regressors as best to initialize procedure
  best <- list(RMSE = NULL, model = NULL)
  baseline <- make_test_fc(traintest = traintest, xreg = baseline_m, response = response)
  best$RMSE <- eval_fc(baseline)
  best$model <- unique(baseline[[1]]$model)

  # set up cluster for parallel processing
  cores <- parallel::detectCores() - spare_cores
  cl <- parallel::makeCluster(cores)
  parallel::clusterExport(cl = cl, c("make_test_fc", "%>%", "eval_fc"))
  doSNOW::registerDoSNOW(cl)

  n_pred <- 1
  improved <- data.frame(RMSE = NA, model = NA)
  global <- NULL
  xreg_candidates <- xreg_candidates[!xreg_candidates %in% Reduce("intersect", exclude)]
  while(n_pred == 1 | length(improved$model) > 0) {
    cat("Testing models with", n_pred, "predictors.\n")
    candidate_models <- make_candidates_list(n = n_pred, existing = improved$model, new = xreg_candidates, baseline = global)
    cat("Number of models:", length(candidate_models), "in step.\n")

    results <- run_models(candidate_models = candidate_models, traintest = traintest, response = response, s = NULL)

    # get models that were better than the previous best
    improved <- with(results, results[RMSE < best$RMSE, ])
    # update best
    if(nrow(improved) > 0) {
    best$RMSE <- with(improved, improved[which.min(RMSE), ]$RMSE)
    best$model <- with(improved, improved[which.min(RMSE), ]$model)
    }
    n_pred <- n_pred + 1
  }

  parallel::stopCluster(cl)
  return(best)
}

#' create training and test data for model selection
#'
#' @param df data.frame holding all variables
#' @param h.test number of test periods
#' @param h forecast horizon
#'
#' @return nested list with training data for each test step
#'
#' @examples
make_train_test <- function(df, h.test, h) {
  train <- list()
  test <- list()
  traintest <- list(train = train, test = test)
  for(i in 0:h) {
    traintest$train[[i + 1]] <- df %>%
      head(-h.test + i) %>%
      tidyr::fill(everything(), .direction = "down")

    traintest$test[[i + 1]] <- dplyr::anti_join(df, traintest$train[[i + 1]], by = "date") %>%
      head(h) %>%
      tidyr::fill(everything(), .direction = "down")
  }
  traintest
}

#' make test forecasts for further evaluation
#'
#' @param traintest a named list with nested lists **train** and **test**
#' @param response variable to be predicted
#' @param xreg external regressors used
#'
#' @export
#' @return list of data.frames holding forecasting results
#'
#' @examples
make_test_fc <- function(traintest, response, xreg, exclude_var) {
  results <- list()
  for(i in 1:length(traintest$train)) {
    m <- smooth::es(dplyr::pull(traintest$train[[i]], response), xreg = traintest$train[[i]] %>% dplyr::select(all_of(xreg)), regressors = "use")
    fc <- forecast::forecast(object = m, h = nrow(traintest$test[[i]]), newdata = traintest$test[[i]])
    actual <- dplyr::pull(traintest$test[[i]], response)
    results[[i]] <- data.frame(date = traintest$test[[i]]$date, fc = fc$mean, actual = actual, error = fc$mean - actual, win = i, model = paste(xreg, collapse = ", "))
  }
  results
}

#' Evaluation of test forecasts
#'
#' @param fc a list of forecast outputs
#'
#' @return
#' @export
#'
#' @examples
eval_fc <- function(fc) {
  median(sapply(fc, function(f) {
    RMSE <- sqrt(mean(f$error^2))
  }))
}

# Create business.model object
#' @export business.model
#' @exportClass business.model
business.model <- setClass("business.model",
                          slots = c(global = "list",
                                    extended = "list",
                                    data = "business.data")
                          )
