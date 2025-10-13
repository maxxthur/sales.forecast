#' Title
#'
#' @param business.model
#'
#' @return
#' @export
#'
#' @examples
setMethod("predict", "business.model", function(object) {
  # prepare data for each model
   extended_data <- vector(mode = "list", length = object@data@h)
   for(i in 1:length(extended_data)) {
     extended_data[[i]] <- prepare_data_fc(response = object@data@response,
                                                                             model = object@extended[[i]]$model,
                                                                             data = object@data,
                                                                             step = i)
     }


   global_data <- prepare_data_fc(response = object@data@response,
                                  model = object@global$model,
                                  data = object@data,
                                  step = object@data@h)

   # estimate models
  extended_models <- vector(mode = "list", length = length(extended_data))
  for(i in 1:length(extended_models)) extended_models[[i]] <- fc(data = extended_data[[i]], response = object@data@response)
  extended_forecast <- do.call("rbind", lapply(extended_models, utils::tail, 1))

  global_forecast <- fc(data = global_data, response = object@data@response)

  return(business.forecast(global = global_forecast,
                    extended = extended_forecast,
                    data = object@data))
})




#' Title
#'
#' @param response
#' @param model
#' @param data
#'
#' @return
#'
#'
#' @examples
prepare_data_fc <- function(response = models@data@response, model = models@extended[[1]]$model, data = object@data, step = 1) {
  # make data for model fitting
  variables <- strsplit(model, ", ")[[1]]
  response_data <- na.omit(data@data[, c("date", response)])
  pred_data <- lapply(strsplit(variables, "_lag_"), function(v) {
    dat <- data@lagged_predictors[v[1]][[1]][, c("date", paste0("lag_", v[2]))]
    names(dat)[2] <- paste(v, collapse = "_lag_")
    dat
  })
  pred_data

  final_data <- response_data
  for(i in 1:length(pred_data)) final_data <- merge(final_data, pred_data[[i]], by = "date", all.x = T)

  # make data set of new predictors for each step
  first <- max(response_data$date) + months(1)
  last <- max(response_data$date) + months(step)

  new_data <- pred_data[[1]]
  if(length(pred_data) > 1) {
    for(i in 2:length(pred_data)) new_data <- merge(new_data, pred_data[[i]], all.x = T, by = "date")
  }
  new_data <- with(new_data, new_data[date >= first & date <= last, ])

  # check for NA, for now just throw warning, later throw error and ensure that in principle no NA can be
  # in data because predictors are available for this step!
  names_na <- names(new_data[, c(1, which(sapply(new_data, function(d) any(is.na(d)))))])

  if(length(names_na) > 1) {
    warning(paste0("NA in predictor(s) ", paste(names_na[-1], collapse = ", "), ". Variable removed."))

   final_data <- final_data[, !names_na[-1] == names(final_data)]

  }
  return(list(estimation = final_data, prediction = new_data))
}

#' Title
#'
#' @param data
#' @param response
#'
#' @return
#'
#' @examples
fc <- function(data = extended_data[[1]], response = object@data@response) {
  not_xreg <- which(names(data$estimation) %in% c("date", response))
  m <- smooth::es(dplyr::pull(data$estimation, response), xreg = data$estimation %>% dplyr::select(-all_of(not_xreg)), regressors = "use")
  fc <- forecast::forecast(object = m, h = nrow(data$prediction), newdata = data$prediction)
  output <- data.frame(date = data$prediction$date, forecast = fc$mean)
  return(output)
}


# Create business.forecast object
#' @export business.forecast
#' @exportClass business.forecast
business.forecast <- setClass("business.forecast",
                           slots = c(global = "data.frame",
                                     extended = "data.frame",
                                     data = "business.data")
)

