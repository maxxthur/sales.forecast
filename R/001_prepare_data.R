# think about implementing something that measures cross correlation more timely
# in addition to the complete cross-correlation to see where maybe relationships
# break or emerge

#' Title
#'
#' @param data a **named** list of data.frames with all data that should be included in the analysis
#' @param h.test how many periods should be considered for testing
#' @param response string corresponding to the name of the data.frame in data which contains the data for the response
#' @param reference optional. Reference against which the model should be evaluated. Default is 0.
#' @param k number of lags to consider for cross correlation analysis.
#' @param no_lags optional. Column names of variables where lags should be ignored.
#' @param h
#' @param baseline_model
#'
#' @return an object of class business.data
#' @export
#'
#' @examples
prepare_data <- function(data = available_data, h = 6, h.test = 12, response = "Umsatz",
                         reference = NULL, k = 24, no_lags = c("holidays", "workdays"), baseline_model = "workdays") {

  if(!response %in% names(data)) stop("'reponse' not found in names of data list.")

  # identify date variables and convert to date, rename to "date"
  data <- lapply(data, function(d) {
    d[] <- lapply(d[], function(y) {
      if(lubridate::is.Date(y)) y else if(is_ymd(y)) lubridate::ymd(y) else y
    })
    d
  })

  data <- lapply(data, function(d) {
    names(d)[which(col_class(d) == "Date")] <- "date"
    d
  })

  # check if there is a date available for all provided data, throw error if not
  if(!all(
    sapply(data, function(d)
    "Date" %in% sapply(d, class)
    )
  )) stop("Not all provided data.frames have a column of type 'Date' or a column that can be coerced to 'Date'.")

  # get min date as the minimum date in the df holding the response - k months to not lose info
  min_date <- min(c(data[[response]][which(col_class(data[[response]]) == "Date")])[[1]]) - months(k)

  # get max date as the overall max_date to not loose any information
  max_date <- as.Date(max(sapply(data, function(d) {max(c(d[which(col_class(d) == "Date")])[[1]])})), origin = "1970-01-01")

  # make initial data.frame
  df <- data.frame(date = seq(min_date, max_date, "months"))

  # merge all other sequentially
  for(i in 1:length(data)) df <- merge(df, data[[i]], by = "date", all.x = T)

  # clean names
  df <- janitor::clean_names(df)

  # identify completely empty columns
  empty_cols <- sapply(df, function(d) all(is.na(d)))
  warning(paste("Removed empty columns:\n", paste(names(empty_cols[empty_cols]), collapse = ",\n")))

  # remove empty columns
  df <- df[, -which(empty_cols)]

  # amend response and reference
  response <- gsub(pattern = " ", replacement = "_", x = tolower(response))
  reference <- gsub(pattern = " ", replacement = "_", x = tolower(reference))
  no_lags <- gsub(pattern = " ", replacement = "_", x = tolower(no_lags))

  # make lags of each variable that is not response, reference or date
  lags <- make_lags(df, exclude = c("date", response, reference, no_lags), k = k)
  no_lag <- vector(mode = "list", length = length(no_lags))
  for(i in 1:length(no_lags)) {no_lag[[i]] <- df[, c("date", no_lags[i])]; names(no_lag[[i]]) <- c("date", "lag_0")}
  names(no_lag) <- no_lags
  lags <- append(lags, no_lag)

  # calculate correlation coefficient for each variable
  corr <- lapply(lags, function(l) {
    cor(df$umsatz, l[,-which(names(l) == "date")], use = "pairwise.complete.obs")
  })

  # Output
  output <- business.data(response = response,
       reference = reference,
       data = df,
       lagged_predictors = lags,
       correlations = corr,
       h.test = h.test,
       h = h,
       baseline_model = baseline_model)

  output
}

# Create business.data object
#' @export business.data
#' @exportClass business.data
business.data <- setClass("business.data",
                          slots = c(response = "character",
                                    reference = "character",
                                    data = "data.frame",
                                    lagged_predictors = "list",
                                    correlations = "list",
                                    h.test = "numeric",
                                    h = "numeric",
                                    baseline_model = "character"))




