#' Title
#'
#' @param business.data
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
setMethod("plot", "business.data", function(x) {
  for(i in 1:length(x@correlations)) {
    # add plots of response too
    plot(0:(ncol(x@correlations[[i]])-1),
         t(x@correlations[[i]]),
         type = "h",
         ylim = c(-1, 1),
         xlab = "lag",
         ylab = "correlation",
         main = names(x@correlations[i]))
    abline(h = 0.3, col = "blue", lty = "dashed")
    abline(h = -0.3, col = "blue", lty = "dashed")
    abline(h = 0, col = "red", lty = "dashed")
    par(ask = T)
  }
})


#' Title
#'
#' @param business.data
#'
#' @return
#' @export
#'
#' @examples
setMethod("show", "business.data", function(object) {
  cat("S4 Object of class business.data for predictions of the response variable:",
      object@response, "\n", "\n")
  cat("Predictors considered:\n\n")
  cat(paste(names(object@data)[!names(object@data) %in% c("date", object@response, object@reference)], collapse = ",\n"), "\n\n")
  cat("Lags: ", ncol(object@lagged_predictors[[1]])-1, "\n")
  cat("Reference:", object@reference, "\n")
  cat("Holdout:", object@h.test)
})

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
plot_fc <- function(object) {
  if(class(object) != "business.forecast") stop("Object is not of class business.forecast")
  historical <- utils::tail(na.omit(object@data@data[, c("date", object@data@response)]), 24)
  reference <- object@data@data %>% dplyr::select(date, reference = object@data@reference)
  historical$color <- "steelblue"
  forecasts <- data.frame(date = object@global$date, umsatz = NA, global = object@global$forecast, extended = object@extended$forecast, color = "darkred")
  plotdat <- dplyr::bind_rows(historical, forecasts)
  plotdat[(nrow(plotdat)-6), 4:5] <- plotdat[(nrow(plotdat)-6), 2]

  with(plotdat, plot(date, umsatz, col = color, type = "l", main = "Forecasts", lwd = 2, xlab = ""))
  with(plotdat, lines(x = date, y = global, col = "darkred", lty = "dashed", lwd = 2))
  with(plotdat, lines(x = date, y = extended, col = "darkgreen", lty = "dashed", lwd = 2))
  with(reference, lines(x = date,  y = reference, col = "orange"))
  legend(x = "bottom",
         legend = c("Historical", "Global", "Extended", "Reference"),
         lty = c(1, 1),
         col = c("steelblue", "darkred", "darkgreen", "orange"),
         lwd = 2,
         bty = "n",
         cex = 0.75,
         xpd = T,
         inset = c(0, -0.125),
         horiz = T)
}

plot_train <- function(object) {
  if(!class(object) == "business.model") stop("Input is not an object of class business.model.")
  plotdat <- vector(mode = "list", length(object@global$results))
    for(i in 1:length(plotdat)) plotdat[[i]] <- merge(object@global$results[[i]], object@extended$results[[i]][, c("date", "fc")], by = "date", all.x = T)
  plotdat <- lapply(plotdat, function(p) {
    names(p)[grepl("fc", names(p))] <- c("global", "extended")
    p
  })
  for(i in 1:length(plotdat)) plotdat[[i]]$window <- i
  plotdat <- do.call("rbind", plotdat)

  plotdat %>%
    tidyr::pivot_longer(cols = c(actual, global, extended, object@data@reference)) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = value, color = name)) +
    ggplot2::scale_color_manual(values = c("darkred", "orange", "steelblue", "darkgreen")) +
    ggplot2::geom_line(size = 1.15) +
    ggplot2::facet_wrap(~window, scales = "free") +
    ggplot2::labs(x = "", title = "Forecast Evaluation Model Identification") +
    ggthemes::theme_base() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}


