#' Calculate and Plot Survival Curve
#'
#' This function takes two numerical vectors: status (censoring status) and time (time to event or censor),
#' calculates the survival probability using the Kaplan-Meier estimator, and plots the survival curve.
#'
#' @param status Numeric vector indicating the event status (1 if event occurred, 0 if censored).
#' @param time Numeric vector indicating the time to event or time to censoring.
#' @import survival
#' @examples
#' # Assuming that 'survival.csv' has been loaded with columns 'time' and 'status'
#' data <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' survCurv(data$status, data$time)
#' @export
survCurv <- function(status, time) {
  if (length(status) != length(time)) {
    stop("The length of 'status' and 'time' must be the same.")
  }

  # Create a survival object
  surv_obj <- Surv(time, status)

  # Fit survival data using Kaplan-Meier estimate
  surv_fit <- survfit(surv_obj ~ 1)

  # Plot the survival curve
  plot(surv_fit, main = "Survival Curve", xlab = "Time", ylab = "Survival Probability",
       col = "black", lwd = 2, conf.int = TRUE)

  # Optionally, you can return the survival fit object for further analysis
  invisible(surv_fit)
}


# Load the dataset
data <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")

# Call the survCurv function with appropriate columns from the dataset
survCurv(data$status, data$time)


