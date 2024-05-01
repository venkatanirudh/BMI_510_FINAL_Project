#' Download RedCap Report
#'
#' This function downloads a RedCap report using the provided API token name, URL, and report ID.
#'
#' @param redcapTokenName The name of the API token stored in the user's .REnviron file.
#' @param redcapUrl The URL of the RedCap project.
#' @param redcapReportId The ID of the RedCap report to download.
#' @return A tibble containing the contents of the RedCap report.
#' @import httr
#' @import readr
downloadRedcapReport <- function(redcapTokenName, redcapUrl, redcapReportId) {
  # Read API token from .REnviron file
  redcapToken <- Sys.getenv(redcapTokenName)
  
  # Construct formData
  formData <- list(
    "token" = redcapToken,
    "content" = 'report',
    "format" = 'csv',
    "report_id" = redcapReportId,
    "csvDelimiter" = '',
    "rawOrLabel" = 'raw',
    "rawOrLabelHeaders" = 'raw',
    "exportCheckboxLabel" = 'false',
    "returnFormat" = 'csv'
  )
  
  # Construct API URL
  api_url <- paste0(redcapUrl, "api/")
  
  # Perform API request
  response <- httr::POST(api_url, body = formData, encode = "form")
  
  # Check if request was successful
  if (httr::status_code(response) == 200) {
    # Read CSV response into tibble
    report_data <- readr::read_csv(httr::content(response, as = "text"))
    return(report_data)
  } else {
    # Print error message if request failed
    cat("Error:", httr::status_code(response), httr::status_reason(response), "\n")
    return(NULL)
  }
}

# Example usage:
# redcap_report <- downloadRedcapReport("redcapToken", "https://redcap.example.com/", "46524")
# print(redcap_report)
