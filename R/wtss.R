#' @title List the coverages available in the WTSS service
#' @name list_coverages
#'
#' @description Lists coverages available in the WTSS service 
#'
#' @param URL       URL of the server
#' @return          vector with coverage name
#' @examples
#' \dontrun{
#' # Using external server 
#' list_coverages("https://brazildatacube.dpi.inpe.br/wtss/")
#' }
#' @export
list_coverages <- function(URL) {
  
  # adjust the URL
  URL <- .wtss_remove_trailing_dash(URL)
  
  # try to retrieve the coverage list
  coverages <- .wtss_list_coverages(URL)
  
  # if the coverage list is NULL, the wtss.obj is invalid
  if (purrr::is_null(coverages)) {
    message(paste0("WTSS server at URL ", URL, " not responding"))
  }
  
  return(coverages)
}

#' @title Retrieves the list of cubes from the URL server
#' @name  describe_coverage
#'
#' @description Contacts the WTSS server to describe one coverage
#' @param URL         URL of the server
#' @param name        name of coverage
#' @param .print      Print the coverage description
#' @return            tibble with coverage description
#' 
#' @examples
#' \dontrun{
#' # Using external server 
#' describe_coverage("https://brazildatacube.dpi.inpe.br/wtss/", 
#'                   "LC8_30_16D_STK-1")
#' }
#' @export
describe_coverage <- function(URL, name, .print = TRUE) {
  
  # adjust the URL
  URL <- .wtss_remove_trailing_dash(URL)
  # only one coverage at a time
  assertthat::assert_that(length(name) == 1, 
                          msg = "Rwtss - select only one coverage to describe")
  result <- NULL
  
  # build a "describe_coverage" request
  request <- paste(URL,"/describe_coverage?name=", name, sep = "")
  # convert the coverage description into a tibble
  result <- .wtss_process_request(request)
  
  # if the coverage list is NULL, the wtss.obj is invalid
  if (purrr::is_null(result)) {
    message(paste0("WTSS service at URL ", URL, "not responding"))
    return(NULL)
  }
  else {
    cov.tb <- .wtss_coverage_description(URL, result)
    # print the content of the coverage
    if (.print)
      .wtss_print_coverage(cov.tb)
  }
  return(invisible(cov.tb))
}

#' @title Get time series
#' @name time_series
#' @author  Gilberto Camara
#' @description Retrieves the time series for a pair of coordinates 
#' 
#' @param URL           URL of the server
#' @param name          Coverage name.
#' @param attributes    Vector of band names.
#' @param longitude     Longitude in WGS84 coordinate system.
#' @param latitude      Latitude in WGS84 coordinate system.
#' @param start_date    Start date in the format yyyy-mm-dd or yyyy-mm 
#'                      depending on the coverage.
#' @param end_date      End date in the format yyyy-mm-dd or yyyy-mm 
#'                      depending on the coverage.
#' @param token         A character with token to be add in URL.
#' @param ...           Additional parameters that can be added in httr.
#' @return              time series in a tibble format (NULL)
#' @examples
#' \dontrun{
#' # connect to a WTSS server
#' wtss_server <- "https://brazildatacube.dpi.inpe.br/wtss/"
#' # retrieve a time series
#' ndvi_ts <- Rwtss::time_series(wtss_server, 
#'                               "LC8_30_16D_STK-1", 
#'                               attributes = "NDVI", 
#'                               latitude = -14.31, 
#'                               longitude = -51.16,
#'                               token = "YOUR-BDC-TOKEN")
#' # plot the time series
#' plot(ndvi_ts)
#' }
#'@export
time_series <- function(URL,
                        name,
                        attributes = NULL,
                        longitude,
                        latitude,
                        start_date = NULL,
                        end_date   = NULL,
                        token = NULL,
                        ...) {
  # clean the URL
  URL <- .wtss_remove_trailing_dash(URL)
  
  # have we described the coverage before?
  # if not, get the coverage description
  # if the description of the coverage is available, skip this part
  if (purrr::is_null(wtss.env$desc) || wtss.env$desc$name != name) {
    wtss.env$desc <-  Rwtss::describe_coverage(URL, name, .print = FALSE)
    # if describe_coverage fails, return a default time series
    
    if (purrr::is_null(wtss.env$desc)) {
      message(paste0("Rwtss - could not retrieve description of coverage ",
                     name, " from WTSS server"))
      message(paste0("Rwtss - retrieving backup time series"))
      
      ts.tb <- readRDS(file = system.file("extdata/ndvi_ts.rds", 
                                          package = "Rwtss"))
      return(ts.tb)
    }      
  }
  
  # check if the selected attributes are available
  cov_bands <- wtss.env$desc$bands[[1]]
  if (purrr::is_null(attributes))
    attributes <- cov_bands
  if (!all(attributes %in% cov_bands)) {
    message("Rwtss - attributes not available.")
    return(NULL)
  }
  
  # check bounds for latitude and longitude
  if (longitude < wtss.env$desc$xmin || longitude > wtss.env$desc$xmax) {
    message("Rwtss - invalid longitude value")
    return(NULL)
  }
  if (latitude < wtss.env$desc$ymin || latitude > wtss.env$desc$ymax) {
    message("Rwtss - invalid latitude value")
    return(NULL)
  }
  
  # check start and end date
  timeline <- wtss.env$desc$timeline[[1]]
  n_dates  <- length(timeline)
  
  if (purrr::is_null(start_date))
    start_date <- lubridate::as_date(timeline[1])
  if (purrr::is_null(end_date))
    end_date <- lubridate::as_date(timeline[n_dates])
  
  # test is start date is valid
  if (lubridate::as_date(start_date) < lubridate::as_date(timeline[1]) ||
      lubridate::as_date(start_date) > lubridate::as_date(timeline[n_dates])) {
    message("Rwtss - invalid start date")
    return(NULL)
  }
  
  # test if end date is valid
  if (lubridate::as_date(end_date) <  lubridate::as_date(timeline[1]) ||
      lubridate::as_date(end_date) >  lubridate::as_date(timeline[n_dates]) ||
      lubridate::as_date(end_date) <  lubridate::as_date(start_date)) {
    message("Rwtss - invalid end date")
    return(NULL)
  }
  items <- NULL
  ce <- 0
  # try to retrieve the time series 
  request <- paste(URL,"/time_series?coverage=", name, 
                   "&attributes=", paste(attributes, collapse = ","),
                   "&longitude=", longitude, 
                   "&latitude=", latitude,
                   "&start_date=", start_date, 
                   "&end_date=", end_date, sep = "")
  
  if (!is.null(token))
    request <- paste(request, "&access_token=", token, sep = "")
  
  # send a request to the WTSS server
  response <- .wtss_send_request(request, ...)
  # parse the response 
  items <- .wtss_parse_json(response)
  
  # if the server does not answer any item
  if (purrr::is_null(items)) {
    
    ts.tb <- readRDS(file = system.file("extdata/ndvi_ts.rds", 
                                        package = "wtss"))
    return(ts.tb)
  }
  
  if (length(items$result$attributes) == 0)
    stop(paste("The requisition returns zero attributes as result.",
    "Please check your request or contact the server maintenance."))
  
  # process the response         
  result <- list(.wtss_time_series_processing(items))
  
  names(result) <- name
  # convert to tibble 
  ts.tb <- .wtss_to_tibble(result, name, attributes, longitude, latitude, 
                           start_date, end_date, wtss.env$desc)
  #append class         
  class(ts.tb) <- append(class(ts.tb), c("wtss"), after = 0)
  
  return(ts.tb)
}
