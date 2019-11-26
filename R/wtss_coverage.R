#' @title Decodes the description from a WTSS coverage
#' @name  .wtss_coverage_description
#' 
#' @description creates a tibble to store the description of the WTSS coverage
#' @param wtss.obj  valid WTSS object
#' @param cov       coverage response provided by WTSS service
#' 
.wtss_coverage_description <- function(wtss.obj, cov){
    
    # retri
    name <- cov$name
    
    # Projection CRS
    crs <- cov$crs$proj4
    # retrieve the satellite associated to the cube
    satellite <- .wtss_config_satellite(name, crs)
    # retrieve the sensor associated to the cube
    sensor    <- .wtss_config_sensor(name, satellite)
    # temporal extent
    timeline <- lubridate::as_date(cov$timeline)
    
    # retrieve information about the bands
    band_info <- cov$attributes
    
    attr <- tibble::as.tibble(band_info)
    bands <- attr$name
    
    t <- dplyr::select(dplyr::filter(attr, name %in% bands), 
                       name, missing_value, scale_factor, valid_range)
    missing_values        <- t$missing_value
    names(missing_values) <- t$name
    
    scale_factors         <- t$scale_factor
    names(scale_factors)  <- t$name
    
    minimum_values        <- t$valid_range$min
    names(minimum_values) <- t$name
    
    maximum_values        <- t$valid_range$max
    names(maximum_values) <- t$name
    
    # Spatial extent
    xmin <- cov$spatial_extent$xmin
    ymin <- cov$spatial_extent$ymin
    xmax <- cov$spatial_extent$xmax
    ymax <- cov$spatial_extent$ymax
    
    # Spatial resolution
    xres <- cov$spatial_resolution$x
    yres <- cov$spatial_resolution$y
    
    # Size (rows and cols)
    nrows <- cov$dimension$y$max_idx - cov$dimensions$y$min_idx + 1
    ncols <- cov$dimension$x$max_idx - cov$dimensions$x$min_idx + 1
    
    # create a tibble to store the metadata
    cov.tb <- tibble::tibble(URL            = wtss.obj$url,
                             satellite      = satellite,
                             sensor         = sensor,
                             name           = name,
                             bands          = list(bands),
                             scale_factors  = list(scale_factors),
                             missing_values = list(missing_values),
                             minimum_values = list(minimum_values),
                             maximum_values = list(maximum_values),
                             timeline       = list(timeline),
                             nrows          = nrows,
                             ncols          = ncols,
                             xmin           = xmin,
                             xmax           = xmax,
                             ymin           = ymin,
                             ymax           = ymax,
                             xres           = xres,
                             yres           = yres,
                             crs            = crs)
    
    class(cov.tb) <- append(class(cov.tb), "coverage", after = 0)
    
    return(cov.tb)
}

.wtss_print_coverage <- function(cov.tb){
    cat("----------------------------------------------------------------------------------")
    cat(paste0("\nWTSS server URL = ", cov.tb$URL, "\n"))
    cat(paste0("Coverage = ", cov.tb$name))
    
    print(knitr::kable(dplyr::select(cov.tb, satellite, sensor, bands), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, scale_factors), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, minimum_values), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, maximum_values), padding = 0))
    print(knitr::kable(dplyr::select(cov.tb, nrows, ncols, xmin, xmax, ymin, ymax, xres, yres, crs),
                       padding = 0))
    # print the timeline
    timeline <- lubridate::as_date(cov.tb$timeline[[1]])
    n_time_steps <- length(timeline)
    cat(paste0("\nTimeline - ",n_time_steps," time steps\n"))
    cat(paste0("start_date: ", timeline[1], " end_date: ", timeline[n_time_steps],"\n"))
    cat("----------------------------------------------------------------------------------\n")
}
#' @title Retrieves the list of cubes from the URL server
#' @name .wtss_list_coverages
#'
#' @description Uses the WTSS services to print information and save metadata about a
#' chosen cube.
#'
#' @param wtss.obj      WTSS object associated to the service
#' @return              updated WTSS object.
.wtss_list_coverages <- function(wtss.obj) {
    
    items <- NULL
    ce <- 0
    
    url <- wtss.obj$url
    
    # concat list_coverages to the service URL 
    request <- paste(url,"/list_coverages",sep = "")
    
    # try only 10 times (avoid time out connection)
    while (purrr::is_null(items) & ce < 10) {
        items <- .wtss_parse_json(.wtss_send_request(request))
        ce <- ce + 1
    }
    
    # if the server does not answer any item
    if (purrr::is_null(items))
        return(NULL)
    
    # if the server answers correctly
    return(items$coverages)
}