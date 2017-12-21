#' The WTSS class
#'
#' Use this class for creating a connection to a Web Time Series Service (WTSS)
#'   
#'@section Slots :
#' \describe{
#' \item{\code{serverUrl}:}{Object of class \code{"character"}, URL of the server.}
#' \item{\code{listCoverages}:}{Object of class \code{"character"}, list of coverages.}
#' }
#'
#' @note No notes
#' @name WTSS
#' @aliases WTSS-class
#' @exportClass WTSS
#' @author Victor Maus, Alber Sanchez, Luiz Fernando Assis, Pedro Andrade, Gilberto Ribeiro
#' @import jsonlite
#' @import methods
#' @import RCurl
#' @import roxygen2
#' @import lubridate
#' @import zoo
setClass(
  
  # Set the name for the class
  Class = "WTSS",
  
  # Define the slots
  slots = c(
    serverUrl = "character",
    listCoverages = "character"
  ),
  
  # Set the default values for the slots.
  prototype=list(),
  
  # Test if the data is consistent. Only called if no initialize function is defined.
  validity = function(object) {
    
    if(length(object@serverUrl) != 1) {
      stop ("[WTSS: validation] Invalid server URL.")
    }
    
    if(nchar(object@serverUrl) <= 1) {
      stop ("[WTSS: validation] Invalid server URL.")
    }
    
    return(TRUE)
    
  }
  
)

#*******************************************************
#CONSTRUCTOR
#*******************************************************
setMethod(
  
  # initialize function
  f = "initialize",
  
  # Method signature
  signature = "WTSS",
  
  # Function definition
  definition = function(.Object, serverUrl) {
    
    # check whether the url is missing or not
    if(!missing(serverUrl)) {
      
      .Object@serverUrl <- serverUrl
      arrays <- .listCoverages(.Object)
      
      if(class(arrays) == "try-error")
        stop(arrays)
      else
        .Object@listCoverages <- arrays
      
      validObject(.Object)
      
    }
    else {
      .Object@serverUrl <- character(0)
    }
    
    return(.Object)
  }
)

#' Creates a WTSS object
#'
#' @param serverUrl A server URL
#' @rdname WTSS
#' @docType methods
#' @export
#' @examples
#' ts.server = WTSS("http://www.dpi.inpe.br/tws/wtss")
#' ts.server
WTSS <- function(serverUrl) {
  
  methods::new (Class="WTSS", serverUrl = serverUrl)
  
}

#*******************************************************
#SHOW
#*******************************************************
setMethod(
  
  # Name of the function
  f = "show", 
  
  # Method signature
  signature = "WTSS", 
  
  # Stylish print of the objects
  definition = function(object) {
    
    # initial message
    cat(paste("Object of Class WTSS\n\n"))
    
    # print serverUrl
    cat(paste("serverUrl: ",paste(object@serverUrl),  "\n"))
    
    # print listCoverages
    cat("listCoverages: ")
    cat(paste(object@listCoverages), " ")
    
    return(invisible())
  }
)

setGeneric(name="show", def=function(object){standardGeneric("show")})

#*******************************************************
#ACCESSORS
#*******************************************************

#' Returns the object's server URL
#'
#' @param object A WTSS object
#' @docType methods
#' @aliases getServerUrl-generic
#' @export

setGeneric("getServerUrl",function(object){standardGeneric ("getServerUrl")})

#' @rdname getServerUrl
setMethod("getServerUrl","WTSS", function(object) {
  
  if(substr(object@serverUrl,nchar(object@serverUrl),nchar(object@serverUrl))!="/")
    return(paste(object@serverUrl,"/",sep=""))
  
  return(object@serverUrl)
  
})

#' Sets the object's server URL
#'
#' @param object A WTSS object
#' @param aServerUrl A character representing the server URL.
#' @docType methods
#' @export
setGeneric("setServerUrl",function(object, aServerUrl){standardGeneric ("setServerUrl")})

#' @rdname  setServerUrl
setMethod("setServerUrl","WTSS", function(object, aServerUrl) {
  
  object@serverUrl <- aServerUrl
  
})

#' List coverages 
#'
#' @param object A WTSS object
#' @docType methods
#' @export
#' @examples
#' ts.server = WTSS("http://www.dpi.inpe.br/tws/wtss")
#' coverages = listCoverages(ts.server)
#' 
setGeneric("listCoverages",function(object){standardGeneric ("listCoverages")})

#' @rdname  listCoverages
setMethod("listCoverages","WTSS", function(object) {
  
  .listCoverages(object) 
  
})

.listCoverages <- function(object) {
  
  url <- getServerUrl(object)
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  if (length(url) == 1 && nchar(url) > 1) {
    
    # concat list_coverages to the service URL 
    request <- paste(url,"list_coverages",sep="")
    
    # try only 10 times (avoid time out connection)
    while(class(items) == "try-error" & ce < 10) {
      items <- .parseJSON(.sendRequest(request))
      ce <- ce + 1
    }
    
    # if the server does not answer any item
    if (class(items) == "try-error")
      return(items)
      
    # if the server answers correctly
    return(unlist(items, use.names = FALSE))
    
  }
}

#' Describe coverage
#'
#' @param object A WTSS object
#' @param coverages A character vector of coverage names
#' @docType methods
#' @export
#' @examples
#' ts.server = WTSS("http://www.dpi.inpe.br/tws/wtss")
#' cv = describeCoverage(ts.server, c("mod13q1_512"))
#' 
setGeneric("describeCoverage",function(object,coverages){standardGeneric("describeCoverage")})


#' @rdname  describeCoverage
setMethod("describeCoverage","WTSS", function(object,coverages) {
  .describeCoverage(object,coverages) 
})

.describeCoverage <- function(object,coverages) {
  
  url <- getServerUrl(object)
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  if(length(url) == 1 && nchar(url) > 1) {
    
    out <- lapply(coverages, function(cov) {
        
        # concat describe_coverage according to a name into the service URL 
        request <- paste(url,"describe_coverage?name=", cov, sep="")
        
        # avoid time out connection 
        while(class(items) == "try-error" & ce < 10) {
           items <- .parseJSON(.sendRequest(request))
           ce <- ce + 1
        }
        
        # if the server does not answer any item
        if (class(items) == "try-error")
          return(items)
        
        return(items)
    })
    
    names(out) <- coverages
    
    return(out)
  
  }
  
}

#' Get list of time series
#'
#' @description This function retrieves the time series for a list of coordinates.
#'
#' @param object Either a WTSS object or a server URL.
#' @param coverages Either a list of coverages and attributes such as retrieved by describe_coverage() or a character with the coverage name.
#' @param attributes A character vector of dataset names.
#' @param coordinates A list or data frame of longitude latitude coordinates in WGS84 coordinate system.
#' @param start_date A character with the start date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @param end_date A character with the end date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @docType methods
#' @export
#' @examples
#' \donttest{
#' ts.server <-  WTSS("http://www.dpi.inpe.br/tws/wtss")
#' bands <- c("ndvi","evi")
#' coords <- list (c(-45.00, -12.00), c(-46.00, -11.00))
#' tsList <- listTimeSeries(ts.server, "mod13q1_512", bands, coords, "2000-02-18", "2016-12-18")
#'}
setGeneric("listTimeSeries",function(object,coverages,attributes,coordinates,start_date,end_date){standardGeneric("listTimeSeries")})

#' @rdname  listTimeSeries
setMethod("listTimeSeries","WTSS", function(object,coverages,attributes,coordinates,start_date,end_date) {
  
  # check type of the list of coordinates 
  if( is.data.frame(coordinates) | is.matrix(coordinates))
    coordinates <- lapply(1:dim(coordinates)[1], function(i) coordinates[i,])
  
  if(!is.list(coordinates))
    stop("Missing a list. Please insert a list of longitude latitude coordinates in WGS84 coordinate system.")
  
  out <- lapply(coordinates, function(coords) {
    longitude <- coords[1]
    latitude <- coords[2]
    items <- .timeSeries(object,coverages,attributes,longitude,latitude,start_date,end_date)
  })
  
  return(out)
  
})

#' Get time series
#'
#' @description This function retrieves the time series for a pair of coordinates.es
#' 
#' @param object Either a WTSS object or a server URL
#' @param coverages Either a list of coverages and attributes such as retrieved by describe_coverage() or a character with the coverage name.
#' @param attributes A character vector of dataset names.
#' @param longitude A longitude in WGS84 coordinate system.
#' @param latitude A latitude in WGS84 coordinate system.
#' @param start_date A character with the start date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @param end_date A character with the end date in the format yyyy-mm-dd or yyyy-mm depending on the coverage.
#' @docType methods
#' @export
#' @examples
#' \donttest{
#' ts.server = WTSS("http://www.dpi.inpe.br/tws/wtss")
#' ts = timeSeries(ts.server, "mod13q1_512", c("ndvi","evi"), 
#'                 longitude = -45.00, latitude  = -12.00,
#'                 start_date = "2000-02-18", end_date = "2016-12-18")
#'}
setGeneric("timeSeries",function(object,coverages,attributes,longitude,latitude,start_date,end_date){standardGeneric("timeSeries")})

#' @rdname  timeSeries
setMethod("timeSeries","WTSS", function(object,coverages,attributes,longitude,latitude,start_date,end_date) {
  
  .timeSeries(object,coverages,attributes,longitude,latitude,start_date,end_date)
  
})

.timeSeries <- function(object,coverages,attributes,longitude,latitude,start_date,end_date) {
  
  if(missing(object))
    stop("Missing either a WTSS object or a server URL.")
  
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  
  url <- object
  
  if(class(object)=="WTSS")
    url <- getServerUrl(object)
  
  if(length(url) == 1 && nchar(url) > 1) {
    
    if(is.list(coverages)) {
      
      out <- lapply(names(coverages), function(cov) {
        
        request <- paste(url,"time_series?coverage=",cov,"&attributes=",paste(attributes, collapse=","),
                         "&longitude=",longitude,"&latitude=",latitude,
                         "&start_date=",start_date,"&end_date=",end_date,sep="")
        
        # try only 10 times (avoid time out connection)
        while(class(items) == "try-error" & ce < 10) {
          items <- .parseJSON(.sendRequest(request))
          ce <- ce + 1
        }
        
        # if the server does not answer any item
        if (class(items) == "try-error") {
          stop("\n Server connection timeout. Verify the URL or try again later.")
          return(items)
        }
        
        # time series processing method call 
        timeseries <- .timeSeriesProcessing(items)
        
        return(timeseries)
        
      })
      
      names(out) <- names(coverages)
      
      return(out)
      
    } 
    else 
        if(is.character(coverages) && length(coverages)==1 && is.character(attributes)) {
          
            request <- paste(url,"time_series?coverage=",coverages,"&attributes=",paste(attributes, collapse=","),
                             "&longitude=",longitude,"&latitude=",latitude,
                             "&start_date=",start_date,"&end_date=",end_date,sep="")
            
            # try only 10 times (avoid time out connection)
            while(class(items) == "try-error" & ce < 10) {
              items <- .parseJSON(.sendRequest(request))
              ce <- ce + 1
            }
      
            # if the server does not answer any item
            if (class(items) == "try-error") {
              stop("\n Server connection timeout. Verify the URL or try again later.")
              return(items)
            }
            
            out <- list(.timeSeriesProcessing(items))
            
            names(out) <- coverages
            
            return(out)
        }
        else {
            stop("Missing either a list of coverages and attributes such as retrieved by describe_coverage()
           or a character with the coverage name and a character vector of dataset names.")
        }
  }
  
  return(NULL)
  
}

.timeSeriesProcessing <- function(items) {
  
  attributes_list <- list(items$result$attributes)
  
  attributes.processed <- lapply(attributes_list, function(subdataset) {
      # assign attribute values 
      value <- subdataset$values
      
      # assign values to dataframe
      value <- data.frame(value, stringsAsFactors = FALSE)
      
      # dataset names to the values vectors 
      names(value) <- subdataset$attribute

      return(value)
      
  })
  
  attributes.processed <- data.frame(attributes.processed, stringsAsFactors = FALSE)
  
  # convert string into date format
  timeline <- unlist(strsplit(items$result$timeline, split=" "))
  
  # check date format
  format <- lubridate::guess_formats(timeline[1], c("%Y-%m-%d", "%Y-%m"))
  
  # if monthly date
  if(any(format == "%Y-%m"))
      timeline = as.Date(zoo::as.yearmon(timeline))
  else # if weekly or daily date
      if(any(format == "%Y-%m-%d"))
        timeline = as.Date(timeline, format)
  
  return(list(center_coordinate = data.frame(longitude=items$result$coordinates$longitude, latitude=items$result$coordinates$latitude), 
              attributes = zoo::zoo(attributes.processed, timeline)))
  
}

.sendRequest <- function(request) {
  
  # check if URL exists and perform the request
  tryCatch(response <- RCurl::getURL(request), error = function(e) {
    e$message <- paste("HTTP request failed. The URL server may be incorrect or the service may be temporarily unavailable."); 
    stop(e);
  })
  
  return(response)
      
}

.parseJSON <- function(response) {
  
  # validate json
  if (validate(response)) {
    json_response <- jsonlite::fromJSON(response)
    if("exception" %in% names(json_response))
      stop(json_response)
    return(json_response)
  }
  else
    stop(response)
  
}
