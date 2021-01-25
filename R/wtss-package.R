#' @title Rwtss
#' @description An R client to the web time series service (WTSS)
#' 
#' @section Rwtss API:
#' 
#' Implements an R interface to a web time series service (WTSS) 
#' that offers time series of remote sensing data using a simple API. 
#' A WTSS server takes as input an Earth observation data cube, 
#' that has a spatial and a temporal dimension 
#' and can be multidimensional in terms of its attributes.
#' 
#' The WTSS API has four commands: 
#' \itemize{
#'    \item `wtss`: given an URL, creates a connection to a WTSS service
#'    \item `list_coverages`: returns a list of coverages (cubes) available 
#'        in the WTSS server.
#'    \item `describe_coverage`: returns the metadata for a given coverage.
#'    \item `time_series`: returns a time series for a spatio-temporal location.
#' }
#' 
#' @docType package
#' @name Rwtss-package
#' @aliases Rwtss
"_PACKAGE"
NULL
