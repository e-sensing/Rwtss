#'@title Reads a configuration file and loads it in the main environment
#' @name wtss_config
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Reads a user-specified configuration file, located in a "config.yml" file
#' in the working directory. If this file is not found, reads a default package configuration file.
#' By default, the wtss configuration file "config.yml" is located at the directory "extdata" of the
#' package. The configuration file is an YAML file that should provide at least the following parameters:
#'
#' default:
#'    WTSS_server    : "http://www.dpi.inpe.br/tws/wtss"
#'
#' To see the contents of the configuration file, please use \code{\link[wtss]{wtss_config_show}}.
#'
#' @return A list with the configuration parameters used by wtss.
#' @examples
#' # create configurtion file
#' config_wtss <- wtss_config()
#' # show configuration file
#' wtss_config_show()
#' @export
wtss_config <- function() {
    # run the default configuration file
    yml_file <- system.file("extdata", "config.yml", package = "wtss")
    
    # check that the file is valid
    ensurer::ensure_that(yml_file, !purrr::is_null(.),
                         err_desc = "wtss_config : invalid configuration file")
    
    # read the configuration parameters
    wtss.env$config <- config::get(file = yml_file)
    
    # try to find a valid user configuration file
    # check if we are running in Windows
    if (.Platform$OS.type != "unix")
        user_yml_file   <- c("~/wtss/config.yml")
    else
        user_yml_file   <- c("~/.wtss/config.yml")
    
    if (file.exists(user_yml_file)) {
        config_user     <- config::get(file = user_yml_file)
        wtss.env$config <- config::merge(wtss.env$config, config_user)
    }
    
    return(invisible(wtss.env$config))
}


#' @title Shows the contents of the wtss configuration file
#' @name wtss_config_show
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#'
#' @description Displays the contents of wtss configuration file. For more details
#' on how to set the configuration file, please use \code{\link[wtss]{wtss_config}}.
#'
#' @return List with the configuration parameters used by wtss.
#' @examples
#' wtss_config_show()
#' @export
wtss_config_show <- function() {
    # retrieve the basic configuration file
    yml_file <- system.file("extdata", "config.yml", package = "wtss")
    # check that the file is valid
    ensurer::ensure_that(yml_file, !purrr::is_null(.),
                         err_desc = "wtss_config: Invalid configuration file")
    # try to find a valid user configuration file
    if (file.exists("~/.wtss/config.yml"))
        yml_user_file <- c("~/.wtss/config.yml")
    else
        yml_user_file <- NULL
    
    # read the configuration parameters
    message("Default system configuration file")
    cat(readLines(yml_file), sep = "\n")
    if (!purrr::is_null(yml_user_file)) {
        message("User configuration file - overrides default config")
        cat(readLines(yml_user_file), sep = "\n")
    }
    
    return(invisible())
}



#' @title retrieve the satellite associated to a given cube
#' @name .wtss_config_satellite
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param name Name of the covergae
#'
#' @return List of providers associated to a service
.wtss_config_satellite <- function(name) {
    p <- paste0(name,"_satellite")
    s <- wtss.env$config[[p]]
    #post-condition
    ensurer::ensure_that(s, !purrr::is_null(.),
                         err_desc = paste0("Could not find satellite for cube ", name))
    return(s)
}

#' @title retrieve the sensor associated to a data cube
#' @name .wtss_config_sensor
#' @author Gilberto Camara, \email{gilberto.camara@@inpe.br}
#' @param name Name of the coverage
#'
#' @return List of providers associated to a service
.wtss_config_sensor <- function(name) {
    p <- paste0(name,"_sensor")
    s <- wtss.env$config[[p]]
    #post-condition
    ensurer::ensure_that(s, !purrr::is_null(.),
                         err_desc = paste0("Could not find sensor for coverage ", name))
    return(s)
}
.wtss_error_handle <- function(){
    message("Error in WTSS function")
    stop()
}