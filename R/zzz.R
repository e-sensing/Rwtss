# On load
.onAttach <- function(lib, pkg){
    packageStartupMessage("wtss - web time series services.")
    packageStartupMessage(
        sprintf("Loaded wtss v%s.
        See ?wtss for help, citation(\"wtss\") for use in publication.
        See demo(package = \"wtss\") for examples.",
                utils::packageDescription("wtss")$Version) )
}

.onLoad <- function(lib, pkg) {
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    Sys.setenv(R_CONFIG_FILE = "config.yml")
    wtss_config()
}

# Creates a package environment to store global variables
wtss.env <- new.env()

utils::globalVariables(c(".", "%>%", "missing_value", "scale_factor", 
                         "valid_range"))