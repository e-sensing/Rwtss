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

utils::globalVariables(c(".", "%>%", "missing_value", "missing_values",
                         "scale_factor", "scale_factors",
                         "minimum_values","maximum_values", "nrows", 
                         "ncols", "xmin", "xmax", "ymin", "ymax", 
                         "xres", "yres", "crs",
                         "satellite", "sensor", "bands", 
                         "Index", "value", "variable",
                         "V1", "med", "qt25", "qt75", "valid_range"))
