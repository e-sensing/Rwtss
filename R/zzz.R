# On load
.onAttach <- function(lib, pkg){
    packageStartupMessage("Rwtss - R interface to Web Time Series Service.")
    packageStartupMessage(
        sprintf("Loaded Rwtss v%s.
        See ?Rwtss for help, citation(\"Rwtss\") for use in publication.
        See demo(package = \"Rwtss\") for examples.",
                utils::packageDescription("Rwtss")$Version) )
}

.onLoad <- function(lib, pkg) {
}

# Creates a package environment to store global variables
wtss.env <- new.env()
wtss.env$desc <- NULL

utils::globalVariables(c(".", "%>%", "missing_value", "missing_values",
                         "scale_factor", "scale_factors",
                         "minimum_values","maximum_values", "nrows", 
                         "ncols", "xmin", "xmax", "ymin", "ymax", 
                         "xres", "yres", "crs",
                         "satellite", "sensor", "bands", 
                         "Index", "value", "variable",
                         "V1", "med", "qt25", "qt75", "valid_range"))
