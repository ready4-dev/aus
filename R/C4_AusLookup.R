#' AusLookup
#' 
#' Lookup tables for Australian geometry and spatial attribute data.
#' 
#' @slot vicinity_abbreviations_r3 Vicinity abbreviations (a ready4 S3)
#' @slot vicinity_raw_r3 Vicinity raw (a ready4 S3)
#' @slot vicinity_processed_r3 Vicinity processed (a ready4 S3)
#' @slot vicinity_resolutions_r3 Vicinity resolutions (a ready4 S3)
#' @slot vicinity_points_r3 Vicinity points (a ready4 S3)
#' @slot vicinity_templates_r3 Vicinity templates (a ready4 S3)
#' @slot vicinity_identifiers_r3 Vicinity identifiers (a ready4 S3)
#' @import vicinity
#' @name AusLookup-class
#' @rdname AusLookup-class
#' @export AusLookup
#' @exportClass AusLookup
AusLookup <- methods::setClass("AusLookup",
contains = "VicinityLookup",
slots = c(vicinity_abbreviations_r3 = "vicinity_abbreviations",vicinity_raw_r3 = "vicinity_raw",vicinity_processed_r3 = "vicinity_processed",vicinity_resolutions_r3 = "vicinity_resolutions",vicinity_points_r3 = "vicinity_points",vicinity_templates_r3 = "vicinity_templates",vicinity_identifiers_r3 = "vicinity_identifiers"),
prototype =  list(vicinity_abbreviations_r3 = vicinity::vicinity_abbreviations()))


methods::setValidity(methods::className("AusLookup"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
