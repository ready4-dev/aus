#' Import data methods
#' @param x Ready4_ S4 object
#' @name import_data

#' @importFrom ready4use import_data
#' @rdname import_data
#' @export
methods::setMethod("import_data",
                   "Aus_16_31_oyh",
                   function(x) {
                     add_aus_oyh_data(x)
                   })

#' @importFrom ready4use import_data
#' @rdname import_data
#' @export
methods::setMethod("import_data",
                   "Aus_16_31_hss",
                   function(x) {
                     add_aus_hss_data(x)
                   })

#' @importFrom ready4use import_data
#' @rdname import_data
#' @export
methods::setMethod("import_data",
                   "Aus_16_31_xx1",
                   function(x) {
                     add_ppr_custom_bound_data(x)
                   })

#' @importFrom ready4use import_data
#' @rdname import_data
#' @export
methods::setMethod("import_data",
                   "Aus_16_31_make_act_ppr_2016",
                   function(x) {
                     make_act_ppr_2016(x)
                   })

#' @importFrom ready4use import_data
#' @rdname import_data
#' @export
methods::setMethod("import_data",
                   "Aus_16_31_add_tas_ppr_data",
                   function(x) {
                     add_tas_ppr_data(x)
                   })
