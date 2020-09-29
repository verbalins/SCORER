#S3 implementation
new_optresult <- function(data, opt_name, id, objectives, inputs, outputs) {
  tibble::validate_tibble(tibble::new_tibble(data, class = "OptResult", opt_name = opt_name, id = id, objectives = objectives, inputs = inputs, outputs = outputs))
}

#' Title
#'
#' @param data A tibble containing optimization results from OptimizeBrowser
#' @param opt_name A name for the optimization run
#' @param id id number of the optimization
#' @param objectives A named character vector
#' @param inputs A character vector with names of the inputs
#' @param outputs A character vector with names of the outputs
#'
#' @return An S3 class
#' @export
#'
optresult <- function(data, opt_name, id, objectives, inputs, outputs) {
  new_optresult(data, opt_name = opt_name, id = id, objectives = objectives, inputs = inputs, outputs = outputs)
}


#S4 implementation
# setOldClass(c("tbl_df", "tbl", "data.frame"))
#
# setClass("OptResults",
#          slots = c(
#            opt_name = "character",
#            id = "numeric",
#            objectives = "logical",
#            inputs = "character",
#            outputs = "character"
#          ),
#          data = function(x) show(x),
#          contains = "tbl_df")
#
# setMethod("show", signature(object="OptResults"), function(object) {
#   as(object, "S3")
# })
#
# OptResults <- function(opt_name, id, objectives, inputs, outputs, data) {
#   new("OptResults", data, opt_name = opt_name, id = id, objectives = objectives, inputs = inputs, outputs = outputs)
# }
