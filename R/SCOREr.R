#S3 implementation
new_optresult <- function(data, opt_name, opt_id, objectives, inputs, outputs, parameters) {
  tibble::validate_tibble(tibble::new_tibble(data, class = "OptResult",
                                             opt_name = opt_name,
                                             opt_id = opt_id,
                                             objectives = objectives,
                                             inputs = inputs,
                                             outputs = outputs,
                                             parameters = parameters))
}

#' @export
`$.OptResult` <- function(x, value, ...) {
  #stopifnot(is.OptResult(value))
  if (value %in% c("objectives", "inputs", "outputs", "parameters", "opt_name", "opt_id")) {
    if (value=="objectives"){
      names(attr(x, value))
    } else {
      attr(x, value)
    }
  } else {
    NextMethod(x, value, ...)
  }
}

#' @export
`$<-.OptResult` <- function(x, name, value) {
  #stopifnot(is.OptResult(value))
  #var <- unlist(...)
  if (name %in% c("objectives", "inputs", "outputs", "parameters", "opt_name", "opt_id")) {
    if (name =="objectives"){
      names(attr(x, name)) <- value
    } else {
      attr(x, name) <- value
    }
    return(x)
  } else {
    NextMethod(x, name, value)
  }
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
optresult <- function(data, opt_name, opt_id=1, objectives, inputs, outputs, parameters) {
  new_optresult(data, opt_name = opt_name, opt_id = opt_id, objectives = objectives, inputs = inputs, outputs = outputs, parameters = parameters)
}


#S4 implementation
#setOldClass(c("tbl_df", "tbl", "data.frame"))
# setOldClass("data.frame")
# setClass(Class="OptResult",
#          slots = c(
#            opt_name = "character",
#            id = "numeric",
#            objectives = "logical",
#            inputs = "character",
#            outputs = "character",
#            parameters = "character"
#          ),
#          #data = function(x) show(x),
#          contains = "data.frame")#c("tbl_df", "tbl", "data.frame"))#class(tibble::tibble())

# setMethod("show", signature(object="OptResult"), function(object) {
#   object
# })
# setGeneric("print", function(object) object)
# setMethod("print", signature(object="OptResult"),function(object) {
#   object
# })

# OptResults <- function(data, opt_name, id=1, objectives, inputs, outputs, parameters) {
#   new("OptResult", data, opt_name = opt_name, id = id, objectives = objectives, inputs = inputs, outputs = outputs, parameters = parameters)
# }
#
# OptResultR6 <- R6::R6Class("OptResult",
#   public = list(
#     opt_name = "character",
#     id = "numeric",
#     objectives = "logical",
#     inputs = "character",
#     outputs = "character",
#     parameters = "character",
#     data = class(tibble::tibble()),
#     initialize = function(data, opt_name, id=1, objectives, inputs, outputs, parameters) {
#       self$data <- tibble::validate_tibble(tibble::new_tibble(data))
#       self$opt_name <- opt_name
#       self$id <- id
#       self$objectives <- objectives
#       self$inputs <- inputs
#       self$outputs <- outputs
#       self$parameters <- parameters
#     },
#     print = function(...) {
#       print(self$data)
#       #invisible(self)
#     })
# )
#
# newR6 <- function(data, opt_name, id=1, objectives, inputs, outputs, parameters) {
#   OptResultR6$new(data, opt_name = opt_name, id = id, objectives = objectives, inputs = inputs, outputs = outputs, parameters = parameters)
# }
