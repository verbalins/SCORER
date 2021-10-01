# S3 Implementation
new_optresult <- function(data = tibble::tibble(),
                          metadata = list(),
                          ..., class = character()) {

  stopifnot(tibble::is_tibble(data))

  tibble::new_tibble(data,
                     nrow = nrow(data),
                     metadata = metadata,
                     class = c(class, "OptResult"),
                     ...)
}

#' @export
`$.OptResult` <- function(x, value, ...) {
  #stopifnot(is.OptResult(value))
  if (value %in% c("objectives", "objective_names", "inputs", "outputs",
                   "parameters", "opt_name", "opt_id",
                   "pareto", "metadata")) {
    if (value == "objective_names") {
      names(attr(x, "metadata")[["objectives"]])
    } else if (value == "pareto") {
      x %>% dplyr::filter(dplyr::across("Rank") == 1)
    } else if (value == "metadata") {
      attr(x, value)
    } else {
      attr(x, "metadata")[[value]]
    }
  } else {
    NextMethod()
  }
}

#' @export
`$<-.OptResult` <- function(x, name, value) {
  #stopifnot(is.OptResult(value))
  #var <- unlist(...)
  if (name %in% c("objectives", "inputs", "outputs",
                  "parameters", "opt_name", "opt_id",
                  "metadata")) {
    if (name == "metadata") {
      attr(x, "metadata") <- value
    } else {
      attr(x, "metadata")[[name]] <- value
    }
    return(x)
  } else {
    return(NextMethod())
  }
}

#' @export
#' @importFrom dplyr rowwise
#' @method rowwise OptResult
rowwise.OptResult <- function(data, ...) {
  out <- NextMethod(data, ...)
  `class<-`(out, unlist(lapply(class(out), function(x) if(x == "rowwise_df") c("rowwise_df", "OptResult") else x)))
}

#' @importFrom dplyr mutate
#' @method mutate rowwise_df
#' @export
mutate.rowwise_df <- function(data, ...) {
  out <- NextMethod(data, ...)
  attr(out, "metadata") <- attr(data, "metadata")
  `class<-`(out, unlist(lapply(class(out), function(x) if(x == "rowwise_df") c("rowwise_df", "OptResult") else x)))
}

#' @export
#' @importFrom dplyr ungroup
#' @method ungroup OptResult
ungroup.OptResult <- function(x, ...) {
  if (missing(...)) {
    out <- tibble::as_tibble(x)
    attr(out, "metadata") <- attr(x, "metadata")
    class(out) <- c("OptResult", class(out))
    out
  } else {
    return(NextMethod(x, ...))
  }
}

#' @export
#' @importFrom dplyr ungroup
#' @method ungroup rowwise_df
ungroup.rowwise_df <- function(x, ...) {
  if (missing(...)) {
    out <- tibble::as_tibble(x)
    attr(out, "metadata") <- attr(x, "metadata")
    class(out) <- c("OptResult", class(out))
    out
  } else {
    return(NextMethod(x, ...))
  }
}

#' An S3 class representing the optimization result, usually returned from [load_dataset()]
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
  new_optresult(data,
                metadata = list(opt_name = opt_name,
                opt_id = opt_id,
                objectives = objectives,
                inputs = inputs,
                outputs = outputs,
                parameters = parameters))
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
