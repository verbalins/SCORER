#' Load optimization data
#'
#' @export
#' @param filename A csv file containing formatted optimization data from Opt
#' @param objectives A character vector naming the objectives
#' @param inputs A character vector naming the inputs
#' @param outputs A character vector naming the outputs
#' @param custom_outputs A character vector describing custom outputs
#'
#' @return A dataframe with the data
load_dataset <- function(filename,
                        objectives = NULL,
                        inputs = NULL,
                        outputs = NULL,
                        custom_outputs = NULL) {

  opt_info <- readr::read_lines(filename, skip_empty_rows = TRUE) %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    stringr::str_replace_all(";$", "") %>% # Remove last delimiter if present
    stringr::str_replace_all(",", ".")

  cust_locale <- readr::locale(decimal_mark = ".")

  opt <- opt_info %>%
    utils::head(., -4) %>%
    readr::read_delim(delim = ";", trim_ws = TRUE, locale = cust_locale) %>%
    dplyr::select(-dplyr::any_of(c("Replications",
                                   "Error",
                                   "ConstraintViolation",
                                   "Iteration_1")),
                  maxOut = dplyr::starts_with("maxTP")) %>%
    dplyr::select(where(function(x) { !all(is.na(x)) }))

  # TODO: Evaluate if parameters are at the bottom.
  # If so, extract them
  opt_info <- opt_info %>%
    utils::tail(., 4) %>%
    paste(collapse = "\n")

  opt_id <- opt_info %>%
    readr::read_delim(delim = ";", n_max = 1, col_names = FALSE,
                      trim_ws = T, locale = cust_locale) %>%
    dplyr::pull(2)

  opt_parameters <- colnames(opt) %>%
    stringr::str_remove_all(pattern = "Iteration|Rank|ConstraintViolation|Replications|Error") %>%
    .[. != ""]

  opt_objectives <- opt_info %>%
    readr::read_delim(delim = ";", skip = 3, n_max = 1, col_names = FALSE,
                      trim_ws = T, locale = cust_locale) %>%
    dplyr::select_if(grepl("Min|Max", .)) %>%
    grepl("Max", .)

  names(opt_objectives) <- utils::head(stringr::str_replace(opt_parameters,
                                                            "maxTP",
                                                            "maxOut"),
                                       length(opt_objectives))

  opt_parameters <- utils::tail(opt_parameters, -length(opt_objectives))

  # TODO: Change this to enable importing of custom optimization examples
  if (is.null(inputs) || length(inputs) == 0) {
    # Try general SCORE naming
    if (any(grep("^imp_", opt_parameters))) {
      inputs <- grep("^imp_", opt_parameters, value = TRUE)
    } else {
      # Set inputs to everything other than outputs and objectives
      inputs <- opt_parameters[!(opt_parameters %in% names(opt_objectives))]
    }
  }

  if (is.null(outputs)) {
    outputs <- opt_parameters[!(opt_parameters %in% inputs)]
  }

  # Add Iteration if it doesn't exist, set Iteration in loaded order
  if (!("Iteration" %in% colnames(opt))) {
    opt <- opt %>%
      dplyr::mutate(Iteration = seq(1, nrow(.)),
                    .before = names(opt_objectives)[1])
  }

  # Add Rank if it doesn't exist, for all objectives
  if (!("Rank" %in% colnames(opt))) {
    opt <- opt %>% ndsecr(objectives = opt_objectives)
  }

  opt_results <- optresult(data = opt,
                           opt_name = tools::file_path_sans_ext(basename(filename)),
                           opt_id = opt_id,
                           objectives = opt_objectives,
                           inputs = inputs,
                           outputs = outputs,
                           parameters = opt_parameters)
  return(opt_results)
}
