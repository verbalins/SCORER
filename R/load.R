#' @importFrom magrittr %>%
NULL

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

  # Determine type of delimiter.
  if (readr::read_lines(filename, n_max = 1, progress = FALSE) %>%
      stringr::str_count(pattern = ",") > 0) {
    delimiter <- ","
  } else {
    delimiter <- ";"
  }

  # Determine whether or not to use lazy reading depending on OS type.
  lazyreading <- .Platform$OS.type != "windows"

  # Read lines and transform the data to be homogeneous.
  opt_info <- readr::read_lines(filename,
                                skip_empty_rows = TRUE,
                                progress = FALSE,
                                lazy = lazyreading) %>%
    stringr::str_replace_all("[\r\n]", "") %>%
    stringr::str_replace_all(paste0(delimiter, "$"), "") # Remove last delimiter of new line if present

  custom_locale <- readr::locale(decimal_mark = ".")

  # TODO: Evaluate if parameters are at the bottom.
  # If so, extract them
  if (grepl("OptId", opt_info %>%
            utils::tail(., 4) %>% .[1]))  {
    opt <- opt_info %>%
      utils::head(., -4)

    opt_info <- opt_info %>%
      utils::tail(., 4)

    opt <- readr::read_delim(file = I(opt), trim_ws = TRUE,
                             delim = delimiter, show_col_types = FALSE,
                             locale = custom_locale,
                             progress = FALSE,
                             name_repair = ~janitor::make_clean_names(.,
                                                                      case = "none",
                                                                      sep_out = "_"),
                             lazy = lazyreading) %>%
      dplyr::select(-dplyr::any_of(c("Replications",
                                     "Error",
                                     "ConstraintViolation",
                                     "Iteration_2")),
                    maxOut = dplyr::starts_with("maxTP")) %>%
      janitor::remove_empty(which = "cols")

    opt_id <- readr::read_delim(file = I(opt_info[[1]]),
                                delim = delimiter,
                                col_names = FALSE,
                                trim_ws = T, locale = custom_locale,
                                show_col_types = FALSE, progress = FALSE,
                                lazy = lazyreading) %>%
      dplyr::pull(2)

    opt_parameters <- colnames(opt) %>%
      stringr::str_remove_all(pattern = "Iteration|Rank|ConstraintViolation|Replications|Error") %>%
      .[. != ""]

    opt_objectives <- readr::read_delim(file = I(opt_info[[4]]),
                                        delim = delimiter,
                                        col_names = FALSE, progress = FALSE,
                                        trim_ws = T, locale = custom_locale,
                                        show_col_types = FALSE,
                                        lazy = lazyreading) %>%
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
  } else {
    assertthat::assert_that(!is.null(objectives),
                            msg = "No objective information detected, supply it manually to the function.")
    opt <- opt_info %>%
      paste(collapse = "\n")

    opt <- readr::read_delim(file = I(opt), trim_ws = TRUE,
                             delim = delimiter, show_col_types = FALSE,
                             locale = custom_locale,
                             progress = FALSE,
                             name_repair = ~janitor::make_clean_names(.,
                                                                      case = "none",
                                                                      sep_out = "_"),
                             lazy = lazyreading) %>%
      dplyr::select(-dplyr::any_of(c("Replications",
                                     "Error",
                                     "ConstraintViolation",
                                     "Iteration_2")),
                    maxOut = dplyr::starts_with("maxTP")) %>%
      janitor::remove_empty(which = "cols")

    opt_parameters <- colnames(opt)
    opt_objectives <- objectives
    opt_id <- "1"
  }

  if (is.null(outputs) & !is.null(inputs)) {
    outputs <- opt_parameters[!(opt_parameters %in% inputs)]
  }

  # Add Iteration if it doesn't exist, set Iteration in loaded order
  if (!("Iteration" %in% colnames(opt))) {
    opt <- opt %>%
      dplyr::mutate(Iteration = seq(1, nrow(.)),
                    .before = names(opt_objectives)[1])
  }

  # Add Rank if it doesn't exist, for all objectives
  if (!("Rank" %in% colnames(opt)) & !is.null(opt_objectives)) {
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
