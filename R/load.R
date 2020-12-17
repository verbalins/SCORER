#' Load optimization data
#'
#' @export
#' @param filename A csv file containing formatted optimization data from Opt
#' @param objectives A character vector naming the objectives
#' @param inputs A character vector naming the inputs
#' @param outputs A character vector naming the ouputs
#' @param custom_outputs A character vector describing custom outputs
#'
#' @return A dataframe with the data
loaddataset <- function(filename, objectives, inputs, outputs, custom_outputs) {
  opt_info <- readr::read_lines(filename)

  # TODO: Evaluate if parameters are at the bottom.
  # If so, extract them
  opt <- head(opt_info, -5) %>%
    paste(collapse = "\n") %>%
    readr::read_csv2() %>%
    dplyr::select(-Replications, -ConstraintViolation, maxOut = dplyr::starts_with("maxTP")) %>%
    dplyr::select(where(function(x){!all(is.na(x))}))

  opt_info <- utils::tail(opt_info, 5) %>%
    paste(collapse = "\n")

  opt_id <- opt_info %>%
    readr::read_csv2(n_max = 1, col_names = FALSE) %>%
    dplyr::pull(2)

  opt_parameters <- opt_info %>%
    readr::read_csv2(skip = 2, n_max = 1, col_names = FALSE) %>%
    dplyr::select(!!-1, -((length(.)-1):length(.))) %>%
    dplyr::slice() %>%
    unlist(., use.names = FALSE)

  #opt_textoutput <- opt_info %>%
  #  readr::read_csv2(skip = 3, n_max = 1, col_names = FALSE)

  opt_objectives <- opt_info %>%
    readr::read_csv2(skip = 4, n_max = 1, col_names = FALSE) %>%
    dplyr::select_if(grepl("Min|Max", .)) %>%
    grepl("Max", .)

  names(opt_objectives) <- utils::head(stringr::str_replace(opt_parameters, "maxTP", "maxOut"), length(opt_objectives))

  opt_parameters <- utils::tail(opt_parameters, -length(opt_objectives))

  # TODO: Change this to enable importing of custom optimization examples
  if (length(inputs)==0) {
    # Try general SCORE naming
    if (grep("^imp_", opt_parameters)) {
      inputs <- grep("^imp_", opt_parameters, value = TRUE)
    } else {
      # Set inputs to everything other than outputs and objectives

    }
  }

  outputs <- opt_parameters[!(opt_parameters %in% inputs)]

  opt_results <- optresult(data = opt,
                           opt_name = tools::file_path_sans_ext(basename(filename)),
                           id = opt_id,
                           objectives = opt_objectives,
                           inputs = inputs,
                           outputs = outputs)
  return(opt_results)
}

