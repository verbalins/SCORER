#' Flexible Pattern Mining FPM
#'
#' `fpm()` returns rules in the decision space to reach certain areas in the
#' objectives space.
#'
#' Flexible pattern mining \insertCite{Bandaru2017b}{SCORER} implemented in R().
#'
#' @param .data Data input, obtained using [SCORER::loaddataset()]
#' @param selected_data The selected solutions, either a vector of iterations or a named list of objectives and their values
#' @param unselected_data The rest of the solutions not in [selected_data]
#' @param max_level The maximum level of rules to be returned, default 1
#' @param min_sig Minimum significance a rule will have to meet to be included
#' @param use_equality Include rules such as x == 1?
#' @param only_most_significant Only return the most significant rules?
#' @return A list of lists where each combination of itemsets are considered
#' @export
#' @importFrom magrittr %>%
#' @importFrom Rdpack reprompt
#' @references{
#'  \insertAllCited{}
#' }
fpm <- function(.data,
                selected_data,
                unselected_data = NULL,
                max_level = 1,
                min_sig = 0.5,
                use_equality = TRUE,
                only_most_significant = TRUE) {
  # Determine if selected_data is just iterations or a reference point
  if (!is.numeric(selected_data)) {
    # If the provided data is a reference point, utilize the k-nearest solutions.
    # 20% of the closest solutions on the pareto-optimal front
    test <- .data %>% dplyr::filter(dplyr::across("Rank") == 1)
    selected_data <-
      test %>%
      dplyr::mutate(
        Distance = sqrt(rowSums(
          (test %>% dplyr::select(names(selected_data)) - selected_data) ** 2)
          )) %>%
      dplyr::distinct(across(names(selected_data)), .keep_all = TRUE) %>%
      dplyr::arrange(Distance) %>%
      head(n = 0.2 * nrow(.)) %>%
      dplyr::pull(Iteration)
  }
  # We filter on the inputs of the data
  inputs <- .data %>%
    dplyr::select(Iteration, .$inputs)
  #truth_table <- tibble::tibble(.rows=nrow(inputs))
  all_rules <- list()
  comb <- NULL
  for (i in seq(1, max_level)) { # Create each level of rules
    if (i > 1) {
      first_rules <- all_rules[[1]]$Rule[1:min(length(all_rules[[1]]), 15)]
      comb <- gtools::combinations(length(first_rules),
                                   i,
                                   first_rules,
                                   repeats.allowed = FALSE)
    }
    truth_table <- create_truth_table(inputs, use_equality, comb)
    all_rules[[i]] <- create_rules(truth_table,
                                   i,
                                   min_sig,
                                   selected_data = selected_data)
  }

  return(do.call(rbind, lapply(all_rules, head, 15)) %>%
           dplyr::arrange(dplyr::desc(Ratio)))
}

# Create the rules
create_rules <- function(data, level, min_sig, selected_data) {
  # Create rules
  # Make sure to delete rules with equal values, keep the one with ==
  tib <- tibble::tibble(Rule = data %>%
                          dplyr::select(-Iteration) %>%
                          colnames(),
                        Sign = dplyr::if_else(grepl("==", Rule),
                                              "A",
                                              "B"), # Give priority to equal
                        Sel = data %>%
                          dplyr::filter(Iteration %in% selected_data) %>%
                          dplyr::select(-Iteration) %>%
                          colSums(),
                        Unsel = data %>%
                          dplyr::filter(!(Iteration %in% selected_data)) %>%
                          dplyr::select(-Iteration) %>%
                          colSums(),
                        Significance = Sel / length(selected_data),
                        Unsignificance = Unsel / (nrow(data) - length(selected_data)),
                        Ratio = dplyr::if_else(Significance >= min_sig,
                                               dplyr::if_else(is.infinite(Sel / Unsel),
                                                              0.0, Sel / Unsel),
                                               0.0)) %>%
    dplyr::filter(Ratio > 0)

  if (level == 1) {
    tib <- tib %>%
      dplyr::arrange(desc(Ratio), Sign) %>%
      dplyr::select(-Sign) %>%
      dplyr::distinct(Significance, Ratio, .keep_all = TRUE)
  } else {
    tib <- tib %>%
      dplyr::select(-Sign) %>%
      dplyr::arrange(desc(Ratio))
  }
  # Keep only the most informative rule for each variable
  tib <- tib %>%
    tidyr::separate(Rule,
                    sep = " ",
                    into = c("Var", "Sign", "Val"),
                    remove = FALSE) %>%
    dplyr::group_by(Var) %>%
    dplyr::slice_max(order_by = Ratio, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(Ratio))
}

# Creating the truth table for this rule level
#' @importFrom rlang :=
#' @importFrom foreach %dopar%
create_truth_table <- function(.data, use_equality, rules = NULL) {
  col_name <- .data$inputs
  if (is.null(rules)) {
    params <- c(" < ", " == ", " > ")
    if (!use_equality) {
      params <- c(" < ", " > ")
    }
    rules <- unlist(lapply(col_name[seq_along(col_name)], create_first_rules, .data, params))
  } else {
    rules <- apply(rules[seq_len(nrow(rules)), ], 1, \(x) { paste0(x, collapse = " & ")})
  }

  cl <- parallel::makeCluster(parallel::detectCores(TRUE) - 1)
  doParallel::registerDoParallel(cl)

  newdata <- foreach::foreach(rule = seq_along(rules), .combine = cbind, .packages = c("dplyr")) %dopar% {
    .data %>% dplyr::mutate(!!(rules[rule]) := dplyr::if_else(eval(rlang::parse_expr(as.character(rules[rule]))),
                                                              1,
                                                              0),
                            .keep = "none")
  }

  doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)

  .data %>% dplyr::select(Iteration) %>% cbind(newdata)
}

# The initially created rules
create_first_rules <- function(col_name, data, params) {
  col_rule <- paste0(col_name,
                     sapply(sort(unique(ceiling(data[[col_name]] * 1000) / 1000)),
                            \(x) paste0(params, x)))
  col_rule <- col_rule[2:length(col_rule)] # Remove first
  col_rule[seq_along(col_rule) - 1] # Remove last element
}
