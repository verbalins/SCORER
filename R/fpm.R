# Functions to implement Flexible Pattern Mining
# Defined in:
#     Bandaru, S., Ng, A. H. C., & Deb, K. (2016). Data Mining Methods for Knowledge Discovery in Multi-Objective Optimization: Part B-New Developments and Applications.
#     Expert Systems With Applications, 70, 119–138. https://doi.org/10.1016/j.eswa.2016.10.016
# Implementation:
#

#' Flexible Pattern Mining FPM
#'
#' Flexible pattern mining in R(), returns rules to reach certain areas in the objectives space
#'
#' @param .data Data input, obtained using [SCORER::loaddataset()]
#'
#' @return A list of lists where each combination of itemsets are considered
#' @export
fpm <- function(.data, maxLevel = 1, minSig = 0.5, selectedData=NULL, unselectedData=NULL, useEquality = TRUE, onlyMostSignificant = TRUE) {
  # We filter on the inputs of the data
  inputs <- .data %>% dplyr::select(.$inputs)
  truth_table <- tibble::tibble(.rows=nrow(inputs))
  # For each discrete column (input) x_i, the row (value) x_ij, is compared to the
  # discrete value range of that column d_i1..n
  #for (dim_name in names(inputs)) {

  #}
  # A truth table is created where each column will contain a truth vector for
  # that decision variable
  #return(truth_table)

  # Convert this to a transactional data frame and analyze with arules::apriori
  #truth_table <- truth_table %>% dplyr::mutate(dplyr::across(dplyr::everything(), function(x) dplyr::if_else(x==1.0, TRUE, NA)))
  #rules <- arules::apriori(truth_table, parameter = list(support = 0.5, confidence=0.8))
  #rules_df <-  arules::DATAFRAME(rules, separate = T, setStart="", setEnd ="")

  # Evaluate rules by filtering the dataset
  #eval(rlang::parse_expr(as.character(rules_df[1,"RHS"])))
  all_rules <- list()
  comb <- NULL
  for (i in seq(1,maxLevel)) {
    if (i > 1) {
      first_rules <- all_rules[[1]]$Rule[1:10]
      comb <- gtools::combinations(length(first_rules), i, first_rules, repeats.allowed = FALSE)
    }
    truth_table <- create_truth_table(inputs, useEquality, comb)
    all_rules[[i]] <- create_rules(truth_table, i, minSig, selectedData = selectedData)
  }

  #all_rules[[1]] <- lapply(all_rules[[2:length(all_rules)]], head(10))
  return(do.call(rbind, lapply(all_rules, head, 10)) %>% dplyr::arrange(desc(Ratio)))
}

create_rules <- function(data, level, minSig, selectedData) {
  # Create rules
  # Make sure to delete rules with equal values, keep the one with ==
  tib <- tibble::tibble(Rule = colnames(data),
                        Sign = dplyr::if_else(grepl("==", Rule), "A", "B"), # Give priority to equal
                        Sel = data %>%
                          dplyr::filter(row.names(.) %in% selectedData) %>% colSums(),
                        Unsel = data %>%
                          dplyr::filter(!(row.names(.) %in% selectedData)) %>% colSums(),
                        Significance = Sel/length(selectedData),
                        Unsignificance = Unsel/(nrow(data)-length(selectedData)),
                        Ratio = dplyr::if_else(Significance >= minSig,
                                               (Significance*100)/(Unsignificance*100), 0.0)) %>%
    dplyr::filter(Ratio > 0)

  if (level == 1) {
    tib <- tib %>% dplyr::arrange(desc(Ratio), Sign) %>%
      dplyr::select(-Sign) %>%
      dplyr::distinct(Ratio, .keep_all = TRUE)
  } else {
    tib <- tib %>%
      dplyr::select(-Sign) %>%
      dplyr::arrange(desc(Ratio))
  }
}

#' @importFrom rlang :=
create_truth_table <- function(.data, useEquality, rules = NULL) {
  col_name <- names(.data)
  if (is.null(rules)) {
    params <- c(" < ", " == ", " > ")
    if (!useEquality){
      params <- c(" < ", " > ")
    }
    rules <- unlist(lapply(col_name[1:length(col_name)], create_first_rules, .data, params))
  } else {
    rules <- apply(rules[seq(1,nrow(rules)),], 1, function(x) { paste0(x, collapse=" & ")})
  }

  for (rule in rules) {
    .data <- .data %>% dplyr::mutate(!!(rule) := dplyr::if_else(eval(rlang::parse_expr(as.character(rule))),1,0))
  }

  .data %>% dplyr::select(-(1:length(col_name)))
}

create_first_rules <- function(col_name, data, params) {
  col_rule <- paste0(col_name, sapply(sort(unique(data[[col_name]])), function(x) paste0(params, x)))
  col_rule <- col_rule[2:length(col_rule)] # Remove first
  col_rule[1:length(col_rule)-1] # Remove last element
}

n_rule_level <- function(.data, n) {
  # Here we get the truth table, and n-level rule combinations
  # Create a new truth table with the n-level support
}
