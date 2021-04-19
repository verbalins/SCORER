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
fpm <- function(.data, maxLevel = 1, minSig = 0.5, selectedData, unselectedData=NULL, useEquality = TRUE, onlyMostSignificant = TRUE) {
  # We filter on the inputs of the data
  inputs <- .data %>% dplyr::select(Iteration,.$inputs)
  #truth_table <- tibble::tibble(.rows=nrow(inputs))
  all_rules <- list()
  comb <- NULL
  for (i in seq(1,maxLevel)) { # Create each level of rules
    if (i > 1) {
      first_rules <- all_rules[[1]]$Rule[1:min(length(all_rules[[1]]),15)]
      comb <- gtools::combinations(length(first_rules), i, first_rules, repeats.allowed = FALSE)
    }
    truth_table <- create_truth_table(inputs, useEquality, comb)
    all_rules[[i]] <- create_rules(truth_table, i, minSig, selectedData = selectedData)
  }

  return(do.call(rbind, lapply(all_rules, head, 15)) %>% dplyr::arrange(desc(Ratio)))
}

create_rules <- function(data, level, minSig, selectedData) {
  # Create rules
  # Make sure to delete rules with equal values, keep the one with ==
  tib <- tibble::tibble(Rule = data %>% dplyr::select(-Iteration) %>% colnames(),
                        Sign = dplyr::if_else(grepl("==", Rule), "A", "B"), # Give priority to equal
                        Sel = data %>%
                          dplyr::filter(Iteration %in% selectedData) %>%
                          dplyr::select(-Iteration) %>% colSums(),
                        Unsel = data %>%
                          dplyr::filter(!(Iteration %in% selectedData)) %>%
                          dplyr::select(-Iteration) %>% colSums(),
                        Significance = Sel/length(selectedData),
                        Unsignificance = Unsel/(nrow(data)-length(selectedData)),
                        Ratio = dplyr::if_else(Significance >= minSig,
                                               dplyr::if_else(is.infinite(Sel/Unsel), 0.0, Sel/Unsel), 0.0)) %>%
    dplyr::filter(Ratio > 0)

  if (level == 1) {
    tib <- tib %>% dplyr::arrange(desc(Ratio), Sign) %>%
      dplyr::select(-Sign) %>%
      dplyr::distinct(Significance, Ratio, .keep_all = TRUE)
  } else {
    tib <- tib %>%
      dplyr::select(-Sign) %>%
      dplyr::arrange(desc(Ratio))
  }
  # Keep only the most informative rule for each variable
  tib <- tib %>%
    tidyr::separate(Rule, sep=" ", into = c("Var", "Sign", "Val"), remove=FALSE) %>%
    dplyr::group_by(Var) %>%
    dplyr::slice_max(order_by=Ratio, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(Ratio))
}

#' @importFrom rlang :=
#' @importFrom foreach %dopar%
create_truth_table <- function(.data, useEquality, rules = NULL) {
  col_name <- .data$inputs
  if (is.null(rules)) {
    params <- c(" < ", " == ", " > ")
    if (!useEquality){
      params <- c(" < ", " > ")
    }
    rules <- unlist(lapply(col_name[1:length(col_name)], create_first_rules, .data, params))
  } else {
    rules <- apply(rules[seq(1,nrow(rules)),], 1, function(x) { paste0(x, collapse=" & ")})
  }

  doParallel::registerDoParallel(parallel::detectCores()-2)
  #newdata <- tibble::new_tibble(nrow=nrow(.data))
  newdata <- foreach::foreach(rule = 1:length(rules), .combine=cbind) %dopar% {
    .data %>% dplyr::mutate(!!(rules[rule]) := dplyr::if_else(eval(rlang::parse_expr(as.character(rules[rule]))),1,0), .keep="none")
  }

  #for (rule in rules) {
  #  .data <- .data %>% dplyr::mutate(!!(rule) := dplyr::if_else(eval(rlang::parse_expr(as.character(rule))),1,0))
  #}
  doParallel::stopImplicitCluster()
  #.data <- cbind(.data, newdata)

  .data %>% dplyr::select(Iteration) %>% cbind(newdata)
}

create_first_rules <- function(col_name, data, params) {
  col_rule <- paste0(col_name, sapply(sort(unique(ceiling(data[[col_name]]*1000)/1000)), function(x) paste0(params, x)))
  col_rule <- col_rule[2:length(col_rule)] # Remove first
  col_rule[1:length(col_rule)-1] # Remove last element
}
