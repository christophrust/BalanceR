#' MakeBalanceAccounts
#'
#' Create the account structure required by Balance sheets according to
#' German HGB for so-called Kaptitalgesellschaften.
#' @export
MakeBalanceAccounts <- function(x, which = "small"){
    if (which == "verysmall"){
        ## tbd
    } else if (which == "small"){
        x_split <- str_split(x, ":")
        x_hrchy <- sapply(x_split, function(xx){
            str_c(xx[1:max(which(str_detect(xx, "[A-GIV1-9]*\\.\\s")))], collapse = ":")
        })
    }
    x_hrchy
}


#' AggStructure
#'
#' Create the aggregate structure from already aggregated balance sheet
#'
#' @param x aggregated accounts as a data frame with columns
#'    "account", "amount". Account hierarchy must be seperated by a ":"
#'    in the account names according to ledger convention
#'
#' @return A data frame containing one side of the balance sheet
#'    already in the correct order.
#'
#' @details function only in beta state...
#' @export
AggStructure <- function(x){
    maxdepth <- vapply(str_split(x$account, ":"), length, 0) %>%
        max()

    lapply(1:maxdepth, function(d){
        x %>%
            mutate(
                sort_name = (function(acc, depth){
                    vapply(str_split(acc, ":"),
                           function(x) {
                               str_c(x[1:min(depth, length(x))], collapse = ":")
                           },
                           character(1))
                })(account, d),
                real_name = (function(acc, depth){
                    vapply(str_split(acc, ":"),
                           function(x) {
                               str_c(str_c(rep("\\phantom{---}", times = max(0,depth-2)),
                                           collapse = ""), x[depth])
                           },
                           character(1))
                })(account, d)) %>%
            drop_na() %>%
            group_by(sort_name, real_name) %>%
            summarise(amount = sum(amount)) %>%
            ungroup()
    }) %>%
        do.call(rbind, .) %>%
        arrange(sort_name) %>%
        select(real_name, amount)
}

#' CreateBalanceTable
#'
#' Create a balance table as tex from a ledger journal imported using the
#' register function of the R package \code{ledger}.
#'
#' @param journal The a data frame as returned from the \code{register} function
#' of the package \code{ledger}.
#' 
#' @param which Character of length one, indicating up to which hierarchy the balance
#' sheet has to be created. "verysmall" for 'Kleinstkapitalgesellschaften', "small" for
#' 'Kleinkapitalgesellschaften' and "normal" for all other sizes. This will affect, how
#' detailed the resulting balance sheet will be.
#'
#' @details The function CreateBalanceTable creates a TeX table from a ledger journal
#' including all the accounts required for reports according to the German HGB. Currently,
#' the functions result should only be used in landscape mode (for instance by including
#' the output in a rotatebox environment). Several other things are hard coded as well
#' and will be made available as an argument in the future.
#'
#' @return An xtable object containing the balance sheet.
#'
#' @author Christoph Rust
#'
#' @export
CreateBalanceSheet <- function(journal, which = "small"){
    agg <- journal %>%
        filter(str_detect(account, "Aktiv") | str_detect(account, "Passiv")) %>%
        mutate(balance_var_small = MakeBalanceAccounts(account, which = which)) %>%
        group_by(balance_var_small) %>%
        summarize(amount = sum(amount))

    ## create active and passiv side of balance seperately
    akt <- agg %>%
        filter(str_detect(balance_var_small, "Aktiv")) %>%
        rename(account = balance_var_small) %>%
        AggStructure() %>%
        filter(real_name != "Aktiva")
    pas <- agg %>%
        filter(str_detect(balance_var_small, "Passiv")) %>%
        rename(account = balance_var_small) %>%
        AggStructure() %>%
        filter(real_name != "Passiva") %>%
        mutate(amount = -amount)

    ## create tex tables
    if (nrow(akt) > nrow(pas)){
        pas <- rbind(pas, data.frame(real_name = rep("", times = nrow(akt) - nrow(pas)),
                                     amount = rep(NA, times = nrow(akt) - nrow(pas))))
    } else if (nrow(akt) < nrow(pas)) {
        akt <- rbind(akt, data.frame(real_name = rep("", times = nrow(pas) - nrow(akt)),
                                     amount = rep(NA, times = nrow(pas) - nrow(akt))))
    }
    names(akt) <- c("names_akt", "val_akt")
    names(pas) <- c("names_pas", "val_pas")
    bal <- cbind(akt, pas)
    bal <- rbind(bal,
                 data.frame(names_akt = "Summe Aktiva",
                            val_akt = sum((agg %>%
                                           filter(str_detect(balance_var_small,
                                                             "Aktiva")))$amount
                                          ),
                            names_pas = "Summe Passiva",
                            val_pas = -sum((agg %>%
                                            filter(str_detect(balance_var_small,
                                                              "Passiva")))$amount
                                           )))
    xtable(bal, align = c("l","L{0.38\\textheight}", "R{0.1\\textheight}",
                          "|","L{0.38\\textheight}", "R{0.1\\textheight}"))
}
