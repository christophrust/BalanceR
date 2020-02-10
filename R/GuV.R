#' AggStructureGuV
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
AggStructureGuV <- function(x){
    maxdepth <- vapply(stringr::str_split(x$account, ":"), length, 0) %>%
        max()

    lapply(1:maxdepth, function(d){
        x %>%
            mutate(
                sort_name = (function(acc, depth){
                    vapply(stringr::str_split(acc, ":"),
                           function(x) {
                               str_c(x[1:min(depth, length(x))], collapse = ":")
                           },
                           character(1))
                })(account, d),
                real_name = (function(acc, depth){
                    vapply(stringr::str_split(acc, ":"),
                           function(x) {
                               itemContent <- stringr::str_split(x[depth], "\\. ")[[1]]
                               stringr::str_c("\\myItem",c("One", "Two", "Three")[depth-1],
                                              "{",itemContent[1], ".}",
                                              "{",itemContent[2], "}"
                                              )
                               ## stringr::str_c(stringr::str_c(rep("\\phantom{---}",
                               ##                                  times = max(0,depth-2)),
                               ##            collapse = ""), x[depth])
                           },
                           character(1))
                })(account, d)) %>%
            tidyr::drop_na() %>%
            group_by(sort_name, real_name) %>%
            summarise(amount = sum(amount)) %>%
            ungroup()
    }) %>%
        do.call(rbind, .) %>%
        arrange(sort_name) %>%
        select(real_name, amount)
}

#' MakeGuVAccounts
#'
#' Create the account structure required by Balance sheets according to
#' German HGB for so-called Kaptitalgesellschaften.
#' @export
MakeGuVAccounts <- function(x){
    x_split <- stringr::str_split(x, ":")
    x_hrchy <- sapply(x_split, function(xx){
        stringr::str_c(xx[1:max(which(stringr::str_detect(xx, "[0-9]+\\.\\s")))], collapse = ":")
    })
    x_hrchy
}


#' CreateGuVSheet
#'
#' Create a balance table as tex from a ledger journal imported using the
#' register function of the R package \code{ledger}.
#'
#' @param journal A data frame as returned from the \code{register} function
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
CreateGuVSheet <- function(journal, tablewidth = "\\textwidth",
                               colwidths = paste0(c(0.8,0.1),tablewidth)){
    agg <- journal %>%
        filter(stringr::str_detect(account, "Ertrag") | stringr::str_detect(account, "Aufwand")) %>%
        mutate(balance_var_small = MakeGuVAccounts(account)) %>%
        group_by(balance_var_small) %>%
        summarize(amount = sum(amount))

    ## create expenses and income seperately
    expens <- agg %>%
        filter(stringr::str_detect(balance_var_small, "Aufwand:")) %>%
        rename(account = balance_var_small) %>%
        AggStructureGuV() %>%
        filter(real_name != "Aufwand") %>%
        mutate(pos_nr = as.numeric(stringr::str_extract(real_name, "[0-9]+\\.")))
    income <- agg %>%
        filter(stringr::str_detect(balance_var_small, "Ertrag:")) %>%
        rename(account = balance_var_small) %>%
        AggStructureGuV() %>%
        filter(real_name != "Ertrag") %>%
        mutate(amount = -amount)  %>%
        mutate(pos_nr = as.numeric(stringr::str_extract(real_name, "[0-9]+\\.")))


    guv <- rbind(expens, income) %>%
        arrange(pos_nr)
    ## compute missing entries
    BrErgUms <- data.frame(real_name = "\\myItemOne{3.}{Bruttoergebnis vom Umsatz}",
                           amount = guv$amount[1] - guv$amount[2], pos_nr = 3)
    ErNaSteu <- data.frame(real_name = "\\myItemOne{14.}{Ergebnis nach Steuern}",
                           amount = guv$amount %*% c(1,-1,-1,-1,1,-1,1,1,1,-1,-1,-1,0),
                           pos_nr = 14)
    JaUebFehl <- data.frame(real_name = '\\myItemOne{16.}{Jahres\\"uberschuss/Jahresfehlbetrag}',
                            amount = ErNaSteu$amount - guv$amount[13],
                            pos_nr = 16)
    guv <- rbind(guv, BrErgUms, ErNaSteu, JaUebFehl) %>%
        arrange(pos_nr) %>%
        select(real_name, amount) %>%
        mutate(amount = if_else(amount == 0, 0, amount))

    xtable::xtable(guv, align = c("l", paste0("L{", colwidths[1], "}"),
                                  paste0("R{", colwidths[2], "}")))
}
