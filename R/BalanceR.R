#' MakeBalanceAccounts
#'
#' Create the account structure required by Balance sheets according to
#' German HGB for so-called Kaptitalgesellschaften.
#' @export
MakeBalanceAccounts <- function(x, which = "small"){
    if (which == "verysmall"){
        x_split <- stringr::str_split(x, ":")
        x_hrchy <- sapply(x_split, function(xx){
            stringr::str_c(xx[1:max(which(stringr::str_detect(xx, "[A-G]+\\.\\s")))], collapse = ":")
        })
    } else if (which == "small"){
        x_split <- stringr::str_split(x, ":")
        x_hrchy <- sapply(x_split, function(xx){
            stringr::str_c(xx[1:max(which(stringr::str_detect(xx, "[A-GIV]+\\.\\s")))], collapse = ":")
        })
    } else {
        x_split <- stringr::str_split(x, ":")
        x_hrchy <- sapply(x_split, function(xx){
            stringr::str_c(xx[1:max(which(stringr::str_detect(xx, "[A-GIV1-9]+\\.\\s")))], collapse = ":")
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
                               stringr::str_c("\\myItem",c("", "One", "Two", "Three")[depth],
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

#' CreateTable
#'
#' Create a table from a yournal according to a given scheme, specified by via
#' the argument \code{which}.
#'
#' @param journal A data frame as returned from the \code{register} function
#' of the package \code{ledger}.
#'
#' @param which Character of length one, indicating up to which hierarchy the balance
#' sheet has to be created. "verysmall" for 'Kleinstkapitalgesellschaften', "small" for
#' 'Kleinkapitalgesellschaften' and "normal" for all other sizes. This will affect, how
#' detailed the resulting balance sheet will be.
#'
#' @return A list with two data frames with two columns, first column
#' contains the aggregated account name according to the scheme and
#' the second column contains the corresponding value.
#' The first entry is the "Aktiv", the second the "Passiv" part of the balance.
#'
#' @export
CreateTable <- function(journal, which){

    agg <- journal %>%
        filter(stringr::str_detect(account, "Aktiv") | stringr::str_detect(account, "Passiv")) %>%
        mutate(balance_var_small = MakeBalanceAccounts(account, which = which)) %>%
        group_by(balance_var_small) %>%
        summarize(amount = sum(amount))

    ## create active and passiv side of balance seperately
    akt <- agg %>%
        filter(stringr::str_detect(balance_var_small, "Aktiv")) %>%
        rename(account = balance_var_small) %>%
        AggStructure() %>%
        filter(real_name != "Aktiva") %>%
      mutate(amount = if_else(abs(amount) < 0.0001, 0 , amount),
             amount = na_if(amount, 0))
    pas <- agg %>%
        filter(stringr::str_detect(balance_var_small, "Passiv")) %>%
        rename(account = balance_var_small) %>%
        AggStructure() %>%
        filter(real_name != "Passiva") %>%
      mutate(amount = -amount,
             amount = if_else(abs(amount) < 0.0001, 0 , amount),
             amount = na_if(amount, 0))

    ## create tex tables
    ## if (nrow(akt) > nrow(pas)){
    ##     pas <- rbind(pas, data.frame(real_name = rep("", times = nrow(akt) - nrow(pas)),
    ##                                  amount = rep(NA, times = nrow(akt) - nrow(pas))))
    ## } else if (nrow(akt) < nrow(pas)) {
    ##     akt <- rbind(akt, data.frame(real_name = rep("", times = nrow(pas) - nrow(akt)),
    ##                                  amount = rep(NA, times = nrow(pas) - nrow(akt))))
    ## }

    names(akt) <- c("names_akt", "val_akt")
    names(pas) <- c("names_pas", "val_pas")

    ## bal <- cbind(akt, pas)
    bal <- list(
        akt = rbind(akt, data.frame(names_akt = "Summe Aktiva",
                            val_akt = sum((agg %>%
                                           filter(stringr::str_detect(balance_var_small,
                                                                      "Aktiva")))$amount
                                          ))),
        pas = rbind(pas, data.frame(names_pas = "Summe Passiva",
                            val_pas = -sum((agg %>%
                                            filter(stringr::str_detect(balance_var_small,
                                                                       "Passiva")))$amount
                                           ))))
    bal
}






#' CreateBalanceTable
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
CreateBalanceSheet <- function(journal, journal_lastyear = NULL, which = "small",
                               tablewidth = "\\textwidth", years_in_header = NULL,
                               colwidths = paste0(c(0.3,0.08),tablewidth),
                               file = NULL) {


    bal_list <- CreateTable(journal = journal, which = which)

    if (!is.null(journal_lastyear)){
        aktpas_lastyear <- CreateTable(journal = journal_lastyear, which = which) %>% {
            .$akt <- .$akt %>% rename(val_akt_lastyear = val_akt)
            .$pas <- .$pas %>% rename(val_pas_lastyear = val_pas)
            .
        }
        ## we assume that lastyear's accounts always is a subset of current yeras accounts
        akt <- left_join(bal_list$akt, aktpas_lastyear$akt, by="names_akt")
        pas <- left_join(bal_list$pas, aktpas_lastyear$pas, by="names_pas")

        bal_list <- list(akt = akt, pas = pas)
    }




    if (is.null(file)){

        ## make both entries have same number of rows
        if (nrow(bal_list$akt) > nrow(bal_list$pas)){
            pas <- rbind(bal_list$pas,
                         data.frame(real_name = rep("", times = nrow(bal_list$akt) -
                                                            nrow(bal_list$pas)),
                                    amount = rep(NA, times = nrow(bal_list$akt) -
                                                         nrow(bal_list$pas))))
        } else if (nrow(bal_list$akt) < nrow(bal_list$pas)) {
            akt <- rbind(bal_list$akt,
                         data.frame(real_name = rep("", times = nrow(bal_list$pas) -
                                                            nrow(bal_list$akt)),
                                    amount = rep(NA, times = nrow(bal_list$pas) -
                                                         nrow(bal_list$akt))))
        }

        bal <- cbind(akt, pas)

        xtable::xtable(bal, align = c("l", paste0("L{", colwidths[1], "}"),
                                      paste0("R{", colwidths[2], "}"),
                                      "|",
                                      paste0("L{", colwidths[1], "}"),
                                      paste0("R{", colwidths[2], "}")))
    } else {

        left_side <- if (is.null(journal_lastyear)){
                         c(paste0("\\begin{tabular}[t]{L{0.75\\textwidth}R{0.15\\textwidth}}"),
                           if(!is.null(years_in_header)) {
                               paste0("&",years_in_header[1],"\\\\\\midrule")
                           } else NULL,
                           bal_list$akt %>%
                           select(names_akt, val_akt) %>% {
                               vapply(1:(nrow(.)-1), function(x){
                                   paste0(.[x,1], "&", .[x,2], "\\\\")
                               }, character(1))
                           },
                           "\\end{tabular}")
                     } else {
                         c(paste0("\\begin{tabular}[t]{L{0.58\\textwidth}R{0.15\\textwidth}R{0.15\\textwidth}}"),
                           if(!is.null(years_in_header)) {
                               paste0("&",years_in_header[1],"&", years_in_header[2],"\\\\\\midrule")
                           } else NULL,
                           bal_list$akt %>%
                           select(names_akt, val_akt, val_akt_lastyear) %>% {
                               vapply(1:(nrow(.)-1), function(x){
                                   paste0(.[x,1], "&", .[x,2], "&", .[x,3], "\\\\")
                               }, character(1))
                           },
                           "\\end{tabular}")
                     }

        left_side_lastrow <- if (is.null(journal_lastyear)){
                         c(paste0("\\begin{tabular}[t]{L{0.75\\textwidth}R{0.15\\textwidth}}"),
                           bal_list$akt %>%
                           select(names_akt, val_akt) %>% {
                               paste0("\\textbf{",
                                      .[nrow(.),1], "}&\\textbf{",
                                      .[nrow(.),2], "}")
                           },
                           "\\end{tabular}")
                     } else {
                         c(paste0("\\begin{tabular}[t]{L{0.58\\textwidth}R{0.15\\textwidth}R{0.15\\textwidth}}"),
                           bal_list$akt %>%
                           select(names_akt, val_akt, val_akt_lastyear) %>% {
                               paste0("\\textbf{",
                                      .[nrow(.),1], "}&\\textbf{",
                                      .[nrow(.),2], "}&\\textbf{",
                                      .[nrow(.),3], "}")
                           },
                           "\\end{tabular}")
                     }


        right_side <- if(is.null(journal_lastyear)){
                          c(paste0("\\begin{tabular}[t]{L{0.75\\textwidth}R{0.15\\textwidth}}"),
                            if(!is.null(years_in_header)) {
                                paste0("&",years_in_header[1],"\\\\\\midrule")
                            } else NULL,
                            bal_list$pas %>%
                            select(names_pas, val_pas) %>% {
                                  vapply(1:(nrow(.)-1), function(x){
                                      paste0(.[x,1], "&", .[x,2], "\\\\")
                                  }, character(1))
                              },
                          "\\end{tabular}")
                      } else {
                          c(paste0("\\begin{tabular}[t]{L{0.58\\textwidth}R{0.15\\textwidth}R{0.15\\textwidth}}"),
                            if(!is.null(years_in_header)) {
                                paste0("&",years_in_header[1],"&", years_in_header[2],"\\\\\\midrule")
                            } else NULL,
                            bal_list$pas %>%
                            select(names_pas, val_pas, val_pas_lastyear) %>% {
                                  vapply(1:(nrow(.)-1), function(x){
                                      paste0(.[x,1], "&", .[x,2], "&", .[x,3], "\\\\")
                                  }, character(1))
                              },
                          "\\end{tabular}")
                      }
        right_side_lastrow <- if(is.null(journal_lastyear)){
                          c(paste0("\\begin{tabular}[t]{L{0.75\\textwidth}R{0.15\\textwidth}}"),
                            bal_list$pas %>%
                            select(names_pas, val_pas) %>% {
                                paste0("\\textbf{",
                                       .[nrow(.),1], "}&\\textbf{",
                                       .[nrow(.),2], "}")
                              },
                          "\\end{tabular}")
                      } else {
                          c(paste0("\\begin{tabular}[t]{L{0.58\\textwidth}R{0.15\\textwidth}R{0.15\\textwidth}}"),
                            bal_list$pas %>%
                            select(names_pas, val_pas, val_pas_lastyear) %>% {
                                paste0("\\textbf{",
                                       .[nrow(.),1], "}&\\textbf{",
                                       .[nrow(.),2], "}&\\textbf{",
                                       .[nrow(.),3], "}")
                              },
                          "\\end{tabular}")
                      }


        ## write the tex string into the file
        c("\\let\\myblength\\relax",
          "\\let\\mbyl\\relax",
          "\\ifdefined\\BalItem",
          "\\else",
          "\\newcolumntype{L}[1]{>{\\raggedright\\arraybackslash}p{#1}}",
          "\\newcolumntype{C}[1]{>{\\centering\\arraybackslash}p{#1}}",
          "\\newcolumntype{R}[1]{>{\\raggedleft\\arraybackslash}p{#1}}",
          "\\newlength{\\myblength}",
          "\\newlength{\\mbyl}",
          "\\def\\BalItem#1#2#3{\\setlength{\\myblength}{\\linewidth}\\settowidth{\\mbyl}{#3}\\addtolength{\\myblength}{-1.5\\mbyl}\\parbox{\\mbyl}{#1}\\parbox[t]{\\myblength}{#2}}",
          "\\def\\BalItemOne#1#2{\\BalItem{#1}{#2}{A.-}}",
          "\\def\\BalItemTwo#1#2{\\BalItem{\\phantom{---}#1}{#2}{---III.-}}",
          "\\def\\BalItemThree#1#2{\\BalItem{\\phantom{------}#1}{#2}{------8.-}}",
          "\\fi",
          "\\resizebox{\\textwidth}{!}{",
          "\\begin{tabular}{l|r}",
          "\\toprule",
          paste0("\\begin{minipage}[t]{10cm}"),
          left_side,
          "\\end{minipage}",
          "&",
          paste0("\\begin{minipage}[t]{10cm}"),
          right_side,
          "\\end{minipage}\\\\",
          "\\midrule",
          paste0("\\begin{minipage}[t]{10cm}"),
          left_side_lastrow,
          "\\end{minipage}",
          "&",
          paste0("\\begin{minipage}[t]{10cm}"),
          right_side_lastrow,
          "\\end{minipage}\\\\",
          "\\bottomrule\\end{tabular}}") %>%
            stringr::str_replace_all("NA", "") %>%
            stringr::str_replace_all("myItem", "BalItem") %>%
            writeLines(con = file)
    }
}
