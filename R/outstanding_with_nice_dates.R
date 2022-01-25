#' Beregn utestående volum med fine dato-akser
#'
#' Funksjonen brukes i funksjonene for å plotte utestående volum for repo og FX-swap, \cr
#' med daglig datoakse. Dette er nyttig blant annet for å få fine plots når man lager bar- \cr
#' plots, men kan også være nyttig i andre sammenhenger. Funksjonen lar deg velge hvilken \cr
#' variabel du vil gruppere etter når man plotter utestående. Det vil si at man f.eks. kan \cr
#' velge om man vil plotte utestående for en bank fordelt på motpart, løpetid, type sikkerhet \cr
#' eller andre variabler ved å endre parameteren grouping_variable. Man kan også begrense hvor \cr
#' mange av disse som vises med å bruke grouping_restriction. Setter man denne lik f.eks. 5, \cr
#' vil man få tilbake utestående for f.eks. 5 største motparter på inn- og utlånssiden. Alle \cr
#' andre motparter vil samles opp som "Annen".
#'
#'
#' @param df dataframe. Datasett man vil bruke. Kan være fx-swap-datasett, repo-datasett eller annet.
#' @param settlementdate char. Navn på kolonne som gir oppgjørsdato.
#' @param maturitydate char. Navn på kolonne som inneholder forfallsdato
#' @param transaction_volume char. Navn på kolonne som inneholder transaksjonsvolum
#' @param grouping_variable char. Variabel man skal gruppere for. Eksempelvis "tenor"  \cr
#' for å få utestående fordelt på løpetid, eller "CounterPartyName" for fordeling på motpart.
#' @param grouping_restriction FALSE eller integer. Vil du begrense grupperingsvariabelen. \cr
#'  Nyttig hvis utestående f.eks. fordeles på veldig mange motparter.Velger man eksempelvis \cr
#'  grouping_variable = "CounterPartyName og grouping_restriction = 5, vises utestående for de \cr
#'  5 motpartene med størst gjennomsnittlig omsetning  på inn- og utlånssiden gjennom tidsperioden \cr
#'  man plotter for. Utestående for øvrige motparter vises som "annen".
#' @param return_raw logical. Vil du ha ut rådata for utestående (uten pen datoserie)
#'
#' @importFrom dplyr filter mutate left_join rename select if_else group_by summarise arrange bind_rows ungroup
#' @importFrom tidyr pivot_wider gather fill
#' @importFrom stringr str_replace str_replace_all str_trunc
#' @importFrom tibble tibble
#' @import magrittr
#'
#' @encoding UTF-8
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Med default-verdier:
#' df = outstanding_with_nice_dates(df)
#' Med spesifisert grupperingsvariabel og grupperingsrestriksjon.
#' Viser da omsetning fordelt på motpart for 5 motparter med mest
#' omsetning på inn- og utlånssiden
#' df = outstanding_with_nice_dates(df,
#' grouping_variable = "CounterPartyName",
#' grouping_restriction = 5)
#' }


outstanding_with_nice_dates <- function(df,
                                        settlementdate = "SttlmDt",
                                        maturitydate = "MtrtyDt",
                                        transaction_volume = "TxNmnlAmt",
                                        grouping_variable,
                                        grouping_restriction = FALSE,
                                        return_raw = FALSE) {


  options_list = list("transaction_volume" = transaction_volume,
                      "settlementdate" = settlementdate,
                      "maturitydate" = maturitydate,
                      "grouping_variable" = grouping_variable)



  df = df %>%rename(TxNmnlAmt = options_list[["transaction_volume"]],
                    SttlmDt = options_list[["settlementdate"]],
                    MtrtyDt = options_list[["maturitydate"]],
                    grouping = options_list[["grouping_variable"]])


  new = df %>% select(-MtrtyDt) %>%
    rename(date = SttlmDt, new = TxNmnlAmt) %>% mutate(matured = 0)

  matured = df %>% select(-SttlmDt) %>%
    rename(date = MtrtyDt, matured = TxNmnlAmt) %>% mutate(new = 0)


  outstanding = bind_rows(new, matured)
  #outstanding$matured[is.na(outstanding$matured)] = 0
  #outstanding$new[is.na(outstanding$new)] = 0

  outstanding_by_group = outstanding %>%
    arrange(date) %>%
    group_by(date, grouping, TxTp) %>%
    summarise(sum_new = sum(new), sum_matured = sum(matured)) %>%
    group_by(TxTp, grouping) %>%
    mutate(cumsum_new = cumsum(sum_new),
           cumsum_matured = cumsum(sum_matured),
           outstanding = (cumsum_new - cumsum_matured)/10^9) %>%
    ungroup()

  outstanding_by_group_raw = outstanding_by_group

  outstanding_by_group = outstanding_by_group %>%
    select(-c(sum_new, sum_matured, cumsum_new, cumsum_matured))

  datedf <- tibble(date = seq(as.Date(min(df$TradDt)), as.Date(max(df$TradDt)), by = 1))

  txtp_levels <- levels(as.factor(df$TxTp))

  t_list <- list()

  for (i in txtp_levels) {
    t = outstanding_by_group %>%
      filter(TxTp == i) %>%
      pivot_wider(names_from = grouping, values_from = outstanding)

    t = left_join(datedf, t , by = "date") %>%
      fill(2:ncol(t), .direction = "down") %>%
      mutate(TxTp = i)

    t[is.na(t)] <- 0
    t = t %>% gather(grouping, outstanding, -c(date, TxTp))

    t_list[[i]] <- t

  }

  outstanding_by_group <- bind_rows(t_list) %>% ungroup()

  if(grouping_restriction == FALSE) {
    outstanding_by_group = outstanding_by_group
  } else{

    topgroup_1 = outstanding_by_group %>%
      filter(TxTp == txtp_levels[1]) %>%
      group_by(grouping) %>%
      summarise(outstanding = mean(outstanding)) %>%
      arrange(-outstanding)


    topgroup_1 = topgroup_1$grouping[1:min(nrow(topgroup_1),grouping_restriction)]

    topgroup_2 = outstanding_by_group %>%
      filter(TxTp == txtp_levels[2]) %>%
      group_by(grouping) %>%
      summarise(outstanding = mean(outstanding)) %>%
      arrange(-outstanding)

    topgroup_2 = topgroup_2$grouping[1:min(nrow(topgroup_2),grouping_restriction)]

    topgroup = c(topgroup_1, topgroup_2)

    outstanding_by_group = outstanding_by_group %>%
      mutate(grouping = as.character(grouping),
             grouping = as.factor(if_else(grouping %in% topgroup, grouping, "Annen")))

  }


  #ooooobs: dette er ikke spesielt plassbesparende. verdt å tenke på hvis det blir veldig mye data noen gang
  fill_1 <- outstanding_by_group %>% filter(TxTp == txtp_levels[1]) %>%
    mutate(outstanding = 0, TxTp = txtp_levels[2])

  fill_2 <- outstanding_by_group %>% filter(TxTp == txtp_levels[2]) %>%
    mutate(outstanding = 0, TxTp = txtp_levels[1])

  outstanding_by_group <- bind_rows(list(outstanding_by_group,
                                         fill_1,
                                         fill_2)) %>% group_by(date, TxTp, grouping) %>%
    summarise(outstanding = sum(outstanding)) %>% ungroup()

  names(outstanding_by_group)[names(outstanding_by_group) == "grouping"] = options_list$grouping_variable
  names(outstanding_by_group_raw)[names(outstanding_by_group_raw) == "grouping"] = options_list$grouping_variable

  if(return_raw) {

    return(outstanding_by_group_raw)

  } else{

    return(outstanding_by_group)
  }



}
