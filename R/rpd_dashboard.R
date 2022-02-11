
#' Dashboard for RPD-data
#'
#' @param load_source_data logical. Skal man laste inn data til applikasjonen eller ikke.
#' Dette kan ta litt tid, avhengig av hastighet på tilkobling etc.
#' Alternativ kan man laste ned datasett på forhånd, og kjøre applikasjon
#' med 'load_source_data = F'. Forhåndslastede datasett må da ha navnene
#' fx, repos og depos for hhv fx-swap, repo og usikrede transaksjoner."
#'
#' @importFrom dplyr filter mutate left_join rename select if_else group_by summarise arrange bind_rows ungroup
#' @importFrom stringr str_replace str_replace_all str_trunc
#' @import magrittr
#' @importFrom shinydashboard dashboardSidebar dashboardHeader dashboardBody menuItem tabItem box tabBox tabItems dashboardPage sidebarMenu
#' @importFrom shiny fluidRow tabPanel dateRangeInput checkboxInput conditionalPanel reactive observeEvent downloadHandler shinyApp runApp downloadButton numericInput HTML textInput h1 br
#' @importFrom shinyWidgets pickerInput updatePickerInput pickerOptions
#' @importFrom htmlwidgets saveWidget
#' @importFrom plotly plotlyOutput renderPlotly as_widget
#' @importFrom shinyTime timeInput
#' @importFrom openxlsx write.xlsx
#' @importFrom tidyselect all_of
#' @importFrom shinyjs useShinyjs
#' @export
#'
#' @examples
#'
#' #Start dashboard og last inn data
#' \dontrun{
#' rpd_dashboard()
#' }
#'
#'
#'
rpd_dashboard <- function(load_source_data = TRUE) {

  if(load_source_data) {
    cat("Laster inn data, vennligst vent.
        \nDette kan ta litt tid, avhengig av hastighet på tilkobling etc.
        \nAlternativ kan man laste ned datasett på forhånd, og kjøre applikasjon
        \nmed 'load_source_data = F'. Forhåndslastede datasett må da ha navnene
        \nfx, repos og depos for hhv fx-swap, repo og usikrede transaksjoner.")
    #Hent FX-data
    fx = fx_swap()
    #Hent repo-data, og rediger Hrcuts fra danske og nordea.
    repos = repo() %>%
    mutate(Hrcut = if_else(ReportingAgentName %in% c("Danske Bank", "Nordea Bank Abp") & Hrcut < -.5,
                           -Hrcut, Hrcut))

    #Hent data for usikrede transaksjoner
    depos = deposits()

    cat("\n\nData lastet inn, starter applikasjon.\n\n")


  } else{
    #Sjekk om datasett finnes.
    check = sapply(c("repos", "fx", "depos"), FUN = exists)

    if(all(check == T)){
      print("Nødvendige datasett funnet, starter applikasjon")
    } else{
      stop(cat("Nødvendige datasett ikke funnet. Applikasjonen trenger tre datasett: \br
           fx, repos og depos med hhv. fx-swapdata, repodata og data for usikrede transaksjoner. \br
           De må ha disse navnene. Enten last dem ned/endre navn eller kjør applikasjon med \br
            load_source_data = TRUE"))
    }


  }


  ##### Sett opp valg til FX-swap-grafer #######
  fx_reportingbanks = c(sort(unique(fx$ReportingAgentName)))
  fx_counterparties = c(sort(unique(fx$CounterPartyName)))

  fx_counterparties_group = list("Norske motparter (eks. rapportører)" = "no_cps",
                                 "Utenlandske motparter (eks. rapportører)" = "foreign_cps",
                                 "Alle motparter (eks. rapportører)" = "all_other_cps")

  fx_groupings = c("Løpetid" = "tenor",
                   "Motpart" = "CounterPartyName",
                   "Motpartsland" = "CounterpartyCountry",
                   "Rapportørbank" = "ReportingAgentName")

  fx_tenors = c(levels(fx$tenor))
  fx_currencies = c(sort(unique(fx$FxFrgnCcy)))

  ###### Sett opp valg til repo-grafer #######
  repo_reportingbanks = c(sort(unique(repos$ReportingAgentName)))
  repo_counterparties = c(sort(unique(repos$CounterPartyName)))

  repo_groupings = list("Løpetid" = "tenor2",
                        "Motpart" = "CounterPartyName",
                        "Land på motpart" = "CounterpartyCountry",
                        "Rapportørbank" = "ReportingAgentName",
                        "Type sikkerhet" = "collateral_category",
                        "ISIN" = "ShortName")

  repo_collateral_types = c(sort(unique(repos$collateral_category)))
  repo_tenors = c(levels(repos$tenor2))
  repo_transaction_types = list("Utlån av kroner mot papir" = "LEND",
                                "Innlån av kroner mot papir" = "BORR",
                                "Begge" = "Begge")


  #### Sett opp valg til depos-grafer
  depos_reportingbanks = c(sort(unique(depos$ReportingAgentName)))
  depos_counterparties = c(sort(unique(depos$CounterPartyName)))

  depos_counterparties_group = list("Norske motparter (eks. rapportører)" = "no_cps",
                                    "Utenlandske motparter (eks. rapportører)" = "foreign_cps",
                                    "Alle motparter (eks. rapportører)" = "nonreportingagents",
                                    "Andre rapportørbanker" = "reportingagents")
  depos_ratetypes = list("Fast" = "FIXE",
                         "Flyt" = "VARI",
                         "Begge" = "both")


  depos_groupings = c("Løpetid" = "tenor",
                      "Motpart" = "CounterPartyName",
                      "Motpartsland" = "CounterpartyCountry",
                      "Rapportørbank" = "ReportingAgentName",
                      "Rentetype" = "Ratetype")

  depos_tenors = c(levels(depos$tenor))

  depos_directions = list("Innlån" = "BORR",
                          "Utlån" = "LEND")

  min_date = min(fx$TradDt)
  max_date = max(fx$TradDt)

  header <- dashboardHeader(title = "Data fra RPD")

  nb_colors <- nomafunctions::nb_colors


  ##### Oppsett av sidebar ####
  sidebar <- dashboardSidebar(

    #Oppsett av fanene i menyen til venstre:
    sidebarMenu(
      #Velkomstfane
      menuItem("Velkommen!",
               tabName = "velkommen"),

      #Fane for FX-swap. Denne inneholder tabs med forskjellige grafer, definert under
      menuItem("FX-swap",
               tabName = "fx-swap"),

      #Fane for repos
      menuItem("Repo",
               tabName = "repo"),

      menuItem("Usikret",
               tabName = "depos")


    )
  )


  ##### Oppsett av Body ####

  body <- dashboardBody(


    ##### Sett opp tabs ####
    tabItems(
      ##### Oppsett av velkomstside ####
      tabItem(tabName = "velkommen",
              fluidRow(
                box(h1("Velkommen til RPD-data i Shiny"),
                    br(),
                    HTML("Her kan man plotte diverse grafer med data fra RPD. De tre segmentene,
                    FX-swap, repo og usikrede transaksjoner er delt opp i hver sin side. Velg disse i
                    menyen til venstre.
                         "),
                    br(),
                    br(),
                    HTML("Man kan filtrere, sortere og gruppere dataene på mange forskjellige
                         måter i applikasjonen. Man kan f.eks. plotte utestående volum i FX-swap for en eller
                         flere rapportørbanker, fordelt etter motpart, løpetid, land på motpart, mm."),
                    br(),
                    br(),
                    HTML("Man kan velge motparter man vil inkludere i grafene på to måter. Enten kan man
                         velge enkeltmotparter, f.eks. DNBs omsetning i FX-swap mot en eller flere enkeltmotparter,
                         slik som Nordea, Handelsbanken og JPMorgan, eller man kan plotte DNBs omsetning mot
                         forhåndsdefinerte motpartsgrupper, slik som alle utenlandske motparter. De forhåndsdefinerte
                         motpartsgruppene varierer fra plot til plot. Man styrer hva slags type motpartsvalg
                         man vil gjøre med menyen 'Type motpartsvalg'. "),
                    br(), br(),
                    HTML("De forskjellige plotene er interaktive. Man kan hovre over, zoome inn og ut,
                         og isolere eller fjerne serier ved å dobbel- eller enkeltklikke på serienes
                         legend. En liten advarsel: Det tar langre tid å generere figuren, jo flere
                         serier og tilhørende datapunkter man har. Dette problemet blir ikke noe mindre
                         av at figurene er interaktive. Så langt man kan, er det derfor et poeng å ikke
                         plotte alt for mange serier på en gang (det gir uansett en veldig lite lesbar
                         graf). Eksempelvis kan det ta lang tid å generere et plot om man velger å
                         plotte all omsetning for alle banker mot alle motparter i FX-swap, og sorterer
                         på motpart. Da skal man plotte omsetning for veldig veldig mange datapunkter,
                         ettersom bankene handler med veldig mange motparter i FX-swapper. Plottet vil
                         genereres til slutt, men det kan ta en del tid. Hvis man opplever at applikasjonen
                         kjører treigt, og det tar lang tid å generere plots, kan man og prøve å lukke bloomberg,
                         eller andre applikasjoner som krever mye minne, særlig hvis man sitter på VPN.")
                    , width = 8)
              )
      ),

      ##### Oppsett av FX-swap-siden ####
      tabItem(
        #tabName fx-swap refererer til tab definert i sidebar
        tabName = "fx-swap",

        #FX-swap-siden består av en boks med menyer for valg av parametre,
        # og forskjellige faner med plots
        #### Innhold i FX-swap-side ####
        fluidRow(
          #### Boks med oppsett av menyer for valg av parametre ####
          box(
            ##### dropdown-meny for valg av rapportørbank####
            pickerInput(inputId = "fx_reporting_agent",
                        label = "Velg rapportørbank",
                        choices = fx_reportingbanks, # <- Default er select all
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(fx_reportingbanks)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = TRUE,
                        selected = fx_reportingbanks),

            ##### dropdown-meny for type motpartsvalg: grupper eller enkeltmotparter ####
            pickerInput(inputId = "fx_counterpartychoice",
                        "Type motpartsvalg",
                        choices = c("Enkeltmotparter", "Motpartsgrupper"),
                        multiple = F,
                        selected = "Enkeltmotparter"),


            ##### dropdown-meny for valg av motpart ####
            conditionalPanel("input.fx_counterpartychoice == 'Enkeltmotparter'",
                             pickerInput(inputId = "fx_counterparty",
                                         label = "Velg motpart",
                                         choices = fx_counterparties,
                                         selected = fx_counterparties, # <- Default er select all
                                         multiple = TRUE,
                                         options = pickerOptions(
                                           `actions-box` = TRUE,
                                           liveSearch = T,
                                           liveSearchNormalize = T,
                                           `selected-text-format` = paste0("count > ", length(fx_counterparties)-1),
                                           `count-selected-text` = "Alle",
                                           selectAllText = "Velg alle",
                                           deselectAllText = "Dropp alle"))),

            ##### dropdown-meny for valg av motpart ####
            conditionalPanel("input.fx_counterpartychoice == 'Motpartsgrupper'",
                             pickerInput(inputId = "fx_counterpartygroup",
                                         label = "Velg motpartsgruppe",
                                         choices = fx_counterparties_group,
                                         selected = fx_counterparties_group[[1]],
                                         multiple = FALSE
                                         # options = pickerOptions(
                                         #   `actions-box` = TRUE,
                                         #   liveSearch = T,
                                         #   liveSearchNormalize = T,
                                         #   `selected-text-format` = paste0("count > ", length(fx_counterparties)-1),
                                         #   `count-selected-text` = "Alle",
                                         #   selectAllText = "Velg alle",
                                         #   deselectAllText = "Dropp alle")

                             )),

            ##### dropdown-meny for valg av grupperingsvariabel     ########
            pickerInput(inputId = "fx_grouping",
                        label = "Sorter etter",
                        choices = fx_groupings,
                        multiple = F, #Kan kun velge en
                        selected = "tenor"),

            ##### dropdown-meny for valg av løpetider      ####
            pickerInput(inputId = "fx_plot_tenors",
                        label = "Løpetider",
                        choices = fx_tenors,
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(fx_tenors)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = TRUE,
                        selected = fx_tenors),

            checkboxInput(inputId = "fx_group_forwards",
                               label = "Grupper forward løpetider",
                               value = TRUE),


            ##### dropdown-meny for valg av valuta      ####
            pickerInput(inputId = "fx_plot_currency",
                        label = "Valuta",
                        choices = fx_currencies,
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(fx_tenors)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = TRUE,
                        selected = "USD"),

            ##### checkbox som bestemmer om man skal bruke grouping restriction eller ikke ######
            #default er TRUE
            checkboxInput("fx_grouping_restriction",
                          "Begrens sorteringsvariabler",
                          value = TRUE),


            ##### conditionalPanel: Hvis forrige checkbox er TRUE, #######
            #vis numericInput som viser grupperingsrestriksjonsvariabelen
            conditionalPanel("input.fx_grouping_restriction",
                             numericInput("fx_number_restriction",
                                          label = "Maks sorteringsvariabler",
                                          min = 1, max = 50, value = 15)),

            ##### checkbox som bestemmer om man skal trunkere navn eller ikke ########
            checkboxInput("fx_truncate_names",
                          "Trunker navn",
                          value = TRUE),

            ##### checkbox som bestemmer om man skal ha automatisk tittel eller ikke ######
            checkboxInput("fx_autotitle",
                          "Automatisk tittel",
                          value = TRUE),

            ##### checkbox som bestemmer om man skal ha automatisk undertittel ######
            checkboxInput("fx_auto_subtitle",
                          "Automatisk undertittel",
                          value = TRUE),

            ##### conditionalPanel som vises hvis man ikke huker av for automatisk tittel #####
            #der kan man skrive inn tittelen man vil ha
            conditionalPanel("input.fx_autotitle==false",
                             textInput("fx_title_text",
                                       "Egendefinert tittel",
                                       value = "Dette er en tittel")),


            ##### conditionalPanel som vises hvis man ikke huker av for automatisk undertittel #####
            #der kan man skrive inn undertittelen man vil ha
            conditionalPanel("input.fx_auto_subtitle==false",
                             textInput("fx_subtitle_text",
                                       "Egendefinert undertittel",
                                       value = "Dette er en undertitteltittel")),
            width = 2
          ),




          #### tabBox med innhold til FX-swap-siden ######
          tabBox(id = "fx_faner",
                 #### Tab med graf for omsetning ####
                 tabPanel("Omsetning",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="fx_turnover_date",
                                         label = "Velg datoer",
                                         start = as.character(max_date - 30),
                                         end = as.character(max_date)),
                          ##### plotlyOutput med plot, + downloadButtons for nedlasting av data og plot #####
                          plotlyOutput("p_fx_turnover", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_fx_turnover_data",
                                         label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_fx_turnover_plot",
                                         label = "Last ned html-graf")
                 ),

                 #### Tab med graf for utestående ####
                 tabPanel("Utestående",
                          ##### dateRangeInput med valg av datoer for plotting ####
                          dateRangeInput(inputId ="fx_outstanding_date",
                                         label = "Velg datoer",
                                         start = "2020-03-01",
                                         end = as.character(Sys.Date())),
                          ##### plotlyOutput med plot + downloadButtons for nedlasting ####
                          plotlyOutput("p_fx_outstanding", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_fx_outstanding_data",
                                         label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_fx_outstanding_plot",
                                         label = "Last ned html-graf")
                 ),
                 #### Tab med graf for aggregert utestående ####
                 tabPanel("Samlet utestående",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="fx_aggregate_outstanding_date",
                                         label = "Velg datoer",
                                         start = "2020-03-01",
                                         end = as.character(Sys.Date())),
                          ##### plotlyOutpout og downloadButtons ####
                          plotlyOutput("p_fx_aggregate_outstanding", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_fx_aggregate_outstanding_data", label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_fx_aggregate_outstanding_plot", label = "Last ned html-graf")
                 ),
                 tabPanel("Summert utestående",
                          dateRangeInput(inputId = "fx_summed_outstanding_date",
                                         label = "Velg datoer",
                                         start = "2020-03-01",
                                         end = as.character(Sys.Date()-1)),
                          pickerInput(inputId = "fx_summed_series",
                                      label = "Velg type",
                                      choices = c("Netto" = "netto",
                                                  "Innlån" = "innlan",
                                                  "Utlan" = "utlan"),
                                      selected = "netto"),
                          plotlyOutput("p_fx_summed_outstanding",width = "auto", height = "600px")

                 ),

                 #### Tab med graf for intradag omsetning ####
                 tabPanel("Intradag omsetning",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="fx_intraday_turnover_date",
                                         label = "Velg datoer",
                                         start = as.character(max_date),
                                         end = as.character(max_date)),
                          ##### checkboxInput som bestemmer om man skal plotte fra et gitt tidspunkt
                          checkboxInput("fx_begrens_tidspunkt",
                                        "Plot fra gitt tidspunkt",
                                        value = FALSE),
                          conditionalPanel("input.fx_begrens_tidspunkt",
                                           timeInput("fx_start_time",
                                                     "Starttidspunkt",
                                                     value = strptime("07:00", "%H:%M"),
                                                     seconds = FALSE)),
                          ##### plotlyOutput med intradag Fx-swap data + downloadbuttons #####
                          plotlyOutput("p_fx_intraday_turnover", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_fx_intraday_turnover_data", label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_fx_intraday_turnover_plot", label = "Last ned html-graf")



                 ),

                 ##### Høyde og bredde på boks. (Litt usikker på hvor godt den funker)
                 width = 10,
                 height = 8)
        )
        ##### FX-side slutt #####
      ),

      #### Repo-side ####
      tabItem(
        tabName = "repo",
        fluidRow(
          box(
            ##### dropdown-meny for valg av rapportørbank####
            pickerInput(inputId = "repo_reporting_agent",
                        label = "Velg rapportørbank",
                        choices = repo_reportingbanks, # <- Default er select all
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(repo_reportingbanks)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = TRUE,
                        selected = repo_reportingbanks),

            ##### dropdown-meny for type motpartsvalg: grupper eller enkeltmotparter ####
            # pickerInput(inputId = "repo_counterpartychoice",
            #             "Type motpartsvalg",
            #             choices = c("Enkeltmotparter", "Motpartsgrupper"),
            #             multiple = F,
            #             selected = "Enkeltmotparter"),

            ##### dropdown-meny for valg av motpart ######
            pickerInput(inputId = "repo_counterparty",
                        label = "Velg motpart",
                        choices = repo_counterparties,
                        selected = repo_counterparties, # <- Default er select all
                        multiple = TRUE,
                        options = pickerOptions(
                          `actions-box` = TRUE,
                          liveSearch = T,
                          liveSearchNormalize = T,
                          `selected-text-format` = paste0("count > ", length(repo_counterparties)-1),
                          `count-selected-text` = "Alle",
                          selectAllText = "Velg alle",
                          deselectAllText = "Dropp alle")),
            ##### dropdown-meny for valg av motpart (IKKE I BRUK) ####
            # conditionalPanel("input.repo_counterpartychoice == 'Enkeltmotparter'",
            #                  pickerInput(inputId = "repo_counterparty",
            #                              label = "Velg motpart",
            #                              choices = repo_counterparties,
            #                              selected = repo_counterparties, # <- Default er select all
            #                              multiple = TRUE,
            #                              options = pickerOptions(
            #                                `actions-box` = TRUE,
            #                                liveSearch = T,
            #                                liveSearchNormalize = T,
            #                                `selected-text-format` = paste0("count > ", length(repo_counterparties)-1),
            #                                `count-selected-text` = "Alle",
            #                                selectAllText = "Velg alle",
            #                                deselectAllText = "Dropp alle"))),

            ##### dropdown-meny for valg av motpart (IKKE I BRUK) ####
            #Ikke i bruk. To do: lag en inndeling av f.eks. HFs, sparebanker, nibor-banker
            #til å bruke som motpartsgrupper
            # conditionalPanel("input.repo_counterpartychoice == 'Motpartsgrupper'",
            #                  pickerInput(inputId = "repo_counterpartygroup",
            #                              label = "Velg motpartsgruppe",
            #                              choices = repo_counterparties_group,
            #                              selected = repo_counterparties_group[[1]],
            #                              multiple = FALSE)),

            ##### dropdown-meny for valg av grupperingsvariabel     ########
            pickerInput(inputId = "repo_grouping",
                        label = "Sorter etter",
                        choices = repo_groupings,
                        multiple = F, #Kan kun velge en
                        selected = repo_groupings[["Løpetid"]]),

            ##### dropdown-meny for valg av løpetider      ####
            pickerInput(inputId = "repo_plot_tenors",
                        label = "Løpetider",
                        choices = repo_tenors,
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(repo_tenors)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = TRUE,
                        selected = repo_tenors),

            ##### dropdown-meny for valg av type sikkerhet      ####
            pickerInput(inputId = "repo_plot_collateral",
                        label = "Typer sikkerhet",
                        choices = repo_collateral_types,
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(repo_collateral_types)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = TRUE,
                        selected = repo_collateral_types),

            ##### checkbox som bestemmer om man skal bruke grouping restriction eller ikke ######
            #default er TRUE
            checkboxInput("repo_grouping_restriction",
                          "Begrens sorteringsvariabel",
                          value = TRUE),

            conditionalPanel("input.repo_grouping_restriction",
                             numericInput("repo_number_restriction",
                                          label = "Maks antall sorteringsvariabler",
                                          min = 1, max = 50, value = 10)),

            ##### checkbox som bestemmer om man skal trunkere navn eller ikke ########
            checkboxInput("repo_truncate_names",
                          "Trunker navn",
                          value = TRUE),

            ##### conditionalPanel: Hvis forrige checkbox er TRUE, #######
            #vis numericInput som viser grupperingsrestriksjonsvariabelen


            ##### checkbox som bestemmer om man skal ha automatisk tittel eller ikke ######
            checkboxInput("repo_autotitle",
                          "Automatisk tittel",
                          value = TRUE),

            ##### checkbox som bestemmer om man skal ha automatisk undertittel ######
            checkboxInput("repo_auto_subtitle",
                          "Automatisk undertittel",
                          value = TRUE),

            ##### conditionalPanel som vises hvis man ikke huker av for automatisk tittel #####
            #der kan man skrive inn tittelen man vil ha
            conditionalPanel("input.repo_autotitle==false",
                             textInput("repo_title_text",
                                       "Egendefinert tittel",
                                       value = "Dette er en tittel")),


            ##### conditionalPanel som vises hvis man ikke huker av for automatisk undertittel #####
            #der kan man skrive inn undertittelen man vil ha
            conditionalPanel("input.repo_auto_subtitle==false",
                             textInput("repo_subtitle_text",
                                       "Egendefinert undertittel",
                                       value = "Dette er en undertitteltittel")),
            width = 2
          ),

          #### tabBox med innhold til repo-fanen ######
          tabBox(id = "repo_faner",

                 #### tabPanel for repo-omsetning #####
                 tabPanel("Omsetning",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="repo_turnover_date",
                                         label = "Velg datoer",
                                         start = as.character(max_date - 30),
                                         end = as.character(max_date)),
                          ##### plotlyOutput med plot, + downloadButtons for nedlasting av data og plot #####
                          plotlyOutput("p_repo_turnover", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_repo_turnover_data",
                                         label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_repo_turnover_plot",
                                         label = "Last ned html-graf")
                 ),

                 #### tabPanel for repo-utestående ####
                 tabPanel("Utestående",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="repo_outstanding_date",
                                         label = "Velg datoer",
                                         start = "2020-02-01",
                                         end = as.character(max_date)),
                          ##### plotlyOutput med plot, + downloadButtons for nedlasting av data og plot #####
                          plotlyOutput("p_repo_outstanding", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_repo_outstanding_data",
                                         label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_repo_outstanding_plot",
                                         label = "Last ned html-graf")
                 ),
                 #### tabPanel for repo-renter ####

                 tabPanel("Renter",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="repo_rates_date",
                                         label = "Velg datoer",
                                         start = as.character(max_date - 30),
                                         end = as.character(max_date)),
                          pickerInput(inputId = "repo_rates_direction",
                                      label = "Velg transaksjonstype",
                                      choices = repo_transaction_types,
                                      selected = repo_transaction_types,
                                      multiple = F),
                          ##### plotlyOutput med plot, + downloadButtons for nedlasting av data og plot #####
                          plotlyOutput("p_repo_rates", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_repo_rates_data",
                                         label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_repo_rates_plot",
                                         label = "Last ned html-graf")
                 ),
                 #### tabPanel for repo-haircuts ####
                 tabPanel("Haircuts",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="repo_haircuts_date",
                                         label = "Velg datoer",
                                         start = as.character(max_date - 30),
                                         end = as.character(max_date)),
                          pickerInput(inputId = "repo_haircuts_direction",
                                      label = "Velg transaksjonstype",
                                      choices = repo_transaction_types,
                                      selected = repo_transaction_types,
                                      multiple = F),
                          ##### plotlyOutput med plot, + downloadButtons for nedlasting av data og plot #####
                          plotlyOutput("p_repo_haircuts", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_repo_haircuts_data",
                                         label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_repo_haircuts_plot",
                                         label = "Last ned html-graf")
                 ),


                 width = 10,
                 height = 8



          )

        )

      ),
      ##### Oppsett av depos-siden ####
      tabItem(
        #tabName depos-swap refererer til tab definert i sidebar
        tabName = "depos",

        #depos-swap-siden består av en boks med menyer for valg av parametre,
        # og forskjellige faner med plots
        #### Innhold i depos-side ####
        fluidRow(
          #### Boks med oppsett av menyer for valg av parametre ####
          box(
            ##### dropdown-meny for valg av rapportørbank####
            pickerInput(inputId = "depos_reporting_agent",
                        label = "Velg rapportørbank",
                        choices = depos_reportingbanks, # <- Default er select all
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(depos_reportingbanks)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = TRUE,
                        selected = depos_reportingbanks),

            ##### dropdown-meny for type motpartsvalg: grupper eller enkeltmotparter ####
            pickerInput(inputId = "depos_counterpartychoice",
                        "Type motpartsvalg",
                        choices = c("Enkeltmotparter", "Motpartsgrupper"),
                        multiple = F,
                        selected = "Motpartsgrupper"),


            ##### dropdown-meny for valg av enkeltmotpart ####
            conditionalPanel("input.depos_counterpartychoice == 'Enkeltmotparter'",
                             pickerInput(inputId = "depos_counterparty",
                                         label = "Velg motpart",
                                         choices = depos_counterparties,
                                         selected = depos_counterparties, # <- Default er select all
                                         multiple = TRUE,
                                         options = pickerOptions(
                                           `actions-box` = TRUE,
                                           liveSearch = T,
                                           liveSearchNormalize = T,
                                           `selected-text-format` = paste0("count > ", length(depos_counterparties)-1),
                                           `count-selected-text` = "Alle",
                                           selectAllText = "Velg alle",
                                           deselectAllText = "Dropp alle"))),

            ##### dropdown-meny for valg av motpartsgruppe ####
            conditionalPanel("input.depos_counterpartychoice == 'Motpartsgrupper'",
                             pickerInput(inputId = "depos_counterpartygroup",
                                         label = "Velg motpartsgruppe",
                                         choices = depos_counterparties_group,
                                         selected = depos_counterparties_group[[4]],
                                         multiple = FALSE

                             )),

            ##### dropdown-meny for valg av grupperingsvariabel     ########
            pickerInput(inputId = "depos_grouping",
                        label = "Sorter etter",
                        choices = depos_groupings,
                        multiple = F, #Kan kun velge en
                        selected = "ReportingAgentName"),

            ##### dropdown-meny for valg av løpetider      ####
            pickerInput(inputId = "depos_plot_tenors",
                        label = "Løpetider",
                        choices = depos_tenors,
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(depos_tenors)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = TRUE,
                        selected = "ON"),
            #### dropdown-meny for valg av rentetype      ####
            pickerInput(inputId = "depos_plot_ratetype",
                        label = "Rentetype",
                        choices = depos_ratetypes,
                        options = pickerOptions(`actions-box` = TRUE,
                                                `selected-text-format` = paste0("count > ", length(depos_tenors)-1),
                                                `count-selected-text` = "Alle",
                                                selectAllText = "Velg alle",
                                                deselectAllText = "Dropp alle"),
                        multiple = F,
                        selected = "FIXE"),

            ##### checkbox som bestemmer om man skal bruke grouping restriction eller ikke ######
            #default er TRUE
            checkboxInput("depos_grouping_restriction",
                          "Begrens sorteringsvariabler",
                          value = TRUE),


            ##### conditionalPanel: Hvis forrige checkbox er TRUE, #######
            #vis numericInput som viser grupperingsrestriksjonsvariabelen
            conditionalPanel("input.depos_grouping_restriction",
                             numericInput("depos_number_restriction",
                                          label = "Maks sorteringsvariabler",
                                          min = 1, max = 50, value = 15)),

            ##### checkbox som bestemmer om man skal trunkere navn eller ikke ########
            checkboxInput("depos_truncate_names",
                          "Trunker navn",
                          value = TRUE),

            ##### checkbox som bestemmer om man skal ha automatisk tittel eller ikke ######
            checkboxInput("depos_autotitle",
                          "Automatisk tittel",
                          value = TRUE),

            ##### checkbox som bestemmer om man skal ha automatisk undertittel ######
            checkboxInput("depos_auto_subtitle",
                          "Automatisk undertittel",
                          value = TRUE),

            ##### conditionalPanel som vises hvis man ikke huker av for automatisk tittel #####
            #der kan man skrive inn tittelen man vil ha
            conditionalPanel("input.depos_autotitle==false",
                             textInput("depos_title_text",
                                       "Egendefinert tittel",
                                       value = "Dette er en tittel")),


            ##### conditionalPanel som vises hvis man ikke huker av for automatisk undertittel #####
            #der kan man skrive inn undertittelen man vil ha
            conditionalPanel("input.depos_auto_subtitle==false",
                             textInput("depos_subtitle_text",
                                       "Egendefinert undertittel",
                                       value = "Dette er en undertitteltittel")),
            width = 2
          ),




          #### tabBox med innhold til depos-siden ######
          tabBox(id = "depos_faner",
                 #### Tab med graf for omsetning ####
                 tabPanel("Omsetning",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="depos_turnover_date",
                                         label = "Velg datoer",
                                         start = as.character(max_date - 30),
                                         end = as.character(max_date)),
                          ##### plotlyOutput med plot, + downloadButtons for nedlasting av data og plot #####
                          plotlyOutput("p_depos_turnover", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_depos_turnover_data",
                                         label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_depos_turnover_plot",
                                         label = "Last ned html-graf")
                 ),

                 #### Tab med graf for renter/spread over referanserente  ####
                 tabPanel("Renter",
                          ##### dateRangeInput med valg av datoer for plotting #####
                          dateRangeInput(inputId ="depos_rates_date",
                                         label = "Velg datoer",
                                         start = as.character(max_date-30),
                                         end = as.character(max_date)),
                          ##### pickerInput med valg av innlån, utlån eller begge
                          pickerInput(inputId = "depos_rates_direction",
                                      label = "Velg transaksjonstype",
                                      choices = depos_directions,
                                      selected = "LEND",
                                      multiple = T),
                          ##### plotlyOutput med data for renter/spread + downloadbuttons #####
                          plotlyOutput("p_depos_rates", width = "auto", height = "600px"),
                          br(),
                          downloadButton(outputId = "download_depos_rates_data", label = "Last ned data til Excel"),
                          downloadButton(outputId = "download_depos_rates_plot", label = "Last ned html-graf")



                 ),

                 ##### Høyde og bredde på boks. (Litt usikker på hvor godt den funker)
                 width = 10,
                 height = 8
          )
        )
        ##### Depos-side slutt #####
      )

    )
  )


  ########### UI ##############
  ui <- dashboardPage(header, sidebar, body, useShinyjs())

  server <- function(input, output, session) {

    #   output$res <- renderText({
    #
    #
    #     as.character(strftime(input$fx_start_time, format = "%H:%M"))
    #
    #     })


    ##### FX-plots ####

    #Hvis man skulle ha lys til å automatisk bytte valg
    # av motpartsvalg når man bytter fane. Ikke i bruk.
    # observeEvent(input$fx_faner, {
    #   if(input$fx_faner == "Samlet utestående"){
    #     updatePickerInput(session = session,
    #                       inputId = "fx_counterpartychoice",
    #                       selected = "Motpartsgrupper")
    #   }
    #
    # })


    ##### Sett opp alternativer i fx_options ####
    #Litt knot, men kan til gjengjeld gjenbrukes for alle FX-plots.
    fx_options <- list()

    fx_options[["reporters"]] <- reactive({
      if(length(input$fx_reporting_agent) == length(fx_reportingbanks)) {
        "all"
      } else {
        input$fx_reporting_agent
      }
    })

    fx_options[["counters"]] <- reactive({

      if(input$fx_counterpartychoice == "Enkeltmotparter"){
        if(length(input$fx_counterparty) == length(fx_counterparties)) {
          "all"
        } else {
          input$fx_counterparty
        }
      } else{
        input$fx_counterpartygroup
      }

    })

    fx_options[["restriction"]] <- reactive({
      if(input$fx_grouping_restriction) {
        input$fx_number_restriction
      } else{
        FALSE
      }
    })

    fx_options[["plot_title"]] <- reactive({
      if(input$fx_autotitle){
        NULL
      } else{
        input$fx_title_text
      }
    })

    fx_options[["plot_subtitle"]] <- reactive({
      if(input$fx_auto_subtitle){
        NULL
      } else{
        input$fx_subtitle_text
      }
    })

    fx_options[["tenors"]] <- reactive({

      if(length(input$fx_plot_tenors) == length(fx_tenors)) {
        "all"
      } else {
        input$fx_plot_tenors
      }

    })

    fx_options[["time_start"]] <- reactive({

      if(all(input$fx_begrens_tidspunkt == FALSE)) {
        NULL
      } else {
        as.character(strftime(input$fx_start_time, format = "%H:%M"))
      }

    })

    fx_options[["currency"]] <- reactive({

      if(length(input$fx_plot_currency) == length(fx_currencies)){
        "all"
      } else{
        input$fx_plot_currency
      }

    })

    fx_filtered = reactive({


      fx <- fx %>% filter(!is.na(CounterPartyName))

      if(input$fx_group_forwards)  {

        fx %>% mutate(tenor = as.character(tenor),
                      tenor = if_else(grepl("F",tenor), "Forward start", tenor),
                      tenor = factor(tenor, levels = c("ON", "TN", "SN", "1w", "2w", "3w", "1m", "2m", "3m", "4m", "5m", "6m", ">6m", "Forward start", "Other")))
      } else
        fx
    })

    #### FX-turnover plot og data. ####
    #Kjører først kommandoen plot_fx_swap_turnover med return_data = TRUE
    #Output blir da en liste med to elementer, plot med navn "p" og datasett
    #med navn df. Plot-output blir da fx_turnover()[["p]] (Husk reactive-klammer),
    #mens datasett blir fx_turnover()[["df"]]
    fx_turnover <- reactive({
      plot_fx_swap_turnover(fx_filtered(),
                            start_date = input$fx_turnover_date[1],
                            end_date = input$fx_turnover_date[2],
                            reporting_banks = fx_options$reporters(),
                            grouping = input$fx_grouping,
                            grouping_restriction = fx_options$restriction(),
                            truncate_names = input$fx_truncate_names,
                            return_data = TRUE,
                            counterparty = fx_options$counters(),
                            currency = fx_options$currency(),
                            tenors = fx_options$tenors(),
                            title = TRUE,
                            subtitle = TRUE,
                            title_text = fx_options$plot_title(),
                            subtitle_text = fx_options$plot_subtitle()
      )

    })

    # Turnover-plot
    output$p_fx_turnover <- renderPlotly({

      fx_turnover()[["p"]] ## NB, merk "()" i fx_turnover(). Den er reactive, og derfor en funksjon.
    })

    #Eksport av turnover-data og -plots
    #Sett opp eksport-data:
    #Tar datasettet generert fra plot_fx_swap_turnover, deler det i to,
    #en med innlån, en med utlån, pivot_wider, slik at det blir strukturert som
    #tidsserier og legger dem i en liste
    fx_turnover_export = reactive({

      grouping_name = names(fx_turnover()$df[2])

      list(innlan = fx_turnover()$df %>%
             filter(grepl("BUYI", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = Volume),

           utlan = fx_turnover()$df %>%
             filter(grepl("SELL", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = Volume)
      )


    })

    #Listen generert i forrige steg eksporteres
    output$download_fx_turnover_data <- downloadHandler(
      filename = "fx_swap_turnover.xlsx",

      content = function(file){
        write.xlsx(fx_turnover_export(), file)}
    )

    #Turnover-plot eksporteres:
    output$download_fx_turnover_plot <- downloadHandler(

      filename = "fx_swap_turnover_plot.html",

      content = function(file){
        saveWidget(as_widget(fx_turnover()[["p"]]), file, selfcontained = TRUE)
      }
    )


    #### FX swap outstanding plot og data ####
    #Sett opp liste med plot og data basert på valg-
    fx_outstanding <- reactive({
      plot_fx_swap_outstanding(fx_filtered(),
                               start_date = input$fx_outstanding_date[1],
                               end_date = input$fx_outstanding_date[2],
                               reporting_banks = fx_options$reporters(),
                               #input$fx_turnover_reporting_agent,
                               grouping = input$fx_grouping,
                               grouping_restriction = fx_options$restriction(),
                               truncate_names = input$fx_truncate_names,
                               return_data = TRUE,
                               counterparty = fx_options$counters(),
                               currency = fx_options$currency(),
                               tenors = input$fx_plot_tenors,
                               title = TRUE,
                               subtitle = TRUE,
                               title_text = fx_options$plot_title(),
                               subtitle_text = fx_options$plot_subtitle()
      )

    })

    #### Output og data for FX turnover-plots
    output$p_fx_outstanding <- renderPlotly({

      fx_outstanding()[["p"]] ## NB, merk "()" i fx_outstanding(). Den er reactive, og derfor en funksjon.
    })

    # Eksport av outstanding-data og -plots ##
    #Sett opp eksport-data. Samme oppsett som for turnover:
    #Tar datasettet generert fra plot_fx_swap_outstanding, deler det i to,
    #en med innlån, en med utlån, pivot_wider, slik at det blir strukturert som
    #tidsserier og legger dem i en liste
    fx_outstanding_export = reactive({

      grouping_name = names(fx_outstanding()$df[3])

      list(innlan = fx_outstanding()$df %>%
             filter(grepl("BUYI", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = outstanding),

           utlan = fx_outstanding()$df %>%
             filter(grepl("SELL", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = outstanding)
      )


    })

    #Listen generert i forrige steg eksporteres
    output$download_fx_outstanding_data <- downloadHandler(
      filename = "fx_swap_outstanding.xlsx",

      content = function(file){
        write.xlsx(fx_outstanding_export(), file)}
    )

    #outstanding-plot eksporteres:
    output$download_fx_outstanding_plot <- downloadHandler(

      filename = "fx_swap_outstanding_plot.html",

      content = function(file){
        saveWidget(as_widget(fx_outstanding()[["p"]]), file, selfcontained = TRUE)
      }
    )

    #### Aggregate FX-swap outstanding plot og data ####
    # Sett opp liste med plot og data basert på valg
    fx_aggregate_outstanding <- reactive({
      plot_aggregate_fx_swap_outstanding(fx_filtered(),
                                         start_date = input$fx_aggregate_outstanding_date[1],
                                         end_date = input$fx_aggregate_outstanding_date[2],
                                         reporting_banks = fx_options$reporters(),
                                         #input$fx_turnover_reporting_agent,
                                         return_data = TRUE,
                                         counterparty = fx_options$counters(),
                                         currency = fx_options$currency(),
                                         tenors = fx_options[["tenors"]](),
                                         title = TRUE,
                                         subtitle = TRUE,
                                         title_text = fx_options$plot_title(),
                                         subtitle_text = fx_options$plot_subtitle()
      )

    })

    # Aggregate outstanding-plot
    output$p_fx_aggregate_outstanding <- renderPlotly({

      fx_aggregate_outstanding()[["p"]] ## NB, merk "()" i fx_outstanding(). Den er reactive, og derfor en funksjon.

    })

    # Eksport av aggregate outstanding-data og -plots #
    #Sett opp eksport-data. Samme oppsett som for turnover:
    #Tar datasettet generert fra plot_fx_swap_outstanding, deler det i to,
    #en med innlån, en med utlån, pivot_wider, slik at det blir strukturert som
    #tidsserier og legger dem i en liste
    fx_aggregate_outstanding_export = reactive({

      fx_aggregate_outstanding()[["df"]] %>%
        pivot_wider(names_from = TxTp, values_from = outstanding)

    })

    #Listen generert i forrige steg eksporteres
    output$download_fx_aggregate_outstanding_data <- downloadHandler(
      filename = "fx_swap_aggregate_outstanding.xlsx",

      content = function(file){
        write.xlsx(fx_aggregate_outstanding_export(), file)}
    )

    #outstanding-plot eksporteres:
    output$download_fx_aggregate_outstanding_plot <- downloadHandler(

      filename = "fx_swap_aggregate_outstanding_plot.html",

      content = function(file){
        saveWidget(as_widget(fx_aggregate_outstanding()[["p"]]), file, selfcontained = TRUE)
      }
    )




    fx_summed_outstanding <- reactive({


      agg_plot(fx_filtered(),
               series = input$fx_summed_series,
                          start_date = input$fx_summed_outstanding_date[1],
                          end_date = input$fx_summed_outstanding_date[2],
                          reporting_banks = fx_options$reporters(),
                          grouping = input$fx_grouping,
                          counterparty = fx_options$counters(),
                          currency = fx_options$currency(),
                          tenors = fx_options[["tenors"]]()
                          # title = TRUE,
                          # subtitle = TRUE,
                          # title_text = fx_options$plot_title(),
                          # subtitle_text = fx_options$plot_subtitle()
                 )


      # if(input$fx_group_forwards)  {
      #
      #   fx %>% mutate(tenor = as.character(tenor),
      #                 tenor = if_else(grepl("F",tenor), "Forward start", tenor),
      #                 tenor = factor(tenor, levels = c("ON", "TN", "SN", "1w", "2w", "3w", "1m", "2m", "3m", "4m", "5m", "6m", ">6m", "Forward start", "Other"))) %>%
      #     agg_plot(start_date = input$fx_summed_outstanding_date[1],
      #              end_date = input$fx_summed_outstanding_date[2],
      #              reporting_banks = fx_options$reporters(),
      #              #input$fx_turnover_reporting_agent,
      #              #return_data = TRUE,
      #              counterparty = fx_options$counters(),
      #              currency = fx_options$currency(),
      #              tenors = fx_options[["tenors"]]()
      #              # title = TRUE,
      #              # subtitle = TRUE,
      #              # title_text = fx_options$plot_title(),
      #              # subtitle_text = fx_options$plot_subtitle()
      #
      #
      #
      #     )
      #
      # } else {
      #
      #   agg_plot(fx,
      #            start_date = input$fx_summed_outstanding_date[1],
      #            end_date = input$fx_summed_outstanding_date[2],
      #            reporting_banks = fx_options$reporters(),
      #            #input$fx_turnover_reporting_agent,
      #            #return_data = TRUE,
      #            counterparty = fx_options$counters(),
      #            currency = fx_options$currency(),
      #            tenors = fx_options[["tenors"]]()
      #            # title = TRUE,
      #            # subtitle = TRUE,
      #            # title_text = fx_options$plot_title(),
      #            # subtitle_text = fx_options$plot_subtitle()
      #   )
      #
      #
      # }



    })

    output$p_fx_summed_outstanding <- renderPlotly({

      fx_summed_outstanding()[["p"]]

    })


    #### Intraday turnover plot og data ######
    # Sett opp liste med plot og data basert på valg #
    fx_intraday_turnover <- reactive({
      plot_intraday_fx_swap_trades(fx_filtered(),
                                   start_date = input$fx_intraday_turnover_date[1],
                                   end_date = input$fx_intraday_turnover_date[2],
                                   reporting_banks = fx_options$reporters(),
                                   time_start = fx_options$time_start(),
                                   grouping = input$fx_grouping,
                                   return_data = TRUE,
                                   counterparty = fx_options$counters(),
                                   currency = fx_options$currency(),
                                   tenors = fx_options[["tenors"]](),
                                   title = TRUE,
                                   subtitle = TRUE,
                                   title_text = fx_options$plot_title(),
                                   subtitle_text = fx_options$plot_subtitle()
      )

    })

    # Intradag omsetning-plot #
    output$p_fx_intraday_turnover <- renderPlotly({

      fx_intraday_turnover()[["p"]] ## NB, merk "()" i fx_intraday_turnover(). Den er reactive, og derfor en funksjon.

    })

    # Dataframe til eksport
    fx_intraday_turnover_export = reactive({

      fx_intraday_turnover()[["df"]] %>%
        select(TradeTime,
               TxNmnlAmt,
               TxTp,
               ReportingAgentName,
               CounterPartyName,
               tenor,
               Rentedifferanse)


    })

    #Dataframe generert i forrige steg eksporteres
    output$download_fx_intraday_turnover_data <- downloadHandler(
      filename = "fx_swap_intraday_turnover.xlsx",

      content = function(file){
        write.xlsx(fx_intraday_turnover_export(), file)}
    )

    #intradag-plot eksporteres:
    output$download_fx_intraday_turnover_plot <- downloadHandler(

      filename = "fx_swap_intraday_turnover_plot.html",

      content = function(file){
        saveWidget(as_widget(fx_intraday_turnover()[["p"]]), file, selfcontained = TRUE)
      }
    )





    ################### REPO #########################

    ##### Sett opp alternativer i repo_options, på samme måte som for fx-swap ####
    #Litt knot, men kan til gjengjeld gjenbrukes for alle repo-plots.
    repo_options <- list()

    repo_options[["reporters"]] <- reactive({
      if(length(input$repo_reporting_agent) == length(repo_reportingbanks)) {
        "all"
      } else {
        input$repo_reporting_agent
      }
    })

    repo_options[["counters"]] <- reactive({

      #if(input$repo_counterpartychoice == "Enkeltmotparter"){
      if(length(input$repo_counterparty) == length(repo_counterparties)) {
        "all"
      } else {
        input$repo_counterparty
      }
      #} else{
      #  input$repo_counterpartygroup
      #}

    })

    repo_options[["restriction"]] <- reactive({
      if(input$repo_grouping_restriction) {
        input$repo_number_restriction
      } else{
        FALSE
      }
    })

    repo_options[["plot_title"]] <- reactive({
      if(input$repo_autotitle){
        NULL
      } else{
        input$repo_title_text
      }
    })

    repo_options[["plot_subtitle"]] <- reactive({
      if(input$repo_auto_subtitle){
        NULL
      } else{
        input$repo_subtitle_text
      }
    })

    repo_options[["tenors"]] <- reactive({

      if(length(input$repo_plot_tenors) == length(repo_tenors)) {
        "all"
      } else {
        input$repo_tenors
      }

    })

    repo_options[["collateral_category"]] <- reactive({

      if(length(input$repo_plot_collateral) == length(repo_collateral_types)) {
        "all"
      } else {
        input$repo_plot_collateral
      }

    })

    repo_options[["rates_direction"]] <- reactive({

      if(input$repo_rates_direction == "Begge"){
        c("BORR", "LEND")
      } else{
        input$repo_rates_direction
      }




    })

    repo_options[["haircuts_direction"]] <- reactive({

      if(input$repo_haircuts_direction == "Begge"){
        c("BORR", "LEND")
      } else{
        input$repo_haircuts_direction
      }




    })

    #### Hvis man velger rente- eller haircuts-fanen, skrus valg av grouping restriction av
    observeEvent(input$repo_faner, {
      if(input$repo_faner %in% c("Renter", "Haircuts")){
        shinyjs::disable("repo_grouping_restriction")
        shinyjs::disable("repo_number_restriction")
      } else{
        shinyjs::enable("repo_grouping_restriction")
        shinyjs::enable("repo_number_restriction")

      }

    })


    #### Repo turnover plot og data #####
    repo_turnover <- reactive({
      plot_repo_turnover(repos,
                         start_date = input$repo_turnover_date[1],
                         end_date = input$repo_turnover_date[2],
                         reporting_banks = repo_options$reporters(),
                         #input$repo_turnover_reporting_agent,
                         grouping = input$repo_grouping,
                         grouping_restriction = repo_options$restriction(),
                         truncate_names = input$repo_truncate_names,
                         return_data = TRUE,
                         counterparty = repo_options$counters(),
                         collateral_category = repo_options$collateral_category(),
                         tenors = input$repo_plot_tenors,
                         title = TRUE,
                         subtitle = TRUE,
                         title_text = repo_options$plot_title(),
                         subtitle_text = repo_options$plot_subtitle()
      )

    })

    #Turnover-plot ####
    output$p_repo_turnover <- renderPlotly({

      repo_turnover()[["p"]] ## NB, merk "()" i fx_turnover(). Den er reactive, og derfor en funksjon.
    })

    # Eksporter turnover data og html-graf

    repo_turnover_export = reactive({

      grouping_name = names(repo_turnover()$df)[2]

      list(innlan = repo_turnover()$df %>%
             filter(grepl("Inn", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = Volume),

           utlan = repo_turnover()$df %>%
             filter(grepl("Ut", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = Volume)
      )


    })

    #Listen generert i forrige steg eksporteres
    output$download_repo_turnover_data <- downloadHandler(
      filename = "repo_turnover.xlsx",

      content = function(file){
        write.xlsx(repo_turnover_export(), file)}
    )

    #Turnover-plot eksporteres:
    output$download_repo_turnover_plot <- downloadHandler(

      filename = "repo_turnover_plot.html",

      content = function(file){
        saveWidget(as_widget(repo_turnover()[["p"]]), file, selfcontained = TRUE)
      }
    )



    # #### Repo outstanding plot og data ####
    repo_outstanding <- reactive({
      plot_repo_outstanding(repos,
                            start_date = input$repo_outstanding_date[1],
                            end_date = input$repo_outstanding_date[2],
                            reporting_banks = repo_options$reporters(),
                            grouping = input$repo_grouping,
                            grouping_restriction = repo_options$restriction(),
                            truncate_names = input$repo_truncate_names,
                            return_data = TRUE,
                            counterparty = repo_options$counters(),
                            collateral_category = repo_options$collateral_category(),
                            tenors = repo_options$tenors(),
                            title = TRUE,
                            subtitle = TRUE,
                            title_text = repo_options$plot_title(),
                            subtitle_text = repo_options$plot_subtitle()
      )

    })

    #### Outstanding-plot
    output$p_repo_outstanding <- renderPlotly({

      repo_outstanding()[["p"]] ##

    })

    #### Eksporter turnover data og html-graf ######
    repo_outstanding_export = reactive({

      grouping_name = names(repo_outstanding()$df)[3]

      list(innlan = repo_outstanding()$df %>%
             filter(grepl("Inn", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = outstanding),

           utlan = repo_outstanding()$df %>%
             filter(grepl("Ut", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = outstanding)
      )


    })

    #Listen generert i forrige steg eksporteres
    output$download_repo_outstanding_data <- downloadHandler(
      filename = "repo_outstanding.xlsx",

      content = function(file){
        write.xlsx(repo_outstanding_export(), file)}
    )

    #outstanding-plot eksporteres:
    output$download_repo_outstanding_plot <- downloadHandler(

      filename = "repo_outstanding_plot.html",

      content = function(file){
        saveWidget(as_widget(repo_outstanding()[["p"]]), file, selfcontained = TRUE)
      }
    )

    ##### Repo rates plot og data ####
    repo_rates <- reactive({
      plot_repo_rates(repos,
                      start_date = input$repo_rates_date[1],
                      end_date = input$repo_rates_date[2],
                      reporting_banks = repo_options$reporters(),
                      grouping = input$repo_grouping,
                      #grouping_restriction = repo_options$restriction(),
                      truncate_names = input$repo_truncate_names,
                      return_data = TRUE,
                      counterparty = repo_options$counters(),
                      collateral_category = repo_options$collateral_category(),
                      tenors = repo_options$tenors(),
                      direction = repo_options$rates_direction(),
                      title = TRUE,
                      subtitle = TRUE,
                      title_text = repo_options$plot_title(),
                      subtitle_text = repo_options$plot_subtitle()
      )

    })

    #### rates-plot
    output$p_repo_rates <- renderPlotly({

      repo_rates()[["p"]] ##

    })

    # Eksporter rates data og html-graf
    repo_rates_export = reactive({

      repo_rates()[["df"]] %>%
        select(TradDt,
               SttlmDt,
               MtrtyDt,
               tenor2,
               TxNmnlAmt,
               TxTp,
               ReportingAgentName,
               CounterPartyName,
               DealRate,
               Hrcut,
               kupongrente,
               ShortName,
               ISIN)


    })

    #Listen generert i forrige steg eksporteres
    output$download_repo_rates_data <- downloadHandler(
      filename = "repo_rates.xlsx",

      content = function(file){
        write.xlsx(repo_rates_export(), file)}
    )

    #rates-plot eksporteres:
    output$download_repo_rates_plot <- downloadHandler(

      filename = "repo_rates_plot.html",

      content = function(file){
        saveWidget(as_widget(repo_rates()[["p"]]), file, selfcontained = TRUE)
      }
    )

    ##### Repo haircuts plot og data ####
    repo_haircuts <- reactive({
      plot_repo_haircuts(repos,
                         start_date = input$repo_haircuts_date[1],
                         end_date = input$repo_haircuts_date[2],
                         reporting_banks = repo_options$reporters(),
                         grouping = input$repo_grouping,
                         #grouping_restriction = repo_options$restriction(),
                         truncate_names = input$repo_truncate_names,
                         return_data = TRUE,
                         counterparty = repo_options$counters(),
                         collateral_category = repo_options$collateral_category(),
                         tenors = repo_options$tenors(),
                         direction = repo_options$haircuts_direction(),
                         title = TRUE,
                         subtitle = TRUE,
                         title_text = repo_options$plot_title(),
                         subtitle_text = repo_options$plot_subtitle()
      )

    })

    #### haircuts-plot
    output$p_repo_haircuts <- renderPlotly({

      repo_haircuts()[["p"]] ##

    })

    #Eksporter haircuts data og html-graf
    repo_haircuts_export = reactive({

      repo_haircuts()[["df"]] %>%
        select(TradDt,
               SttlmDt,
               MtrtyDt,
               tenor2,
               TxNmnlAmt,
               TxTp,
               ReportingAgentName,
               CounterPartyName,
               DealRate,
               Hrcut,
               kupongrente,
               ShortName,
               ISIN)




    })

    #Listen generert i forrige steg eksporteres
    output$download_repo_haircuts_data <- downloadHandler(
      filename = "repo_haircuts.xlsx",

      content = function(file){
        write.xlsx(repo_haircuts_export(), file)}
    )

    #haircuts-plot eksporteres:
    output$download_repo_haircuts_plot <- downloadHandler(

      filename = "repo_haircuts_plot.html",

      content = function(file){
        saveWidget(as_widget(repo_haircuts()[["p"]]), file, selfcontained = TRUE)
      }
    )

    ########## Deposits ############
    ##### Sett opp alternativer i depos_options ####
    #Litt knot, men kan til gjengjeld gjenbrukes for alle depos-plots.
    depos_options <- list()

    depos_options[["reporters"]] <- reactive({
      if(length(input$depos_reporting_agent) == length(depos_reportingbanks)) {
        "all"
      } else {
        input$depos_reporting_agent
      }
    })

    depos_options[["counters"]] <- reactive({

      if(input$depos_counterpartychoice == "Enkeltmotparter"){
        if(length(input$depos_counterparty) == length(depos_counterparties)) {
          "all"
        } else {
          input$depos_counterparty
        }
      } else{
        input$depos_counterpartygroup
      }

    })

    depos_options[["restriction"]] <- reactive({
      if(input$depos_grouping_restriction) {
        input$depos_number_restriction
      } else{
        FALSE
      }
    })

    depos_options[["plot_title"]] <- reactive({
      if(input$depos_autotitle){
        NULL
      } else{
        input$depos_title_text
      }
    })

    depos_options[["plot_subtitle"]] <- reactive({
      if(input$depos_auto_subtitle){
        NULL
      } else{
        input$depos_subtitle_text
      }
    })

    depos_options[["tenors"]] <- reactive({

      if(length(input$depos_plot_tenors) == length(depos_tenors)) {
        "all"
      } else {
        input$depos_plot_tenors
      }

    })

    depos_options[["ratetype"]] <- reactive({

      if(length(input$depos_plot_ratetype) == length(depos_ratetypes)) {
        "both"
      } else {
        input$depos_plot_ratetype
      }

    })

    # depos_options[["time_start"]] <- reactive({
    #
    #   if(all(input$depos_begrens_tidspunkt == FALSE)) {
    #     NULL
    #   } else {
    #     as.character(strftime(input$depos_start_time, format = "%H:%M"))
    #   }
    #
    # })



    ###### depos-turnover plot og data. ####
    #Kjører først kommandoen plot_unsecured_turnover med return_data = TRUE
    #Output blir da en liste med to elementer, plot med navn "p" og datasett
    #med navn df. Plot-output blir da depos_turnover()[["p]] (Husk reactive-klammer),
    #mens datasett blir depos_turnover()[["df"]]
    depos_turnover <- reactive({
      plot_unsecured_turnover(depos,
                              start_date = input$depos_turnover_date[1],
                              end_date = input$depos_turnover_date[2],
                              reporting_banks = depos_options$reporters(),
                              #input$depos_turnover_reporting_agent,
                              grouping = input$depos_grouping,
                              grouping_restriction = depos_options$restriction(),
                              truncate_names = input$depos_truncate_names,
                              return_data = TRUE,
                              counterparty = depos_options$counters(),
                              ratetype = depos_options$ratetype(),
                              tenors = depos_options$tenors(),
                              title = TRUE,
                              subtitle = TRUE,
                              title_text = depos_options$plot_title(),
                              subtitle_text = depos_options$plot_subtitle()
      )

    })

    # Depos turnover plot
    output$p_depos_turnover <- renderPlotly({

      depos_turnover()[["p"]] ## NB, merk "()" i depos_turnover(). Den er reactive, og derfor en funksjon.
    })

    # Eksport av turnover-data og -plots #
    # Sett opp eksport-data:
    # Tar datasettet generert fra plot_fx_swap_turnover, deler det i to,
    # en med innlån, en med utlån, pivot_wider, slik at det blir strukturert som
    # tidsserier og legger dem i en liste
    depos_turnover_export = reactive({

      grouping_name = names(depos_turnover()$df[2])

      list(innlan = depos_turnover()$df %>%
             filter(grepl("Inn", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = Volume),

           utlan = depos_turnover()$df %>%
             filter(grepl("Ut", TxTp)) %>%
             select(-TxTp) %>%
             pivot_wider(names_from = all_of(grouping_name), values_from = Volume)
      )


    })

    #Listen generert i forrige steg eksporteres
    output$download_depos_turnover_data <- downloadHandler(
      filename = "depos_turnover.xlsx",

      content = function(file){
        write.xlsx(depos_turnover_export(), file)}
    )

    #Turnover-plot eksporteres:
    output$download_depos_turnover_plot <- downloadHandler(

      filename = "depos_turnover_plot.html",

      content = function(file){
        saveWidget(as_widget(depos_turnover()[["p"]]), file, selfcontained = TRUE)
      }
    )


    ### Oppdater rentevalg

    observeEvent(input$depos_faner, {
      if(input$depos_faner == "Renter"){
        updatePickerInput(session = session,
                          inputId = "depos_plot_ratetype",
                          choices = list("Fast" = "FIXE",
                                         "Flyt" = "VARI"),
                          selected = "FIXE")
      } else{
        updatePickerInput(session = session,
                          inputId = "depos_plot_ratetype",
                          choices = depos_ratetypes,
                          selected = "FIXE")
      }

    })


    depos_rates <- reactive({
      plot_unsecured_rates(depos,
                           start_date = input$depos_rates_date[1],
                           end_date = input$depos_rates_date[2],
                           reporting_banks = depos_options$reporters(),
                           #input$depos_rates_reporting_agent,
                           grouping = input$depos_grouping,
                           truncate_names = input$depos_truncate_names,
                           return_data = TRUE,
                           counterparty = depos_options$counters(),
                           ratetype = depos_options$ratetype(),
                           direction = input$depos_rates_direction,
                           tenors = depos_options$tenors(),
                           title = TRUE,
                           subtitle = TRUE,
                           title_text = depos_options$plot_title(),
                           subtitle_text = depos_options$plot_subtitle()
      )

    })

    # Depos rates plot
    output$p_depos_rates <- renderPlotly({

      depos_rates()[["p"]] ## NB, merk "()" i depos_rates(). Den er reactive, og derfor en funksjon.
    })

    # Eksport av rates-data og -plots #
    # Sett opp eksport-data:
    depos_rates_export = reactive({

      depos_rates()$df %>% select(
        TradDt,
        SttlmDt,
        MtrtyDt,
        ReportingAgentName,
        CounterPartyName,
        TxNmnlAmt,
        DealRate,
        BsisPtSprd,
        RefRateIndx)



    })

    #Listen generert i forrige steg eksporteres
    output$download_depos_rates_data <- downloadHandler(
      filename = "depos_rates.xlsx",

      content = function(file){
        write.xlsx(depos_rates_export(), file)}
    )

    #rates-plot eksporteres:
    output$download_depos_rates_plot <- downloadHandler(

      filename = "depos_rates_plot.html",

      content = function(file){
        saveWidget(as_widget(depos_rates()[["p"]]), file, selfcontained = TRUE)
      }
    )


    ###### Output slutt ######
    session$onSessionEnded(stopApp)
  }

  k= shinyApp(ui, server)



  runApp(k, launch.browser = TRUE)


}


