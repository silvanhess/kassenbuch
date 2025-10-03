library(shiny)
library(DT)

ui <-
  fluidPage(
    titlePanel("Kassenbuch Jugend Ridu"),
    tabsetPanel(
      tabPanel(
        "Übersicht",
        fluidRow(
          column(
            6,
            h3("Export / Sicherung"),
            downloadButton("backupData", "Daten sichern"),
            fileInput(
              "restoreFile",
              "Daten aus Sicherung wiederherstellen",
              accept = c(".xlsx")
            )
          )
        )
      ),
      tabPanel(
        "Buchung hinzufügen",
        dateInput("date", "Datum:", Sys.Date()),
        numericInput("amount", "Betrag (+Einnahme, -Ausgabe):", 0),
        textInput("note", "Bemerkung:"),
        uiOutput("topicSelect"),
        uiOutput("accountSelect"),
        actionButton("addTrans", "Buchung hinzufügen"),
        DTOutput("transTable")
      ),
      tabPanel(
        "Anlässe verwalten",
        textInput("newTopic", "Neuer Anlass"),
        actionButton("addTopic", "Anlass hinzufügen"),
        DTOutput("topicList")
      ),
      tabPanel(
        "Konten verwalten",
        textInput("newAccount", "Neues Konto"),
        numericInput("startBalance", "Anfangssaldo", 0),
        actionButton("addAccount", "Konto hinzufügen"),
        DTOutput("accountList")
      ),
      tabPanel(
        "Anlass abrechnen",
        uiOutput("topicSelectReport"),
        DTOutput("reportTable")
      ),
      tabPanel(
        "Kontoauszug erstellen",
        dateInput("startDate", "Beginndatum", Sys.Date() - 365),
        dateInput("endDate", "Enddatum", Sys.Date()),
        uiOutput("accountSelectReport"),
        DTOutput("statementsTable")
      ),
    )
  )
