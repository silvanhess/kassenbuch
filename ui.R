library(shiny)
library(DT)

ui <-
  fluidPage(
    titlePanel("Kassenbuch Jugend Ridu"),
    tabsetPanel(
      tabPanel(
        "Übersicht",
        fluidRow(
          # column(
          #   6,
          #   h3("Aktuelle Salden"),
          #   verbatimTextOutput("saldoCash", ),
          #   verbatimTextOutput("saldoBank")
          # ),
          column(
            6,
            h3("Export / Sicherung"),
            # dateInput("compareDate1", "Beginndatum", Sys.Date() - 365),
            # dateInput("compareDate2", "Endedatum", Sys.Date()),
            # downloadButton("downloadExcel", "Berichte herunterladen (Excel)"),
            # br(),
            downloadButton("backupData", "Daten sichern"),
            fileInput(
              "restoreFile",
              "Daten aus Sicherung wiederherstellen",
              accept = c(".xlsx")
            )
          )
        ) #,
        # fluidRow(
        #   column(12, h3("Auswertung nach Themen"), DTOutput("topicTable"))
        # ),
        # fluidRow(
        #   column(
        #     12,
        #     h3("Auswertung nach Typ (Bar / Bank)"),
        #     DTOutput("typeTable")
        #   )
        # )
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
        "Abrechnung erstellen",
        dateInput("startDate", "Beginndatum", Sys.Date() - 365),
        dateInput("endDate", "Enddatum", Sys.Date()),
        uiOutput("topicSelectReport"),
        uiOutput("accountSelectReport"),
        downloadButton("downloadReport", "Abrechnung herunterladen (Excel"),
        DTOutput("reportTable")
      )
    )
  )

# dateInput("compareDate1", "Beginndatum", Sys.Date() - 365),
# dateInput("compareDate2", "Endedatum", Sys.Date()),
# downloadButton("downloadExcel", "Berichte herunterladen (Excel)"),
