library(shiny)
library(DT)
library(dplyr)
library(openxlsx)

# Hauptdatei für Daten
data_file <- "finance_data.xlsx"

ui <- fluidPage(
  titlePanel("Persönlicher Finanzmanager"),
  tabsetPanel(
    tabPanel(
      "Übersicht",
      fluidRow(
        column(
          6,
          h3("Aktuelle Salden"),
          verbatimTextOutput("saldoCash"),
          verbatimTextOutput("saldoBank")
        ),
        column(
          6,
          h3("Export / Sicherung"),
          dateInput("compareDate1", "Vergleichsdatum 1", Sys.Date() - 365),
          dateInput("compareDate2", "Vergleichsdatum 2", Sys.Date()),
          downloadButton("downloadExcel", "Berichte herunterladen (Excel)"),
          br(),
          br(),
          downloadButton("backupData", "Daten sichern"),
          fileInput(
            "restoreFile",
            "Daten aus Sicherung wiederherstellen",
            accept = c(".xlsx")
          )
        )
      ),
      fluidRow(
        column(12, h3("Auswertung nach Themen"), DTOutput("topicTable"))
      ),
      fluidRow(
        column(
          12,
          h3("Auswertung nach Typ (Bar / Bank)"),
          DTOutput("typeTable")
        )
      )
    ),
    tabPanel(
      "Buchung hinzufügen",
      fluidRow(
        column(4, dateInput("date", "Datum:", Sys.Date())),
        column(4, numericInput("amount", "Betrag (+Einnahme, -Ausgabe):", 0)),
        column(4, selectInput("type", "Transaktionsart:", c("Bar", "Bank")))
      ),
      fluidRow(
        column(6, uiOutput("topicSelect")),
        column(6, actionButton("addTrans", "Buchung hinzufügen"))
      ),
      DTOutput("transTable")
    ),
    tabPanel(
      "Themen verwalten",
      textInput("newTopic", "Neues Thema"),
      actionButton("addTopic", "Thema hinzufügen"),
      DTOutput("topicList")
    ),
    tabPanel(
      "Einstellungen",
      numericInput("startCash", "Anfangssaldo Bar", 0),
      numericInput("startBank", "Anfangssaldo Bank", 0)
    )
  )
)
