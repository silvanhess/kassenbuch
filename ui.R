library(shiny)
library(DT)

ui <-
  fluidPage(
    titlePanel("Kassenbuch Jugend Ridu"),
    tabsetPanel(
      # ---- Übersicht ----
      tabPanel(
        "Übersicht",
        h3("Export / Sicherung"),
        downloadButton("backupData", "Daten sichern"),
        fileInput(
          "restoreFile",
          "Daten aus Sicherung wiederherstellen",
          accept = c(".xlsx")
        ),
        h4("Anlassabrechnung"),
        uiOutput("topicSelectReport"),
        actionButton("generateTopicReport", "Anlassabrechnung erstellen")
        # br(),
        # br(),
        # h4("Kontoauszug"),
        # uiOutput("accountSelectReport"),
        # dateInput("startDate", "Startdatum", Sys.Date() - 30),
        # dateInput("endDate", "Enddatum", Sys.Date()),
        # actionButton("generateAccountReport", "Kontoauszug erstellen")
      ),

      # ---- Topics ----
      tabPanel(
        "Anlässe verwalten",
        textInput("newTopic", "Neuer Anlass"),
        actionButton("addTopic", "Anlass hinzufügen"),
        br(),
        br(),
        DTOutput("topicList"),
        actionButton("deleteTopic", "Ausgewählten Anlass löschen"),
        br(),
        textInput("editTopicName", "Neuen Namen eingeben"),
        actionButton("editTopic", "Anlass umbenennen")
      ),

      # ---- Accounts ----
      tabPanel(
        "Konten verwalten",
        textInput("newAccount", "Neues Konto"),
        numericInput("startBalance", "Anfangssaldo", 0),
        actionButton("addAccount", "Konto hinzufügen"),
        br(),
        br(),
        DTOutput("accountList"),
        actionButton("deleteAccount", "Ausgewähltes Konto löschen"),
        br(),
        textInput("editAccountName", "Neuen Kontonamen eingeben"),
        actionButton("editAccount", "Konto umbenennen")
      ),

      # ---- Transactions ----
      tabPanel(
        "Buchungen verwalten",
        uiOutput("topicSelect"),
        uiOutput("accountSelect"),
        dateInput("date", "Datum", Sys.Date()),
        numericInput("amount", "Betrag", 0),
        textInput("note", "Bemerkung"),
        actionButton("addTrans", "Buchung hinzufügen"),
        br(),
        br(),
        DTOutput("transTable"),
        actionButton("deleteTrans", "Ausgewählte Buchung löschen"),
        br(),
        textInput("editNote", "Bemerkung bearbeiten"),
        numericInput("editAmount", "Betrag bearbeiten", 0),
        actionButton("editTrans", "Buchung aktualisieren")
      ),

      # ---- Einstellungen ----
      tabPanel(
        "Einstellungen",
        h4("Datenverwaltung"),
        actionButton("resetData", "Alle Daten löschen", class = "btn-danger"),
        br(),
        br(),
        helpText(
          "Achtung: Dadurch werden alle Anlässe, Konten und Buchungen gelöscht!"
        )
      )
    )
  )
