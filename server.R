library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(openxlsx)

# Hauptdatei für Daten
data_file <- "finance_data.xlsx"

server <- function(input, output, session) {
  # ---- Load data from Excel workbook if exists ----
  load_data <- function() {
    if (file.exists(data_file)) {
      sheets <- getSheetNames(data_file)

      if ("Anlässe" %in% sheets) {
        topics_data <- read.xlsx(data_file, sheet = "Anlässe")
      } else {
        topics_data <- data.frame(
          Anlass = character(),
          stringsAsFactors = FALSE
        )
      }

      if ("Konten" %in% sheets) {
        accounts_data <- read.xlsx(data_file, sheet = "Konten")
      } else {
        accounts_data <- data.frame(
          Konto = character(),
          stringsAsFactors = FALSE
        )
      }

      if ("Buchungen" %in% sheets) {
        trans_data <- read.xlsx(data_file, sheet = "Buchungen")
        trans_data$Datum <- as.Date(trans_data$Datum)
      } else {
        trans_data <- data.frame(
          Datum = as.Date(character()),
          Betrag = numeric(),
          Bemerkung = character(),
          Konto = character(),
          Anlass = character(),
          stringsAsFactors = FALSE
        )
      }
    } else {
      topics_data <- data.frame(
        Anlass = character(),
        stringsAsFactors = FALSE
      )
      accounts_data <- data.frame(
        Konto = character(),
        stringsAsFactors = FALSE
      )
      trans_data <- data.frame(
        Datum = as.Date(character()),
        Betrag = numeric(),
        Bemerkung = character(),
        Konto = character(),
        Anlass = character(),
        stringsAsFactors = FALSE
      )
    }
    list(
      topics = topics_data,
      accounts = accounts_data,
      transactions = trans_data
    )
  }

  # Initialize data
  init_data <- load_data()
  topics <- reactiveVal(init_data$topics)
  accounts <- reactiveVal(init_data$accounts)
  transactions <- reactiveVal(init_data$transactions)

  # ---- Save everything back into Excel workbook ----
  save_data <- function() {
    wb <- createWorkbook()
    addWorksheet(wb, "Anlässe")
    writeData(wb, "Anlässe", topics())

    addWorksheet(wb, "Konten")
    writeData(wb, "Konten", accounts())

    addWorksheet(wb, "Buchungen")
    writeData(wb, "Buchungen", transactions())

    saveWorkbook(wb, data_file, overwrite = TRUE)
  }

  # ---- Manage Topics ----
  observeEvent(input$addTopic, {
    if (input$newTopic != "") {
      already_exists <- input$newTopic %in% topics()$Anlass

      if (!already_exists) {
        newdf <- rbind(
          topics(),
          data.frame(Anlass = input$newTopic)
        )
        topics(newdf)
        save_data()
      }
    }
  })

  topicSummary <- reactive({
    if (nrow(topics()) == 0) {
      return(NULL)
    }

    # Summarise transactions per account
    trans_summary <- transactions() %>%
      group_by(Anlass) %>%
      summarise("Gewinn/Verlust" = sum(Betrag), .groups = "drop")

    # Join with accounts list (to show even empty accounts)
    result <- topics() %>%
      left_join(trans_summary, by = c("Anlass" = "Anlass")) %>%
      mutate(
        `Gewinn/Verlust` = replace_na(`Gewinn/Verlust`, 0)
      )

    result
  })

  output$topicList <- renderDT(topicSummary(), options = list(dom = 't'))

  # ---- Manage Accounts ----
  observeEvent(input$addAccount, {
    if (input$newAccount != "") {
      already_exists <- input$newAccount %in% accounts()$Konto

      if (!already_exists) {
        # add account
        newdf <- rbind(
          accounts(),
          data.frame(Konto = input$newAccount)
        )
        accounts(newdf)

        # add starting balance as a booking if != 0
        if (input$startBalance != 0) {
          newTrans <- data.frame(
            Datum = Sys.Date(),
            Betrag = input$startBalance,
            Bemerkung = "Initialbuchung",
            Konto = input$newAccount,
            Anlass = "Anfangssaldo"
          )
          transactions(rbind(transactions(), newTrans))
        }

        save_data()
      }
    }
  })

  accountSummary <- reactive({
    if (nrow(accounts()) == 0) {
      return(NULL)
    }

    # Summarise transactions per account
    trans_summary <- transactions() %>%
      group_by(Konto) %>%
      summarise(Saldo = sum(Betrag), .groups = "drop")

    # Join with accounts list (to show even empty accounts)
    result <- accounts() %>%
      left_join(trans_summary, by = c("Konto" = "Konto")) %>%
      mutate(
        Saldo = replace_na(Saldo, 0)
      )

    result
  })

  output$accountList <- renderDT(accountSummary(), options = list(dom = 't'))

  # Topic select for transactions
  output$topicSelect <- renderUI({
    selectInput("topic", "Anlass:", choices = topics()$Anlass)
  })

  # Account select for transactions
  output$accountSelect <- renderUI({
    selectInput("account", "Konto:", choices = accounts()$Konto)
  })

  # ---- Add Transaction ----
  observeEvent(input$addTrans, {
    if (!is.null(input$topic) && !is.null(input$account) && input$amount != 0) {
      newdf <- rbind(
        transactions(),
        data.frame(
          Datum = input$date,
          Betrag = input$amount,
          Bemerkung = input$note,
          Konto = input$account,
          Anlass = input$topic
        )
      )
      transactions(newdf)
      save_data()
    }
  })

  output$transTable <- renderDT(transactions())

  # ---- Backup data ----
  output$backupData <- downloadHandler(
    filename = function() {
      paste0("backup_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      file.copy(data_file, file, overwrite = TRUE)
    }
  )

  # ---- Restore data ----
  observeEvent(input$restoreFile, {
    req(input$restoreFile)
    file.copy(input$restoreFile$datapath, data_file, overwrite = TRUE)

    # Reload after restore
    restored <- load_data()
    topics(restored$topics)
    transactions(restored$transactions)
  })

  # ---- Topic Report ----
  output$topicSelectReport <- renderUI({
    req(topics())

    valid_topics <- topics() %>%
      filter(Anlass != "Anfangssaldo") %>%
      pull(Anlass)

    selectInput(
      "topicReport",
      "Anlass:",
      choices = valid_topics
    )
  })

  reportData <- reactive({
    req(transactions(), input$topicReport)

    transactions() %>%
      filter(Anlass == input$topicReport) %>%
      arrange(desc(Betrag))
  })

  output$reportTable <- renderDT({
    reportData()
  })

  # ---- Account Statements ----
  output$accountSelectReport <- renderUI({
    req(accounts())
    selectInput(
      "accountReport",
      "Konto:",
      choices = accounts()$Konto
    )
  })

  reportData <- reactive({
    req(transactions(), input$accountReport)

    transactions() %>%
      filter(
        Datum >= input$startDate,
        Datum <= input$endDate,
        Konto == input$accountReport
      ) %>%
      arrange(asc(Datum))
  })

  # ---- Show in DT ----
  output$statementsTable <- renderDT({
    reportData()
  })
}
