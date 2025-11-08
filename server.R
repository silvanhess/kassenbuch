library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)
library(readxl)
library(formattable)

# Hauptdatei für Daten
data_file <- "finance_data.xlsx"

server <- function(input, output, session) {
  # ---- Load data from Excel workbook if exists ----
  load_data <- function() {
    if (file.exists(data_file)) {
      sheets <- getSheetNames(data_file)

      if ("Anlässe" %in% sheets) {
        topics_data <- read_excel(data_file, sheet = "Anlässe")
      } else {
        topics_data <- data.frame(
          Anlass = character(),
          stringsAsFactors = FALSE
        )
      }

      if ("Konten" %in% sheets) {
        accounts_data <- read_excel(data_file, sheet = "Konten")
      } else {
        accounts_data <- data.frame(
          Konto = character(),
          stringsAsFactors = FALSE
        )
      }

      if ("Buchungen" %in% sheets) {
        trans_data <- read_excel(
          data_file,
          sheet = "Buchungen"
        )
        trans_data$Datum <- as.Date(trans_data$Datum, origin = "1899-12-30")
      } else {
        trans_data <- data.frame(
          Datum = as_date(character()),
          Betrag = numeric(),
          # Betrag = currency(numeric(), "Fr."),
          Bemerkung = character(),
          Konto = character(),
          Anlass = character()
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
        Datum = as_date(character()),
        Betrag = numeric(),
        # Betrag = currency(numeric(), "Fr."),
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

  print(str(init_data$transactions))
  print(class(init_data$transactions$Datum))

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
    accounts(restored$accounts)
    transactions(restored$transactions)
  })

  # ---- Generate Topic Report ----
  output$topicSelectReport <- renderUI({
    selectInput("topicReport", "Anlass:", choices = topics()$Anlass)
  })

  report_file <- reactiveVal(NULL)

  observeEvent(input$generateTopicReport, {
    showModal(modalDialog(
      title = "Bitte warten...",
      "Abrechnung wird erstellt. Dies kann einige Sekunden dauern.",
      easyClose = FALSE,
      footer = NULL
    ))

    transactions_df <- transactions()
    if (is.null(transactions_df)) {
      transactions_df <- data.frame()
    }

    tmp_rds <- tempfile(fileext = ".rds")
    saveRDS(transactions_df, tmp_rds)

    out_name <- paste0("Abrechnung_", input$topicReport, ".pdf")
    # out_path <- file.path(tempdir(), out_name)

    quarto::quarto_render(
      input = "topic_report.qmd",
      output_file = out_name,
      execute_params = list(
        topic = input$topicReport,
        transactions_rds = tmp_rds
      )
    )

    report_file(out_name)
    unlink(tmp_rds)
    removeModal()
  })

  # Serve the report file for download
  output$downloadTopicReport <- downloadHandler(
    filename = function() {
      paste0("Abrechnung_", input$topicReport, ".pdf")
    },
    content = function(file) {
      req(report_file())
      file.copy(report_file(), file)
    }
  )

  # ---- Generate Account Statement ----
  output$accountSelectReport <- renderUI({
    selectInput("accountReport", "Konto:", choices = accounts()$Konto)
  })

  report_file <- reactiveVal(NULL)

  observeEvent(input$generateAccountReport, {
    showModal(modalDialog(
      title = "Bitte warten...",
      "Abrechnung wird erstellt. Dies kann einige Sekunden dauern.",
      easyClose = FALSE,
      footer = NULL
    ))

    transactions_df <- transactions()
    if (is.null(transactions_df)) {
      transactions_df <- data.frame()
    }

    tmp_rds <- tempfile(fileext = ".rds")
    saveRDS(transactions_df, tmp_rds)

    out_name <- paste0("Kontoauszug_", input$accountReport, ".pdf")

    quarto::quarto_render(
      input = "account_statement.qmd",
      output_file = out_name,
      execute_params = list(
        account = input$accountReport,
        startDate = input$startDate,
        endDate = input$endDate,
        transactions_rds = tmp_rds
      )
    )

    report_file(out_name)
    unlink(tmp_rds)
    removeModal()
  })

  # Serve the report file for download
  output$downloadAccountReport <- downloadHandler(
    filename = function() {
      paste0("Kontoauszug_", input$accountReport, ".pdf")
    },
    content = function(file) {
      req(report_file())
      file.copy(report_file(), file)
    }
  )

  # ---- Manage Topics ----
  observeEvent(input$addTopic, {
    if (input$newTopic != "") {
      already_exists <- input$newTopic %in% topics()$Anlass

      if (!already_exists) {
        newdf <- rbind(
          topics(),
          tibble(Anlass = input$newTopic)
        )
        topics(newdf)
        save_data()
      }
    }
  })

  # ---- Delete Topic ----
  observeEvent(input$deleteTopic, {
    sel <- input$topicList_rows_selected
    req(sel)

    df_topics <- topics()
    df_trans <- transactions()

    topic_to_delete <- df_topics$Anlass[sel]

    # Check if the topic exists in transactions
    used_in_trans <- topic_to_delete %in% df_trans$Anlass

    # Check if it is the last remaining topic
    last_topic <- nrow(df_topics) == 1

    if (used_in_trans) {
      showNotification(
        paste(
          "Der Anlass",
          topic_to_delete,
          "wird in Buchungen verwendet und kann nicht gelöscht werden."
        ),
        type = "error"
      )
    } else if (last_topic) {
      showNotification(
        print("Es muss immer mindestens ein Konto vorhanden sein."),
        type = "error"
      )
    } else {
      newdf <- df_topics[-sel, ]
      topics(newdf)
      save_data()
      showNotification(
        paste("Anlass", topic_to_delete, "wurde gelöscht."),
        type = "message"
      )
    }
  })

  # ---- Edit Topic ----
  observeEvent(input$renameTopic, {
    sel <- input$topicList_rows_selected
    req(sel, input$editTopicName)

    df <- topics()
    df$Anlass[sel] <- input$editTopicName
    topics(df)
    save_data()
  })

  # ---- Topic Summary ----
  topicSummary <- reactive({
    if (nrow(topics()) == 0) {
      return(NULL)
    }

    # Summarise transactions per account
    trans_summary <- transactions() |>
      group_by(Anlass) |>
      summarise("Gewinn/Verlust" = sum(Betrag), .groups = "drop")

    # Join with accounts list (to show even empty accounts)
    result <- topics() |>
      left_join(trans_summary, by = c("Anlass" = "Anlass")) |>
      mutate(
        `Gewinn/Verlust` = replace_na(`Gewinn/Verlust`, 0)
      )

    result
  })

  output$topicList <- renderDT(
    topicSummary(),
    options = list(dom = 't')
  )

  # ---- Manage Accounts ----
  observeEvent(input$addAccount, {
    if (input$newAccount != "") {
      already_exists <- input$newAccount %in% accounts()$Konto

      if (!already_exists) {
        newdf <- rbind(
          accounts(),
          tibble(Konto = input$newAccount)
        )
        accounts(newdf)

        # add starting balance as a booking if != 0
        if (input$startBalance != 0) {
          newTrans <- tibble(
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

  # ---- Delete Account ----
  observeEvent(input$deleteAccount, {
    sel <- input$accountList_rows_selected
    req(sel)

    df_accounts <- accounts()
    df_trans <- transactions()

    account_to_delete <- df_accounts$Konto[sel]

    # Check if the account exists in transactions
    used_in_trans <- account_to_delete %in% df_trans$Konto

    # Check if this is the last remaining account
    last_account <- nrow(df_accounts) == 1

    if (used_in_trans) {
      showNotification(
        paste(
          "Das Konto",
          account_to_delete,
          "wird in Buchungen verwendet und kann nicht gelöscht werden."
        ),
        type = "error"
      )
    } else if (last_acount) {
      showNotification(
        print("Es muss immer mindestens ein Konto vorhanden sein."),
        type = "error"
      )
    } else {
      newdf <- df_accounts[-sel, ]
      accounts(newdf)
      save_data()
      showNotification(
        paste("Konto", account_to_delete, "wurde gelöscht."),
        type = "message"
      )
    }
  })

  # ---- Edit Account ----
  observeEvent(input$renameAccount, {
    sel <- input$accountList_rows_selected
    req(sel, input$editAccountName)

    df <- accounts()
    df$Konto[sel] <- input$editAccountName
    accounts(df)
    save_data()
  })

  # ---- Account Summary----
  accountSummary <- reactive({
    if (nrow(accounts()) == 0) {
      return(tibble(Konto = character(), Saldo = numeric()))
    }

    # Summarise transactions per account
    trans_summary <- transactions() |>
      group_by(Konto) |>
      summarise(Saldo = sum(Betrag), .groups = "drop")

    result <- accounts() |>
      left_join(trans_summary, by = c("Konto" = "Konto")) |>
      mutate(
        Saldo = replace_na(Saldo, 0)
      )

    result
  })

  output$accountList <- renderDT(
    accountSummary(),
    options = list(dom = 't')
  )

  # ---- Add Transaction ----
  output$topicSelect <- renderUI({
    selectInput("topic", "Anlass:", choices = topics()$Anlass)
  })

  output$accountSelect <- renderUI({
    selectInput("account", "Konto:", choices = accounts()$Konto)
  })

  observeEvent(input$addTrans, {
    if (!is.null(input$topic) && !is.null(input$account) && input$amount != 0) {
      newdf <- rbind(
        transactions(),
        tibble(
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

  # ---- Delete Transaction ----
  observeEvent(input$deleteTrans, {
    sel <- input$transTable_rows_selected
    req(sel)
    newdf <- transactions()[-sel, ]
    transactions(newdf)
    save_data()
  })

  # ---- Edit Transaction ----
  output$editAccountSelect <- renderUI({
    selectInput("editAccount", "Neues Konto:", choices = accounts()$Konto)
  })

  output$editTopicSelect <- renderUI({
    selectInput("editTopic", "Neuer Anlass:", choices = topics()$Anlass)
  })

  observeEvent(input$editTrans, {
    sel <- input$transTable_rows_selected
    req(sel)

    df <- transactions()

    # ---- Update editable fields safely ----
    if (!is.null(input$editDate) && !is.na(input$editDate)) {
      df$Datum[sel] <- as.Date(input$editDate)
    }
    if (!is.null(input$editAmount) && input$editAmount != 0) {
      df$Betrag[sel] <- input$editAmount
    }
    if (!is.null(input$editNote) && input$editNote != "") {
      df$Bemerkung[sel] <- input$editNote
    }
    if (!is.null(input$editAccount) && input$editAccount != "") {
      df$Konto[sel] <- input$editAccount
    }
    if (!is.null(input$editTopic) && input$editTopic != "") {
      df$Anlass[sel] <- input$editTopic
    }

    transactions(df)
    save_data()

    showNotification(
      paste("Buchung", sel, "wurde erfolgreich bearbeitet."),
      type = "message"
    )
  })

  output$transTable <- renderDT(transactions())

  # ---- Reset all data ----
  observeEvent(input$resetData, {
    showModal(modalDialog(
      title = "Bestätigung erforderlich",
      "Möchten Sie wirklich alle Daten löschen? Diese Aktion kann nicht rückgängig gemacht werden.",
      footer = tagList(
        modalButton("Abbrechen"),
        actionButton("confirmReset", "Ja, alles löschen", class = "btn-danger")
      )
    ))
  })

  # ---- Confirm reset ----
  observeEvent(input$confirmReset, {
    removeModal() # close confirmation dialog

    # Empty data frames
    empty_topics <- tibble(Anlass = character(), stringsAsFactors = FALSE)
    empty_accounts <- tibble(Konto = character(), stringsAsFactors = FALSE)
    empty_trans <- tibble(
      Datum = as_date(character()),
      Betrag = numeric(),
      Bemerkung = character(),
      Konto = character(),
      Anlass = character(),
      stringsAsFactors = FALSE
    )

    # Reset reactive values
    topics(empty_topics)
    accounts(empty_accounts)
    transactions(empty_trans)

    # Save empty workbook
    wb <- createWorkbook()
    addWorksheet(wb, "Anlässe")
    writeData(wb, "Anlässe", empty_topics)

    addWorksheet(wb, "Konten")
    writeData(wb, "Konten", empty_accounts)

    addWorksheet(wb, "Buchungen")
    writeData(wb, "Buchungen", empty_trans)

    saveWorkbook(wb, data_file, overwrite = TRUE)

    showNotification(
      "Alle Daten wurden gelöscht und das Dashboard wurde zurückgesetzt.",
      type = "message"
    )
  })
}
