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
        topics_data <- data.frame(Anlass = character(), stringsAsFactors = FALSE)
      }

      if ("Konten" %in% sheets) {
        accounts_data <- read_excel(data_file, sheet = "Konten")
      } else {
        accounts_data <- data.frame(Konto = character(), stringsAsFactors = FALSE)
      }

      if ("Buchungen" %in% sheets) {
        trans_data <- read_excel(data_file, sheet = "Buchungen")
        trans_data$Datum <- as.Date(trans_data$Datum, origin = "1899-12-30")
      } else {
        trans_data <- data.frame(
          Datum = as_date(character()),
          Betrag = numeric(),
          Bemerkung = character(),
          Sollkonto = character(),
          Habenkonto = character()
        )
      }
    } else {
      topics_data <- data.frame(Anlass = character(), stringsAsFactors = FALSE)
      accounts_data <- data.frame(Konto = character(), stringsAsFactors = FALSE)
      trans_data <- data.frame(
        Datum = as_date(character()),
        Betrag = numeric(),
        Bemerkung = character(),
        Sollkonto = character(),
        Habenkonto = character(),
        stringsAsFactors = FALSE
      )
    }
    list(topics = topics_data, accounts = accounts_data, transactions = trans_data)
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

  # ---- Backup data ----
  output$backupData <- downloadHandler(
    filename = function() paste0("backup_", Sys.Date(), ".xlsx"),
    content = function(file) file.copy(data_file, file, overwrite = TRUE)
  )

  # ---- Restore data ----
  observeEvent(input$restoreFile, {
    req(input$restoreFile)
    file.copy(input$restoreFile$datapath, data_file, overwrite = TRUE)
    restored <- load_data()
    topics(restored$topics)
    accounts(restored$accounts)
    transactions(restored$transactions)
  })

  # ---- Combined account choices (Konten + Anlässe) ----
  all_account_choices <- reactive({
    list(
      "Konten (Aktivkonten)" = accounts()$Konto,
      "Anlässe (Erfolgskonten)" = topics()$Anlass
    )
  })

  # ---- Report file (shared between both reports) ----
  report_file <- reactiveVal(NULL)

  # ---- Generate Topic Report ----
  output$topicSelectReport <- renderUI({
    selectInput("topicReport", "Anlass:", choices = topics()$Anlass)
  })

  observeEvent(input$generateTopicReport, {
    showModal(modalDialog(
      title = "Bitte warten...",
      "Abrechnung wird erstellt. Dies kann einige Sekunden dauern.",
      easyClose = FALSE,
      footer = NULL
    ))

    transactions_df <- transactions()
    if (is.null(transactions_df)) transactions_df <- data.frame()

    tmp_rds <- tempfile(fileext = ".rds")
    saveRDS(transactions_df, tmp_rds)

    out_name <- paste0("Abrechnung_", input$topicReport, ".pdf")

    quarto::quarto_render(
      input = "topic_report.qmd",
      output_file = out_name,
      execute_params = list(topic = input$topicReport, transactions_rds = tmp_rds)
    )

    report_file(out_name)
    unlink(tmp_rds)
    removeModal()
  })

  output$downloadTopicReport <- downloadHandler(
    filename = function() paste0("Abrechnung_", input$topicReport, ".pdf"),
    content = function(file) { req(report_file()); file.copy(report_file(), file) }
  )

  # ---- Generate Account Statement ----
  output$accountSelectReport <- renderUI({
    selectInput("accountReport", "Konto:", choices = accounts()$Konto)
  })

  observeEvent(input$generateAccountReport, {
    showModal(modalDialog(
      title = "Bitte warten...",
      "Abrechnung wird erstellt. Dies kann einige Sekunden dauern.",
      easyClose = FALSE,
      footer = NULL
    ))

    transactions_df <- transactions()
    if (is.null(transactions_df)) transactions_df <- data.frame()

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

  output$downloadAccountReport <- downloadHandler(
    filename = function() paste0("Kontoauszug_", input$accountReport, ".pdf"),
    content = function(file) { req(report_file()); file.copy(report_file(), file) }
  )

  # ---- Manage Topics ----
  observeEvent(input$addTopic, {
    if (input$newTopic != "") {
      if (!input$newTopic %in% topics()$Anlass) {
        topics(rbind(topics(), tibble(Anlass = input$newTopic)))
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

    used_in_trans <- topic_to_delete %in% c(df_trans$Sollkonto, df_trans$Habenkonto)
    last_topic <- nrow(df_topics) == 1

    if (used_in_trans) {
      showNotification(
        paste("Der Anlass", topic_to_delete, "wird in Buchungen verwendet und kann nicht gelöscht werden."),
        type = "error"
      )
    } else if (last_topic) {
      showNotification("Es muss immer mindestens ein Anlass vorhanden sein.", type = "error")
    } else {
      topics(df_topics[-sel, ])
      save_data()
      showNotification(paste("Anlass", topic_to_delete, "wurde gelöscht."), type = "message")
    }
  })

  # ---- Rename Topic ----
  observeEvent(input$renameTopic, {
    sel <- input$topicList_rows_selected
    req(sel, input$editTopicName)

    old_name <- topics()$Anlass[sel]

    df <- topics()
    df$Anlass[sel] <- input$editTopicName
    topics(df)

    trans <- transactions()
    trans$Sollkonto[trans$Sollkonto == old_name] <- input$editTopicName
    trans$Habenkonto[trans$Habenkonto == old_name] <- input$editTopicName
    transactions(trans)

    save_data()
  })

  # ---- Topic Summary ----
  # Gewinn/Verlust = Haben - Soll
  # Haben = Ertrag (Einnahmen für diesen Anlass)
  # Soll  = Aufwand (Ausgaben für diesen Anlass)
  topicSummary <- reactive({
    if (nrow(topics()) == 0) {
      return(tibble(Anlass = character(), `Gewinn/Verlust` = numeric()))
    }

    trans <- transactions()

    haben_sum <- trans |>
      filter(Habenkonto %in% topics()$Anlass) |>
      group_by(Anlass = Habenkonto) |>
      summarise(Haben = sum(Betrag), .groups = "drop")

    soll_sum <- trans |>
      filter(Sollkonto %in% topics()$Anlass) |>
      group_by(Anlass = Sollkonto) |>
      summarise(Soll = sum(Betrag), .groups = "drop")

    topics() |>
      left_join(haben_sum, by = "Anlass") |>
      left_join(soll_sum, by = "Anlass") |>
      mutate(
        Haben = replace_na(Haben, 0),
        Soll = replace_na(Soll, 0),
        `Gewinn/Verlust` = Haben - Soll
      ) |>
      select(Anlass, `Gewinn/Verlust`)
  })

  output$topicList <- renderDT(topicSummary(), options = list(dom = 't'))

  # ---- Manage Accounts ----
  observeEvent(input$addAccount, {
    if (input$newAccount != "") {
      if (!input$newAccount %in% accounts()$Konto) {
        accounts(rbind(accounts(), tibble(Konto = input$newAccount)))

        if (input$startBalance != 0) {
          # Ensure "Anfangssaldo" Anlass exists
          if (!"Anfangssaldo" %in% topics()$Anlass) {
            topics(rbind(topics(), tibble(Anlass = "Anfangssaldo")))
          }
          newTrans <- tibble(
            Datum = Sys.Date(),
            Betrag = abs(input$startBalance),
            Bemerkung = "Initialbuchung",
            Sollkonto = if (input$startBalance > 0) input$newAccount else "Anfangssaldo",
            Habenkonto = if (input$startBalance > 0) "Anfangssaldo" else input$newAccount
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

    used_in_trans <- account_to_delete %in% c(df_trans$Sollkonto, df_trans$Habenkonto)
    last_account <- nrow(df_accounts) == 1

    if (used_in_trans) {
      showNotification(
        paste("Das Konto", account_to_delete, "wird in Buchungen verwendet und kann nicht gelöscht werden."),
        type = "error"
      )
    } else if (last_account) {
      showNotification("Es muss immer mindestens ein Konto vorhanden sein.", type = "error")
    } else {
      accounts(df_accounts[-sel, ])
      save_data()
      showNotification(paste("Konto", account_to_delete, "wurde gelöscht."), type = "message")
    }
  })

  # ---- Rename Account ----
  observeEvent(input$renameAccount, {
    sel <- input$accountList_rows_selected
    req(sel, input$editAccountName)

    old_name <- accounts()$Konto[sel]

    df <- accounts()
    df$Konto[sel] <- input$editAccountName
    accounts(df)

    trans <- transactions()
    trans$Sollkonto[trans$Sollkonto == old_name] <- input$editAccountName
    trans$Habenkonto[trans$Habenkonto == old_name] <- input$editAccountName
    transactions(trans)

    save_data()
  })

  # ---- Account Summary ----
  # Saldo = Soll - Haben
  # Soll  = Zugang  (Aktivkonto nimmt zu)
  # Haben = Abgang  (Aktivkonto nimmt ab)
  accountSummary <- reactive({
    if (nrow(accounts()) == 0) {
      return(tibble(Konto = character(), Saldo = numeric()))
    }

    trans <- transactions()

    soll_sum <- trans |>
      filter(Sollkonto %in% accounts()$Konto) |>
      group_by(Konto = Sollkonto) |>
      summarise(Soll = sum(Betrag), .groups = "drop")

    haben_sum <- trans |>
      filter(Habenkonto %in% accounts()$Konto) |>
      group_by(Konto = Habenkonto) |>
      summarise(Haben = sum(Betrag), .groups = "drop")

    accounts() |>
      left_join(soll_sum, by = "Konto") |>
      left_join(haben_sum, by = "Konto") |>
      mutate(
        Soll = replace_na(Soll, 0),
        Haben = replace_na(Haben, 0),
        Saldo = Soll - Haben
      ) |>
      select(Konto, Saldo)
  })

  output$accountList <- renderDT(accountSummary(), options = list(dom = 't'))

  # ---- Add Transaction ----
  output$sollkontoSelect <- renderUI({
    selectInput("sollkonto", "Sollkonto:", choices = all_account_choices())
  })

  output$habenkontoSelect <- renderUI({
    selectInput("habenkonto", "Habenkonto:", choices = all_account_choices())
  })

  observeEvent(input$addTrans, {
    if (!is.null(input$sollkonto) && !is.null(input$habenkonto) && input$amount > 0) {
      transactions(rbind(
        transactions(),
        tibble(
          Datum = input$date,
          Betrag = input$amount,
          Bemerkung = input$note,
          Sollkonto = input$sollkonto,
          Habenkonto = input$habenkonto
        )
      ))
      save_data()
    }
  })

  # ---- Delete Transaction ----
  observeEvent(input$deleteTrans, {
    sel <- input$transTable_rows_selected
    req(sel)
    transactions(transactions()[-sel, ])
    save_data()
  })

  # ---- Edit Transaction ----
  output$editSollkontoSelect <- renderUI({
    selectInput("editSollkonto", "Sollkonto:", choices = all_account_choices())
  })

  output$editHabenkontoSelect <- renderUI({
    selectInput("editHabenkonto", "Habenkonto:", choices = all_account_choices())
  })

  observeEvent(input$editTrans, {
    sel <- input$transTable_rows_selected
    req(sel)

    df <- transactions()

    if (!is.null(input$editDate) && !is.na(input$editDate)) {
      df$Datum[sel] <- as.Date(input$editDate)
    }
    if (!is.null(input$editAmount) && input$editAmount > 0) {
      df$Betrag[sel] <- input$editAmount
    }
    if (!is.null(input$editNote) && input$editNote != "") {
      df$Bemerkung[sel] <- input$editNote
    }
    if (!is.null(input$editSollkonto) && input$editSollkonto != "") {
      df$Sollkonto[sel] <- input$editSollkonto
    }
    if (!is.null(input$editHabenkonto) && input$editHabenkonto != "") {
      df$Habenkonto[sel] <- input$editHabenkonto
    }

    transactions(df)
    save_data()

    showNotification("Buchung wurde erfolgreich bearbeitet.", type = "message")
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
    removeModal()

    empty_topics <- tibble(Anlass = character())
    empty_accounts <- tibble(Konto = character())
    empty_trans <- tibble(
      Datum = as_date(character()),
      Betrag = numeric(),
      Bemerkung = character(),
      Sollkonto = character(),
      Habenkonto = character()
    )

    topics(empty_topics)
    accounts(empty_accounts)
    transactions(empty_trans)

    wb <- createWorkbook()
    addWorksheet(wb, "Anlässe")
    writeData(wb, "Anlässe", empty_topics)
    addWorksheet(wb, "Konten")
    writeData(wb, "Konten", empty_accounts)
    addWorksheet(wb, "Buchungen")
    writeData(wb, "Buchungen", empty_trans)
    saveWorkbook(wb, data_file, overwrite = TRUE)

    showNotification("Alle Daten wurden gelöscht und das Dashboard wurde zurückgesetzt.", type = "message")
  })
}
