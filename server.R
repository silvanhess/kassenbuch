library(shiny)
library(DT)
library(dplyr)
library(openxlsx)

# Main Excel data file
data_file <- "finance_data.xlsx"

server <- function(input, output, session) {
  # ---- Load data from Excel workbook if exists ----
  load_data <- function() {
    if (file.exists(data_file)) {
      sheets <- getSheetNames(data_file)

      if ("Topics" %in% sheets) {
        topics_data <- read.xlsx(data_file, sheet = "Topics")
      } else {
        topics_data <- data.frame(Topic = character(), stringsAsFactors = FALSE)
      }

      if ("Transactions" %in% sheets) {
        trans_data <- read.xlsx(data_file, sheet = "Transactions")
        trans_data$Date <- as.Date(trans_data$Date)
      } else {
        trans_data <- data.frame(
          Date = as.Date(character()),
          Amount = numeric(),
          Type = character(),
          Topic = character(),
          stringsAsFactors = FALSE
        )
      }
    } else {
      topics_data <- data.frame(Topic = character(), stringsAsFactors = FALSE)
      trans_data <- data.frame(
        Date = as.Date(character()),
        Amount = numeric(),
        Type = character(),
        Topic = character(),
        stringsAsFactors = FALSE
      )
    }
    list(topics = topics_data, transactions = trans_data)
  }

  # Initialize data
  init_data <- load_data()
  topics <- reactiveVal(init_data$topics)
  transactions <- reactiveVal(init_data$transactions)

  # ---- Save everything back into Excel workbook ----
  save_data <- function() {
    wb <- createWorkbook()
    addWorksheet(wb, "Topics")
    writeData(wb, "Topics", topics())

    addWorksheet(wb, "Transactions")
    writeData(wb, "Transactions", transactions())

    saveWorkbook(wb, data_file, overwrite = TRUE)
  }

  # ---- Manage Topics ----
  observeEvent(input$addTopic, {
    if (input$newTopic != "") {
      newdf <- rbind(topics(), data.frame(Topic = input$newTopic))
      newdf <- unique(newdf)
      topics(newdf)
      save_data()
    }
  })

  output$topicList <- renderDT(topics(), options = list(dom = 't'))

  # Topic select for transactions
  output$topicSelect <- renderUI({
    selectInput("topic", "Topic:", choices = topics()$Topic)
  })

  # ---- Add Transaction ----
  observeEvent(input$addTrans, {
    if (!is.null(input$topic) && input$amount != 0) {
      newdf <- rbind(
        transactions(),
        data.frame(
          Date = input$date,
          Amount = input$amount,
          Type = input$type,
          Topic = input$topic
        )
      )
      transactions(newdf)
      save_data()
    }
  })

  output$transTable <- renderDT(transactions())

  # ---- Summaries ----
  topicSummary <- reactive({
    req(nrow(transactions()) > 0)
    transactions() %>%
      group_by(Topic) %>%
      summarise(
        Earnings = sum(Amount[Amount > 0]),
        Expenses = -sum(Amount[Amount < 0]),
        ProfitLoss = sum(Amount)
      )
  })

  typeSummary <- reactive({
    req(nrow(transactions()) > 0)
    transactions() %>%
      group_by(Type) %>%
      summarise(
        Earnings = sum(Amount[Amount > 0]),
        Expenses = -sum(Amount[Amount < 0]),
        ProfitLoss = sum(Amount)
      )
  })

  output$topicTable <- renderDT(topicSummary())
  output$typeTable <- renderDT(typeSummary())

  # ---- Dashboard balances ----
  output$saldoCash <- renderText({
    cashBal <- input$startCash +
      sum(transactions()$Amount[transactions()$Type == "Cash"])
    paste("Cash Balance:", cashBal)
  })

  output$saldoBank <- renderText({
    bankBal <- input$startBank +
      sum(transactions()$Amount[transactions()$Type == "Bank"])
    paste("Bank Balance:", bankBal)
  })

  # ---- Balance comparison function ----
  balanceAtDate <- function(date) {
    df <- transactions() %>% filter(Date <= date)
    cash <- input$startCash + sum(df$Amount[df$Type == "Cash"])
    bank <- input$startBank + sum(df$Amount[df$Type == "Bank"])
    data.frame(Date = as.Date(date), Cash = cash, Bank = bank)
  }

  # ---- Export Statements Excel ----
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0("finance_statements_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()

      addWorksheet(wb, "Transactions")
      writeData(wb, "Transactions", transactions())

      addWorksheet(wb, "By Topic")
      writeData(wb, "By Topic", topicSummary())

      addWorksheet(wb, "By Type")
      writeData(wb, "By Type", typeSummary())

      addWorksheet(wb, "Balance Comparison")
      balanceData <- rbind(
        balanceAtDate(input$compareDate1),
        balanceAtDate(input$compareDate2)
      )
      writeData(wb, "Balance Comparison", balanceData)

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

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
}

shinyApp(ui, server)
