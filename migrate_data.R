library(openxlsx)
library(readxl)
library(dplyr)

input_file <- "backup_2026-01-08.xlsx"
output_file <- "backup_2026-01-08_migrated.xlsx"

setwd(
  "C:/Users/sile9/Documents/projects/kassenbuch/.claude/worktrees/elated-kalam"
)

# 1. Einlesen
topics <- read_excel(input_file, sheet = "Anlässe")
accounts <- read_excel(input_file, sheet = "Konten")
trans <- read_excel(input_file, sheet = "Buchungen")

# 2. Minusbeträge: Sollkonto und Habenkonto tauschen, Betrag positiv machen
trans <- trans |>
  mutate(
    Sollkonto_neu = ifelse(Betrag < 0, Habenkonto, Sollkonto),
    Habenkonto_neu = ifelse(Betrag < 0, Sollkonto, Habenkonto),
    Betrag = abs(Betrag)
  ) |>
  select(-Sollkonto, -Habenkonto) |>
  rename(Sollkonto = Sollkonto_neu, Habenkonto = Habenkonto_neu) |>
  select(Datum, Betrag, Bemerkung, Sollkonto, Habenkonto)

# 3. Speichern
wb <- createWorkbook()
addWorksheet(wb, "Anlässe")
writeData(wb, "Anlässe", topics)
addWorksheet(wb, "Konten")
writeData(wb, "Konten", accounts)
addWorksheet(wb, "Buchungen")
writeData(wb, "Buchungen", trans)
saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Fertig.", nrow(trans), "Buchungen gespeichert in", output_file, "\n")
