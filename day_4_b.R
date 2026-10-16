# ReadXL package
install.packages("readxl")
library(readxl)
library(ggplot2)

# 1. Import the summarized Excel data
summary_data <- read_excel("lab_summary.xlsx")

# 2. Inspect the imported table
print(summary_data)

# 3. Create a bar chart with error bars
ggplot(summary_data, aes(x = Condition, y = Mean_Age, fill = Condition)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_errorbar(
    aes(
      ymin = Mean_Age - SE,  # Lower limit
      ymax = Mean_Age + SE   # Upper limit
    ),
    width = 0.2,             # Width of the top/bottom caps
    color = "black",
    linewidth = 0.8
  ) +
  labs(
    title = "Mean Patient Age by Medical Condition",
    subtitle = "Lahore Lab Summary Data (Mean ± Standard Error)",
    x = "Medical Condition",
    y = "Mean Age (Years)"
  ) +
  scale_fill_manual(values = c(
    "HCV Positive" = "#D9534F",
    "Diabetes"     = "#F0AD4E",
    "HIV Positive" = "#5BC0DE"
  )) +
  theme_minimal()







#Officer Package
install.packages("officer")
library(officer)
doc <- read_docx (file.choose())
doc_content <- docx_summary (doc)
head(doc_content)

View(doc_content)

library (dplyr)
paragraphs <- doc_content %>% filter (content_type == "paragraphs") %>%  select(text)

print (paragraphs)

table_data <- doc_content %>% filter (content_type == "table_cell") %>%  select (table_data)
print (table_data)





# Working with CSV files
install.packages("readr")
library(readr)


univ_data <- read_csv("lahore_universities.csv")




glimpse(univ_data)




head(univ_data)


View (univ_data)

















# "": inverted commas used for LITERAL TEXT or Strings. For example "Homo sapians" or "Path"

# (): function input. For example, mean(x)

# []: subsetting. For example, df[row, col]

# {}: multiline code block.



# =: equal (<-)
# ==: test equality

host <- "Human"

host == "Bat"



# &: And
lens <- 1500
host <- "Human"

lens > 1000 & host == "Human"



# |: Or
host <- "Mouse"
host == "Human" | host == "Mouse"



# Sequence generator
1:19




# Value matching in a set
"humn" %in% c("human", "sheep", "dragon")


