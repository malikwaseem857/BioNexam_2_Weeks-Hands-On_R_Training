#Exploratory Data Visualization
library(dslabs)
View(murders)
library(tidyr)


#Scatter-plot of total murders vs population
x <- murders $ population/10^6
y <- murders $ total

plot (x,y)

#Histogram of murder rate
library(dplyr)
data(murders)
new_table <- mutate (murders, rate = total/population*10^5)
View(new_table)

hist (new_table $ rate)

#boxplot of murder rate by region
boxplot(x,y)


boxplot(rate~region, data=new_table)




#Base R graphics: Fast & built-in
# Load standard dataset
data(murders, package = "dslabs")

# Basic Scatter Plot
plot(murders$population / 1e6, murders$total,
     main = "US Gun Murders by State",
     xlab = "Population (Millions)",
     ylab = "Total Murders",
     pch = 19,           # Solid points
     col = "orange",   # Point color
     font.lab = 2)       # Bold axis labels

# Add a trendline
abline(lm(total ~ I(population/1e6), data = murders), col = "red", lwd = 2)




# lets visualize by ggplot2 
# there are different layers in ggplot2

library(ggplot2)
library(dslabs)

# Layer 1: Data & Aesthetic Mapping
ggplot(data = murders, aes(x = population / 1e6, y = total, color = region)) +
  # Layer 2: Geometric Object (Scatter points)
  geom_point(size = 3) +
  # Layer 3: Labels
  labs(
    title = "US Gun Murders by Region",
    subtitle = "Data source: dslabs murders dataset",
    x = "Population (in Millions)",
    y = "Total Murders",
    color = "US Region"
  ) +
  # Layer 4: Theme formatting
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# how to visualize the data that is in  Excel sheet
# furst install package for excel
install.packages("readxl")
library(readxl)
library(ggplot2)

# 1. Load the Excel data
install.packages("readr")
library(readr)

lab_data <- read_csv("laboratory__data.csv")



# without R packagge
lab_data <- read.csv("laboratory__data.csv", stringsAsFactors = FALSE)













####  Option 1 Check the columns loaded cleanly####
head(lab_data)


View(lab_data)

str(lab_data) #to check variable types

# Load the ggplot2 package for data visualization
library(ggplot2)



theme_minimal() +
  theme(legend.position = "bottom")
# Create a histogram showing the Age distribution,
# with bars colored/filled according to the Disease group
p <- ggplot(lab_data, aes(x = Age, fill = Disease)) +
  
  # Add histogram layer:
  #   binwidth = 5     -> each bar represents a 5-year age range
  #   color = "white"  -> white outline around each bar for clarity
  #   alpha = 0.6      -> semi-transparency so overlapping groups remain visible
  #   position = "identity" -> bars are drawn on top of each other (not stacked)
  geom_histogram(binwidth = 5, color = "white", alpha = 0.6,
                 position = "identity") +
  
  # Add titles and axis labels for a publication-ready look
  labs(
    title    = "Age Distribution by Disease",              # Main title
    subtitle = "Lahore Lab Sample Data (N = 12,009)",      # Subtitle with sample size
    x        = "Patient Age (Years)",                      # X-axis label
    y        = "Number of Patients",                       # Y-axis label
    fill     = "Disease"                                   # Legend title
  ) +
  
  # Apply a clean, minimal theme (removes grey background and gridlines)
  theme_minimal()
####### 2. Save as PNG
ggsave("age_distribution_by_disease.png",
       plot   = p,
       width  = 10,
       height = 6,
       dpi    = 300,
       bg     = "white")

##### Option 2: Age Distribution grouped by Gender (cleaner, only 2 groups)####

p<- ggplot(lab_data, aes(x = Age, fill = Gender)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.7,
                 position = "identity") +
  scale_fill_manual(values = c("Male" = "#5BC0DE", "Female" = "#D9534F")) +
  labs(
    title = "Age Distribution by Gender",
    subtitle = "Lahore Lab Sample Data (N = 12,009)",
    x = "Patient Age (Years)",
    y = "Number of Patients",
    fill = "Gender"
  ) +
  theme_minimal()

### save it in png form
ggsave("age_distribution_by_gender.png",
       plot   = p,
       width  = 10,
       height = 6,
       dpi    = 300,
       bg     = "white")

##### Option 3: Faceted histogram — one panel per Disease (best for many groups)####

p <- ggplot(lab_data, aes(x = Age, fill = Disease)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.85) +
  facet_wrap(~ Disease, scales = "free_y") +
  labs(
    title = "Faceted histogram of all the diease",
    subtitle = "Lahore Lab Sample Data (N = 12,009)",
    x = "Patient Age (Years)",
    y = "Number of Patients"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#  save this plot
ggsave("Faceted histogram of all the diease.png",
       plot   = p,
       width  = 10,
       height = 6,
       dpi    = 300,
       bg     = "white")


#### Option 4: Density plot instead (smoother for large N)####
p <- ggplot(lab_data, aes(x = Age, fill = Disease, color = Disease)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Age Distribution by Disease",
    subtitle = "Lahore Lab Sample Data (N = 12,009)",
    x = "Patient Age (Years)",
    y = "Density"
  ) +
  theme_minimal()
#  save this
ggsave("age_distribution_by_disease.png",
       plot   = p,
       width  = 10,
       height = 6,
       dpi    = 300,
       bg     = "white")





#### 1. Load the file####
lab_data <- read.csv("laboratory__data.csv", stringsAsFactors = FALSE)

# 2. Reshape: pivot everything EXCEPT Gender, Age, Disease
long_lab_data <- lab_data %>%
  pivot_longer(
    cols = -c(Gender, Age, Disease),
    names_to  = "Test",
    values_to = "Value"
  )

# Optional: clean up the ugly dotted names for nicer facet labels
long_lab_data <- long_lab_data %>%
  mutate(Test = recode(Test,
                       "AST..aspartate.aminotransferase." = "AST",
                       "ALT..alanine.aminotransferase."   = "ALT"
  ))

# 3. Comparison plot
p <- ggplot(long_lab_data, aes(x = Disease, y = Value, fill = Disease)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  facet_wrap(~ Test, scales = "free_y", ncol = 3) +
  labs(
    title    = "Distribution of Lab Values Across Diseases",
    subtitle = "Lahore Lab Dataset (N = 12,009)",
    x        = "Disease",
    y        = "Measured Value",
    fill     = "Disease"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
#  save this
ggsave("comparsion_plot.png",
       plot   = p,
       width  = 10,
       height = 6,
       dpi    = 300,
       bg     = "white")



# 1. Create Age Groups from lab_data (the wide, original data)
positive_cases <- lab_data %>%
  mutate(Age_Group = cut(Age,
                         breaks = c(0, 20, 35, 50, 70, Inf),
                         labels = c("<20", "20-34", "35-49", "50-69", "70+" ),
                         right  = FALSE))

# 2. Plot counts by Age Group, dodged by Disease
p <- ggplot(positive_cases, aes(x = Age_Group, fill = Disease)) +
  geom_bar(position = "dodge") +
  labs(
    title    = "Disease Diagnoses by Age Category",
    subtitle = "Lahore Lab Sample Data (N = 12,009)",
    x        = "Age Range (Years)",
    y        = "Number of Cases",
    fill     = "Disease"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# save in png form 
P <-  ggsave("disease_by_age_600dpi.png", plot = p,
       width = 10, height = 6, dpi = 600, bg = "white")
