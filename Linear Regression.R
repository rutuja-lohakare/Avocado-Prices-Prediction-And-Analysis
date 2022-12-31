library(sjPlot)
library(ggfortify)
library(readxl)
library(dplyr)

# Import the dataset.
avocado <- read_excel("avocado.xlsx")
data = avocado

conmod <- data %>%
  filter(type == "conventional") %>%
  mutate(Volume = log(`total_volume`)) %>%
  lm(average_price ~ Volume, data = .)

orgmod <- data %>%
  filter(type == "organic") %>%
  mutate(Volume = log(`total_volume`)) %>%
  lm(average_price ~ Volume, data = .)

# Printing the output in a table rather than in the console.
tab_model(conmod, orgmod)

#Regression diagnostics.
autoplot(conmod)
autoplot(orgmod)