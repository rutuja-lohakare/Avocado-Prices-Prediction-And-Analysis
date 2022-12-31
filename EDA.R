library(dplyr)
library(ggplot2)
library(DataExplorer)
library(ggthemes)

data <- avocado
glimpse(data)
summary(data)
plot_missing(data)
sum(is.na(data))

options(repr.plot.width = 8, repr.plot.height = 4)
ggplot(
    data, 
    aes(x = average_price, fill = type)
) + 
geom_density() + 
facet_wrap(~type) + 
theme_minimal() + 
theme(
    plot.title = element_text(hjust = 0.5), 
    legend.position = "bottom"
) + 
labs(title="Avocado Price by Type") +
scale_fill_brewer(palette = "Set2")

vol_type <- data %>%
            group_by(type) %>%
            summarise(avg.vol = mean(total_volume)) %>% 
            mutate(pct=prop.table(avg.vol) * 100) 
print(vol_type)

plot_histogram(data)
plot_density(data)

avocado$year = as.factor(avocado$year)
avocado$date = as.Date(avocado$date)
avocado$month = factor(months(avocado$date), levels = month.name)

options(repr.plot.width = 7, repr.plot.height = 5)
ggplot(avocado, aes(avocado$type, avocado$average_price)) + 
geom_boxplot(aes(colour = avocado$year)) + 
labs(
    colour = "Year", 
    x = "Type", 
    y ="Average Price", 
    title = "Boxplot - Average price per year by type."
)

grouped = avocado %>% 
    group_by(year, month, type) %>% 
    select(year, month, type, average_price) %>%
    summarise(average_price = mean(average_price))

options(repr.plot.width = 12, repr.plot.height = 5)
ggplot(data = grouped, aes(x = month, y = average_price, colour = year, group = year)) +
labs(
    colour = "Year", 
    x = "Month", 
    y ="Average Price", 
    title = "Line Plot - Average monthly prices by type for every year."
) +
geom_line() + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
facet_grid(. ~grouped$type)


grouped_geography_conv = avocado %>% 
    select(year, geography, type, average_price) %>%
    filter(type == 'conventional')
min_con = round(min(grouped_geography_conv$average_price), 1) - 0.1
max_con = round(max(grouped_geography_conv$average_price), 1) + 0.1

grouped_geography_org = avocado %>% 
    select(year, geography, type, average_price) %>%
    filter(type == 'organic')

min_org = round(min(grouped_geography_org$average_price), 1) - 0.1
max_org = round(max(grouped_geography_org$average_price), 1) + 0.1

options(repr.plot.width = 10, repr.plot.height = 12)
ggplot(grouped_geography_conv, aes(x = geography, y = average_price)) +
geom_tufteboxplot() + 
facet_grid(.~grouped_geography_conv$year, scales = "free") +
labs(
    colour = "Year", 
    x = "Geography", 
    y ="Average Price", 
    title = "Average prices of Conventional Avocados for each geography by year"
) +
scale_y_continuous(breaks = c(seq(min_con, max_con, 0.2)), limits = c(min_con, max_con)) +
coord_flip() + 
theme(axis.text.x = element_text(angle = 90, vjust = 0))

options(repr.plot.width = 12, repr.plot.height = 12)
ggplot(grouped_geography_org, aes(x = geography, y = average_price)) +
geom_tufteboxplot() + 
facet_grid(.~grouped_geography_org$year, scales = "free") +
labs(
    colour = "Year", 
    x = "Geography", 
    y ="Average Price", 
    title = "Average prices of Organic Avocados for each geography by year"
) +
scale_y_continuous(breaks = c(seq(min_org, max_org, 0.2)), limits = c(min_org, max_org)) +
coord_flip() + 
theme(axis.text.x = element_text(angle = 90, vjust = 0))

plot_correlation(data, type = 'continuous', 'quality')

data.pca <- prcomp(
  select(data, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  center = TRUE,
  scale = TRUE
)
print(data.pca)
summary(data.pca)