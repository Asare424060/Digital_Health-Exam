#Load your data set
# Read data from a specified file path
file_path <- "C:/Users/OJ/Desktop/RStudio/drinks.csv"
drinks <- read_csv(file_path)
View(drinks)
p <- drinks
View(p)
# Install packages
library(dplyr)
library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(gridExtra)
library(flextable)
library(RColorBrewer)



# Explore and clean the data
# Add comprehensive data cleaning steps here

cols(
  country = col_character(),
  beer_servings = col_double(),
  spirit_servings = col_double(),
  wine_servings = col_double(),
  total_litres_of_pure_alcohol = col_double()
)
  nrow(p)
  ncol(p)
  str(p)
  head(p)
  unique(p$beer_servings)
  unique(p$country)
  unique(p$spirit_servings)
  unique(p$total_litres_of_pure_alcohol)
  p[1:20, c(1:2)]
  a <- p[1:20, c(1:2)]
  
  #Filtering beer_serving data and setting threshold
summary(p$beer_servings)
a[a$beer_servings > 100, ]
a1 <- a[a$beer_servings > 100, ]

#Create a plot for beer_servings
# Visualize beer servings per country
beer_plot <- ggplot(a1, aes(x = country, y = beer_servings)) +
  geom_col(fill = "skyblue") +
  labs(title = "Distribution of Beer Consumption per Country",
       x = "Country",
       y = "Beer Servings") + coord_cartesian(
         xlim = NULL, ylim = c(20, 400)) + theme_bw()
# Show the plot
beer_plot

#Create a plot for spirit servings
p[1:20, c(1,3)]
g <- p[1:20, c(1,3)]

# Filter out countries with zero spirit servings

g[g$spirit_servings > 0, ]
summary(p$spirit_servings)
g[g$spirit_servings > 80, ]
k <- g[g$spirit_servings > 80, ]


# Create the plot with enhanced customization options
k1 <- ggplot(k, aes(x = country, y = spirit_servings)) +
  geom_col(fill = "blue") +
  labs(title = "Distribution of Spirit per Country in Litres",
       x = "Country",
       y = "Frequency") +
  theme_bw() +
  coord_cartesian(ylim = c(0, max(k$spirit_servings) * 1.1))

# Show the plot
print(k1)

 
 #Create a plot for Wine Serving
p[1:20, c(1,4)]
r <- p[1:20, c(1,4)]

# Filter data based on threshold for wine servings
summary(p$wine_servings)
r[r$wine_servings > 50, ]
m <- r[r$wine_servings > 50, ]
 
# Create the plot with enhanced customization options
m1 <- ggplot(m, aes(x = country, y = wine_servings)) +
  geom_col(fill = "lightgreen") +
  labs(title = "Distribution of Wine per Country in Litres",
       x = "Country",
       y = "Frequency") +
  theme_bw() + coord_cartesian(ylim = c(0, max(m$wine_servings) * 1.1))

# Show the plot
print(m1)
