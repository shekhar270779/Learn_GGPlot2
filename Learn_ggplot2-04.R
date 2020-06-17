library(ggplot2)
library(lubridate)

# reading data from csv file into a dataframe df
df <- read.csv('Sachin_WC_Scores.csv')

# Display first 5 recs of dataframe
head(df)

# Convert Date column from string to date-time
df$Date <- parse_date_time(df$Date, orders = ("dmy"))

# Extract year from Date
df$WCYear <- as.factor(paste0('WC-',lubridate::year(df$Date)))

# structure of datarame 
str(df)

# Setup
options(scipen = 999) # it turns of scientific notation for large numbers


# Scatter plot
g <- ggplot(data = df, aes(x = Balls, y = Runs)) 
g + geom_point()

# make all points blue
g + geom_point(color = "steelblue")

# color points based on year of WC
g + geom_point(aes(color = WCYear))

# Adjusting X and Y axis using xlim(), ylim()
# How many runs Sachin scored when he played between 50-100 balls
g + geom_point(aes(color = WCYear)) +
    xlim(c(50, 100)) + ylim(c(20, 120))

# Without deleting points we can zoomin
g + geom_point(aes(color = WCYear)) +
    coord_cartesian(xlim = c(50, 100), ylim = c(20, 120))

# Add title and labels
g1 <- g + geom_point(aes(color = WCYear, size = Runs))
print(g1)

g2 <- g1 + labs(title = "Sachin's score in World Cup matches", 
          subtitle = "Year: 1992-2011",
          x = "Balls Played",
          y = "Runs Scored",
          caption = "World Cup Scores")
print(g2)

# Select colors from palette
g3 <- g2 + scale_color_brewer(palette = "Set1")
print(g3)

## Set X/Y axis Text and Tick locations
g4 <- g3 + scale_x_continuous(breaks = seq(0, 150, 25)) +
      scale_y_continuous(breaks = seq(0, 160, 50),
                       labels = c("Zero", "Fifty", "Century", "150"))
print(g4)

# theme()
g4 + theme_classic()
