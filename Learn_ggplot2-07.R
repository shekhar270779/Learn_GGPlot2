library(ggplot2)
library(lubridate)

# reading data from csv file into a dataframe df
df <- read.csv('Sachin_WC_Scores.csv')

# Convert Date column from string to date-time
df$Date <- parse_date_time(df$Date, orders = ("dmy"))

# Extract year from Date
df$WCYear <- as.factor(paste0('WC-',lubridate::year(df$Date)))

# Setup
options(scipen = 999) # it turns of scientific notation for large numbers

head(df)

# rename first column
colnames(df)[1] = 'Country'

head(df)

## Scatter

g <- ggplot(df, aes(x = Balls, y = Runs)) +
     geom_point(aes(color = WCYear), size = 2) +
     ggtitle("Sachin's score in WC matches")

print(g)

#------------------------#
# ggplot with encircling #
#------------------------#
library(ggalt)

g + geom_encircle(aes(x = Balls, y = Runs), 
                  data = df[df$Runs >=100,],
                  color = "red")


## jitter plot ##
ggplot(df, aes(x = Country, y = Runs)) +
    geom_jitter(aes(color = WCYear), width = .1, size = 2) +
    theme(axis.text.x = element_text(angle = 90)
          ) 
   

theme_set(theme_classic())

## Count Plot ##
# Whereever there are more points overlap, 
# the size of the circle gets bigger.


ggplot(df, aes(x = WCYear, y = Runs )) +
    geom_point()

ggplot(df, aes(x = WCYear, y = Runs )) +
    geom_count(color = 'steelblue', show.legend = F)


## Diverging Bars ## 
## 50+ Scores made by Sachin ## 

df$score_type <- ifelse(df$Runs >=50, "above 50", "below 50")
r <- ifelse(df$Runs >=50, 1, -1)    

ggplot(df, aes(x = WCYear, y = r)) +
    geom_bar(stat = 'identity', aes(fill = score_type), width = .5)  +
    coord_flip() + 
    theme(
        axis.title = element_blank()
    ) +
    scale_fill_manual(name = "Score Type",
                      labels = c("Above 50", "Below 50"),
                      values = c("above 50" = "steelblue",
                                 "below 50" = "tomato")
    )


### Bar chart ###
library(dplyr)
RunsbyWC <- df %>% group_by(WCYear ) %>%
            summarize(Total_Runs = sum(Runs))
            

ggplot(RunsbyWC, aes(x = WCYear, y = Total_Runs)) + 
    geom_bar(stat = "identity", fill = "steelblue")


### Ordered Bar chart ###
RunsbyTeams <- df %>% group_by(Country) %>%
    summarize(Total_Runs = sum(Runs)) %>%
    arrange(desc(Total_Runs))

RunsbyTeams$Country <-  factor(RunsbyTeams$Country, 
                               levels = RunsbyTeams$Country)


ggplot(RunsbyTeams, aes(x = Country, y = Total_Runs)) + 
    geom_bar(stat = "identity", fill = "steelblue") +
    theme(
        axis.text.x = element_text(angle=90)
    )


## Stacked barplot ##

ggplot(df, aes(Country, Runs))  + 
    geom_bar(aes(fill = WCYear), stat = "identity") +
    scale_fill_brewer(palette = "Set1") + 
    theme(
        axis.text.x = element_text(angle = 90)
    )


## Boxplot ##

ggplot(df, aes(WCYear, Runs)) +
    geom_boxplot(aes(fill = WCYear), show.legend = F) +
    geom_jitter(width = 0.1)
