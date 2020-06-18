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

g <- ggplot(data = df, aes(x = Balls, y = Runs)) 
g1 <- g + geom_point(aes(color = WCYear, size = Runs))
g2 <- g1 + labs(title = "Sachin's score in World Cup matches", 
                subtitle = "Year: 1992-2011",
                x = "Balls Played",
                y = "Runs Scored",
                caption = "World Cup Scores")
# Select colors from palette
g3 <- g2 + scale_color_brewer(palette = "Set1")

## Set X/Y axis Text and Tick locations
g4 <- g3 + scale_x_continuous(breaks = seq(0, 150, 25)) +
    scale_y_continuous(breaks = seq(0, 160, 50),
                       labels = c("Zero", "Fifty", "Century", "150"))

# theme()
g4 + theme_classic()

# -----------------------------#
### Plot and Axis Titles ###
# -----------------------------#
# Plot & axis titles and the axis text are part of the plot’s theme.
# Therefore, it can be modified using the theme() function. 
# The theme() function accepts one of the four element_type() functions 
# mentioned below -
# element_text(): is used for title, subtitle and captions
# element_line(): is used to modify line based components 
# element_rect(): Modifies rectangle components such as plot and panel background.
# element_blank(): Turns off displaying the theme item

g5 <- g4 + 
        theme(plot.title = element_text(size= 20, color = "tomato"),
              plot.subtitle = element_text(size = 15, color = "steelblue"),
              plot.caption = element_text(size = 12, color = "red"),
              axis.title.x = element_text(size=10, face = "bold", color = "green"),
              axis.title.y = element_text(size = 10, face = "bold", color="green"),
              axis.text.x = element_text(size = 10, face = "bold", color="blue", angle = 30),
              axis.text.y = element_text(size = 10, face = "bold", color = "blue", angle = 30)
              )

print(g5)

# -----------------------------#
### Modify Legend ###
# -----------------------------#

# Change Legend Title

# Method 1 : using labs()
g5 + labs(color = "World Cup Year",
          size = "Runs Scored")

# Method 2 : using guides()
g5 + guides(color = guide_legend("World Cup Year"),
            size = guide_legend("Runs Scored"))

# Method 3 : using scale_aesthetic_vartype()
g5 + scale_color_discrete(name ="World Cup Year") +
    scale_size_continuous(name = "Runs Scored", guide = FALSE)

g6 <- g5 + 
    scale_color_manual(name = "World Cup Year",
                       labels = c("WC-92", "WC-96",
                                  "WC-99", "WC-2003",
                                  "WC-2007", "WC-2011"),
                       values = c("WC-1992" = "blue",
                                  "WC-1996" = "red",
                                  "WC-1999" = "green",
                                  "WC-2003" = "brown",
                                  "WC-2007" = "orange",
                                  "WC-2011" = "black")) +
    scale_size_continuous(name = "Runs Scored",
                     labels = c("Duck", "Fifty", "Century", "150")
                     ) + 
    guides(colour = guide_legend(order = 1),
           size = guide_legend(order = 2))

print(g6)

### Style Legend Style and Text ###

g7 <- g6 +
        theme(legend.title = element_text(size = 12, color="firebrick"),
          legend.text = element_text(size = 10),
          legend.key = element_rect(fill = "yellow")
              )

print(g7)    

### legend hide or change position ###

g7 + theme(legend.position = "None")


### Add text ###

# take all those records where century scored century
df_cent <- df[df$Runs >=100,]

g7 +
    geom_text(aes(label = Runs, size = Runs), data = df_cent
              )

g7 +
    geom_label(aes(label = Runs), data = df_cent
    )

library(ggrepel)
g8 <- g7 + geom_text_repel(aes(label = Runs), data = df_cent)
print(g8)


### Annotations ###
library(grid)

my_text = "WoW!! Innings vs Pak"
g9 <- g8 + annotate("text", label=my_text,x = 75, y = 98 )
print(g9)

g10 <- g9 + annotate("rect", xmin = 100 , xmax = 155 , ymin = 100 , ymax = 165,
              alpha = .6, fill = "pink")
print(g10)

g11 <- g10 + annotate("segment", x = 30, xend = 120, y = 50, yend = 50,
               color = "blue", size = 2)  +
        annotate("pointrange", x = 75, y = 98, ymin = 90, ymax = 110,
             color = "red", size = 1.5)

print(g11)


