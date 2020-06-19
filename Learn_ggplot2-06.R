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

## Facet
# The facet_wrap() is used to break down a large plot into multiple 
# small plots for individual categories.

ggplot(data = df, aes(x = Balls, y = Runs)) + 
    geom_point(aes(size = Runs)) +
    theme_bw() +
    facet_wrap( ~ WCYear) +
    labs(title = "Runs Scored by Sachin in WC")


#---------------------------------------------------#
### Modify Plot background , major and minor axis ###
#---------------------------------------------------#
    
ggplot(data = df, aes(x = Balls, y = Runs)) +
    geom_point(aes(size = Runs), colour = "tomato") +
    theme_bw() +
    theme(panel.background = element_rect(fill = "lightblue"),
          
          panel.grid.major = element_line(colour = "burlywood", size=1.5),
          panel.grid.minor = element_line(colour = "darkorange", 
                                          size = 0.25, 
                                          linetype = "dashed"),
          panel.border = element_blank(),
          axis.line.x = element_line(color = "darkorange", size = 2,
                                     lineend = "butt"),
          axis.line.y = element_line(colour = "darkblue", size = 2,
                                     lineend = "butt")
          ) +
    labs(title = "Runs Scored by Sachin in WC")



#---------------------------------------------------#
# Remove Major and Minor Grid, Change Border,       #
# Axis Title, Text and Ticks                        #
#---------------------------------------------------#

ggplot(data = df, aes(x = Balls, y = Runs )) +
    geom_point() +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()
          )

   
#----------------------------------------------#
# Import an Image                              #
#----------------------------------------------# 

library(grid)
library(png)

img <- png::readPNG('Sachin.png')
g_pic <- rasterGrob(img, interpolate = TRUE)

ggplot(data = df, aes(x = Balls, y = Runs)) +
    annotation_custom(g_pic) +
    geom_point(color = "tomato", aes(size = Runs)) +
    theme_classic() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) 
    
