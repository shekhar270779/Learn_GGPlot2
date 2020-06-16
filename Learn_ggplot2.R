
## ggplot2 is a visualization package utilizing Grammar of Graphics concept 
### Grammar of Graphics helps us to construct graphical figures out of different 
### visual elements, it uses layered approach to build graphs

# R version 3.6.1

# To install packages 
# install.packages("ggplot2", "dplyr", "gapminder")

library(ggplot2)
library(dplyr)
library(gapminder)


# gapminder package provides values for life expectancy, GDP per capita, 
# and population for every five years, from 1952 to 2007.

# Check unique years of gapminder
unique(gapminder$year)

gap <- gapminder %>% filter(continent != "Oceania")

gap_2007 <- gap %>% filter(year == 2007)

# structure of dataset
str(gap_2007)


## Scatter Plot
ggplot(data = gap_2007, mapping = aes(x = gdpPercap, y = lifeExp)) +
    geom_point() + 
    labs(x = "Per capita GDP",
         y = "Life Expectancy",
         title = "Per Capita GDP Vs Life Expectancy",
         subtitle = "Year 2007"
         ) 

t <-  labs(x = "Per capita GDP",
           y = "Life Expectancy",
           title = "Per Capita GDP Vs Life Expectancy",
           subtitle = "Year 2007")

ggplot(data = gap_2007, mapping = aes(x = gdpPercap, y = lifeExp)) +
    geom_point(aes(color = continent)) + t

ggplot(data = gap_2007, mapping = aes(x = gdpPercap, y = lifeExp)) +
    geom_point(aes(color = continent, size = pop)) + t

ggplot(data = gap_2007, mapping = aes(x = gdpPercap, y = lifeExp)) +
    geom_point(aes(color = continent,  shape = continent)) + t


## BarPlot
ggplot(gap_2007, aes(x = continent)) +
    geom_bar() + 
    labs(title = "Number of countries in Continents",
         x = "Continent", y = "Count of Countries")

# Calculate median population of Contients in 2007
median_pop_continent <- gap_2007 %>% group_by(continent) %>%
                        summarize(median_pop = median(pop))
print(median_pop_continent)

ggplot(median_pop_continent, aes(x = continent, y = median_pop)) + 
    geom_bar(stat = "identity") +
    labs(title = "Median population of Continents in 2007",
         x = "Continent", y = "Median Population")

# Extract data related to India
Ind_df <- gap %>% 
            filter(country == "India")

## line plot with  scatter
g <- ggplot(data = Ind_df, mapping = aes(x = year, y = pop)) +
        geom_line() + 
        geom_point() +
        labs(title = "Population growth in India",
             subtitle = "Period: 1950-2007",
            caption = "Source: gapminder package",
            x = "Year", y = "Population"
            )

print(g)

## to save a plot
ggsave("plot.png", plot = g, height=6, width=8)



## Histogram
ggplot(gap_2007, aes(lifeExp)) +
    geom_histogram(bins = 20, fill='blue', color='white') +
    labs(title = "Frequency distribution of Life Expectancy in 2007",
         x = "Life Expectancy (years)",
         y = "Frequency")


## Boxplot
b <- ggplot(gap_2007, aes(x = continent, y = lifeExp)) +
        geom_boxplot( aes(fill = continent)) + 
        labs(title = "Life Expectancy across continents",
            subtitle = "2007",
            x = "Continent",
            y = "Life Expectancy (years)")

print(b)

# add points in Boxplot
b  + geom_point()

# Facet
gap1 <- gap %>% filter(year >=1997)

f <- ggplot(gap1, aes(x = gdpPercap, y = lifeExp)) +
        geom_point()  

f + facet_grid(continent ~ year)

# Find mean lifeExpectancy by continent and year
gap_life <- gap %>%
    group_by(continent, year) %>%
    summarize(mean_lifeExp = mean(lifeExp))

g1 <- ggplot(gap_life, aes(x = year, y = mean_lifeExp)) + 
    geom_line() 

g1 + facet_grid(. ~ continent)

g1 + facet_wrap( ~ continent)


# Smoother
g2 <- ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp)) +
    geom_point() 

g2 + geom_smooth()

# set confidence interval to 99%
g2 + geom_smooth(level = 0.99)

# remove confidence interval
g2 +  geom_smooth(se = FALSE)

# fit linear model
g2 +  geom_smooth(method='lm')

# smooth + facet_wrap
g2 + facet_wrap(~ continent) + 
    geom_smooth(aes(color = continent), se = FALSE)


# Scales

# Transform x axis
g3 <- ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp, color = continent)) +
    geom_point()

g3 + scale_x_continuous(trans = "log10")

g3 + scale_x_log10()

g3  + scale_x_log10() + 
    scale_y_continuous(limits = c(0, 95))

g3 + scale_x_log10() + 
    scale_y_continuous(limits = c(0, 95)) + 
    scale_color_brewer(palette = "Dark2")

# manual color
g3 +    scale_x_log10() + 
        scale_y_continuous(limits = c(0, 95)) +
        scale_color_manual(values = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"))

# Themes
g3 +   theme_dark()

g3 + theme_void()

library("ggthemes")

g3 +  theme_tufte()

g3 + theme_solarized() + 
    scale_color_solarized("blue")

# Customize 
g3 + theme_light() + 
     theme(legend.position = "bottom")


g3 + theme_light() + 
        theme(legend.key = element_blank(),
          axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title = element_text(size=12)) + 
    labs(x = "Per capita GDP",
         y = "Life Expectancy",
         title = "2007 Life Expectancy"
    ) 


