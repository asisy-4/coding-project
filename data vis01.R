
rm(list=ls()) # clear up data in environment

#load ggplot2
install.packages("tidyverse")
install.packages("ggplot2")

#load gapminder
devtools::install_github("kjhealy/socviz") # install package from github
install.packages("gapminder")
library("ggplot2")
library("gapminder")


# first ggplot graph
## simple scatter plot
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()

### another way
# ggplot(data + gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point()

## use dplyr pipeline
install.packages("dplyr")
library("dplyr")
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

## add label using geom_text
gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) + 
  geom_point() +
  geom_point(data = highlight_df, aes(x=gdpPercap, y= lifeExp), color='red', size=3) +
  geom_text(data = highlight_df, aes(x=gdpPercap, y=lifeExp, label=coutry))

highlight_df$country.year <- paste0(highlight_df$country, ".", as.character(highlight_df$year))

gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_point(data = highlight_df, aes(x=gdpPercap, y=lifeExp),color='red', size=3)+
  geom_text(data = highlight_df, aes(x=gdpPercap, y=lifeExp, label=country.year))

## repel over lapping text labels
install.packages("ggrepel")
library("ggrepel")

gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) +
  geom_point()+
  geom_point(data = highlight_df, aes(x=gdpPercap, y=lifeExp), color="red", size=3) +
  geom_text_repel(data = highlight_df,aes(x=gdpPercap, y=lifeExp,label=country.year))

# add fitted line plot
rmhighlight_df <- gapminder %>%
  filter(gdpPercap<59000)

gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp))+
  geom_point() +
  geom_point(data = highlight_df, aes(x=gdpPercap, y=lifeExp),color='red', size=3) +
  geom_smooth(method = 'lm')+
  geom_smooth(method = 'lm', data = rmhighlight_df, aes(x=gdpPercap, y=lifeExp))

# fix a logged line
gapminder %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  geom_point(data = highlight_df, aes(x=gdpPercap, y=lifeExp), color='red', size=3) +
  geom_smooth(formula = y ~ log(x)) +
  geom_smooth(formula = y ~ log(x),data = rmhighlight_df, aes(x=gdpPercap, y=lifeExp))

#use log transformation of x-axis to repositions the points
gapminder %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp))+
  geom_point()

#change transparency
gapminder %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp))+
  geom_point(alpha=0.3)

#change color
gapminder %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp))+
  geom_point(alpha=0.3, color='blue')

#change size
gapminder %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp))+
  geom_point(alpha=0.3, color='blue', size= 2.5)

# add another dimension
## use color
gapminder %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp,color=continent)) +
  geom_point(alpha=0.3, size=2.5)

## use size
gapminder %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp, size = pop, color=continent)) +
  geom_point(alpha=0.3)

## adjust scale
gapminder %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp, size = pop, color=continent)) +
  geom_point(alpha=0.2) +
  scale_size(range = c(.1, 24), name = "population(M)")

# make it prettier
install.packages("viridis")
install.packages("hrbrthemes")
library("viridisLite")
library("viridis")
library("hrbrthemes")

# add titles and labels
gapminder %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, size = pop, color=continent))+
  geom_point(alpha=0.2)+
  scale_size(range = c(.1, 24), name = "population(M)") +
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "A") +
  theme_ipsum() +
  ylab("life expectancy")+
  xlab("Gdp Per Capita") +
  theme(legend.position = "none") +
  labs(title = 'Economic Growth and Life expectancy, 1952-2007',
       subtitle = 'Data points are country-years',
       caption = 'Source: Gapminder.')

# use a subset of data
unique(gapminder$year)
gapminder2007 <- gapminder %>% filter(year == '2007')
gapminder1957 <- gapminder %>% filter(year == '1957')

gapminder2007 %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.2)+
  scale_size(range = c(.1, 24), name = 'population(M)')

gapminder1957 %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, size = pop, color = continent)) +
  geom_point(alpha=0.2)+
  scale_size(range = c(.1, 24), name = 'population(M)')

# make comparison by putting two figures together
install.packages('patchwork')
library('patchwork')

p2007 <- gapminder2007 %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, size= pop, color= continent))+
  geom_point(alpha=0.3)+
  scale_size(range = c(.1, 24), name = 'population(M)')+
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "A")+
  theme_ipsum()+
  ylab("life expectancy")+
  xlab("Gdp Per Capita") +
  theme(legend.position = "none") +
  labs(title = 'Economic Growth and Life expectancy, 1952-2007',
       subtitle = 'Data points are country-years',
       caption = 'Source: Gapminder.')

p1957 <- gapminder1957 %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, size= pop, color= continent))+
  geom_point(alpha=0.3)+
  scale_size(range = c(.1, 24), name = 'population(M)')+
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "A")+
  theme_ipsum()+
  ylab("life expectancy")+
  xlab("Gdp Per Capita") +
  theme(legend.position = "none") +
  labs(title = 'Economic Growth and Life expectancy, 1952-2007',
       subtitle = 'Data points are country-years',
       caption = 'Source: Gapminder.')

p2007 / p1957
p2007 + p1957

### make the scales the same

p2007 <- gapminder2007 %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, size= pop, color= continent))+
  geom_point(alpha=0.3)+
  scale_size(range = c(.1, 24), name = 'population(M)')+
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "A")+
  xlim(5, 11)+
  ylim(30, 80)+
  theme_ipsum()+
  ylab("life expectancy")+
  xlab("Gdp Per Capita") +
  theme(legend.position = "none") 

p1957 <- gapminder1957 %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp, size= pop, color= continent))+
  geom_point(alpha=0.3)+
  scale_size(range = c(.1, 24), name = 'population(M)')+
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "A")+
  theme_ipsum()+
  xlim(5, 11)+
  ylim(30, 80)+
  ylab("life expectancy")+
  xlab("Gdp Per Capita") +
  theme(legend.position = "none") 


p2007 + p1957

# add fixed line in the figures
geom_smooth(method = 'lm')

## different fixed line methods
gapminder2007 %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp, size= pop))+
  geom_point(alpha=0.2)+
  geom_smooth(method = 'lm', se=FALSE) + # add linear regression line & no confident interval
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se=FALSE) + #add quadratic line
  geom_smooth(method = 'loess', se=FALSE) +
  geom_smooth(method = 'loess', se = FALSE, span = 0.2) + # add locally estimated scatterplot smoothing loess line
  geom_smooth(method = 'gam', se=FALSE,formula = (y~exp(x))) + #add exponential funtion line
  geom_smooth(method = 'gam', se=FALSE,formula = (y~log(x))) + # add logrithmic function line 
  scale_size(range = c(.1,24),name='population(M)') +
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = 'A') +
  xlim(5,11) +
  ylim(30,80)+
  theme_ipsum() +
  ylab('life expectancy') +
  xlab('gdp per capita') +
  theme(legend.position = 'none') +
  labs(title = 'Economic Growth and Life expectancy, 1952-2007',
       subtitle = 'Data points are country-years',
       caption = 'Source: Gapminder.')

#other way to add facets
gapminder2007 %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp,size=pop, color=continent))+
  geom_point(alpha=0.2)+
  facet_wrap(~continent) +
  scale_size(range = c(.1,24),name='population(M)')+
  scale_fill_viridis(discrete = TRUE, guide=FALSE, option='A')+
  theme_ipsum() +
  ylab('life expectancy')+
  xlab('gdp per capita')+
  theme(legend.position = 'none')+
  labs(title = 'Economic Growth and Life expectancy, 1952-2007',
       subtitle = 'Data points are country-years',
       caption = 'Source: Gapminder.')

## Animate it!
library(gganimate)
gapminder %>%
  ggplot(aes(x=log(gdpPercap),y=lifeExp,size=pop,color=continent))+
  geom_point(alpha=0.2)+
  facet_wrap(~continent) +
  scale_size(range = c(.1, 24), name = "population(M)")+
  scale_fill_viridis(discrete = TRUE, guide=FALSE, option='A') +
  theme_ipsum()+
  ylab('life expectancy')+
  xlab('gdp per capita')+
  theme(legend.position = 'none')+
  labs(title = 'Economic Growth and Life expectancy, 1952-2007',
       subtitle = 'Data points are country-years',
       caption = 'Source: Gapminder.')+
  # here is the gganimate specifit bits
  labs(title = 'Year: {frame_time}',x= 'GDP per capita', y= 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

#can also be used to make simple interative map
install.packages("ggplotlyExtra")
library('ggplotlyExtra')
p <- gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(log(gdpPercap), lifeExp, size= pop, color = continent)) +
  geom_point()+
  theme_bw()
ggplotly(p) # 运行不出，ggplotly改成什么了

#connected scatterplot
## grouped data and the group aesthetic
gapminder %>%
  ggplot(aes(x=year, y= lifeExp)) +
  geom_line()

##grouped by country
gapminder %>%
  ggplot(aes(x=year, y=lifeExp)) +
  geom_line(aes(group=country))

## add facets
gapminder %>%
  ggplot(aes(x=year, y= lifeExp)) +
  geom_line(aes(group = country)) +
  facet_wrap(~continent)

### add fitted lines
gapminder %>%
  ggplot(mapping = aes(x=year, y= gdpPercap)) +
  geom_line(color= 'gray70', aes(group=country))+
  geom_smooth(size= 1.1, method = 'loess', se=FALSE)+
  scale_y_log10(label=scales::dollar) +
  facet_wrap(~continent, ncol = 5)+
  labs(x='year',
       y='gdp per capita',
       title = 'GDP per capita on Five continents')

###cont'd
install.packages('socviz')
library('socviz')
gss_sm

gss_sm %>%
  ggplot(aes(x=age, y = childs)) +
  geom_point(alpha=0.2)+
  geom_smooth()+
  facet_grid(sex ~ race) +
  theme(strip.background = element_rect(fill='white')) +
  theme(text = element_text(size=14, family = 'Helvetica-Narrow'))
