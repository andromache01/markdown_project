# from class on 3/7/17 on dplyr
library(dplyr)
gm_by_cont<-group_by(gapminder,continent)
mean_gdp_by_cont<-summarise(gm_by_cont, gdp=mean(gdpPercap))
mean_gdp_by_cont

# pipe approach

gapminder %>% group_by(continent) %>% summarise(gdp=mean(gdpPercap)) %>% ggplot(aes(x=continent,y=gdp))+geom_point()

# filter and select

gapminder %>% filter(year==1977) %>% filter(lifeExp==min(lifeExp))
  
gapminder %>% filter(year==1992) %>% filter(lifeExp==min(lifeExp))

mutate(gapminder, gdp=gdpPercap*pop) %>% filter(year==2007) %>% ggplot(aes(x=continent, y=gdp))+geom_point()+scale_y_log10()

#challenge - calculated average life expectancy per continent - which ahs the longest?

mean_lifeExp_by_cont<-summarise(gm_by_cont, lifeExp=mean(lifeExp))
#shortest
mean_lifeExp_by_cont %>% filter(lifeExp==min(lifeExp))
#longest
mean_lifeExp_by_cont %>% filter(lifeExp==max(lifeExp))

# challenge: repeat above for each year: longest and shortest in 2007? greatest change?

gm_by_cy<-group_by(gapminder, continent, year)
mean_le_by_cy<-summarise(gm_by_cy,lifeExp=mean(lifeExp))
#shortest
mean_le_by_cy %>% ungroup %>% filter(year==2007) %>% filter(lifeExp==min(lifeExp))
#longest
mean_le_by_cy %>% ungroup %>% filter(year==2007) %>% filter(lifeExp==max(lifeExp))

mean_le_by_cy2<-mean_le_by_cy %>% filter(year==c(1952,2007)) %>% group_by(continent)
mean_le_by_cy3<-summarise(mean_le_by_cy2, diff=max(lifeExp)-min(lifeExp))
#biggest difference
mean_le_by_cy3 %>% filter(diff==max(diff))
    
                         