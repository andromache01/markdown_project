#installing the packages we need
install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("curl")
install.packages("knitr")
library(curl)
library(ggplot2)
library(dplyr)
library(viridis)
library(reshape2)
#getting data to work with
gapminder_location<-curl(url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv")
gapminder<-read.csv(gapminder_location, stringsAsFactors = FALSE)
#show first rows
head(gapminder)
#information about the data columns
str(gapminder)
#what kind of data is it
class(gapminder)
#data type for different columns
typeof(gapminder$year)
typeof(gapminder$lifeExp)
typeof(gapminder$continent)
#finding column and row names
colnames(gapminder)
rownames(gapminder)
#make a copy of the data
copygapm<-gapminder
#change the column names to letters a-f
colnames(copygapm)<-letters[1:6]
colnames(copygapm)
#looking at different locations in data
gapminder[15,3]
head(gapminder[,5])
gapminder[1:6,3:4]
gapminder[c(1,6), c(3,4)]
#finding value in data
max(gapminder[,6])
min(gapminder[,6])

#add new columns
gdp<-gapminder$gdpPercap*gapminder$pop
copygapm<-cbind(copygapm, gdp)
gapminder<-cbind(gapminder,gapminder$pop/1000000)

#how to make a function 
#don't necessarily need return statement, automatically returns last line of function
calcgdp<- function(dat){
  gdp<-dat$gdpPercap*dat$pop
  dat<-cbind(dat,gdp)
  return(dat)
}
gapminder<-calcgdp(gapminder)

newgap<-calcgdp(gapminder[1:4,])
#add GDP for 2007 data and 1952 data
newgap2007<-calcgdp(gapminder[gapminder$year==2007,])
newgap2007<-calcgdp(gapminder[gapminder$year==2007|gapminder$year==1952,])

#add gdp  for some year
calcgdp1<-function(dat, year){
  dat<-dat[dat$year %in% year,]
  gdp<-dat$gdpPercap*dat$pop
  dat<-cbind(dat,gdp)
  return(dat)
}
newgap1952<-calcgdp1(gapminder,1952)

#get for year adn country
calcgdp2<-function(dat, year, country){
  dat<-dat[dat$year %in% year & dat$country %in% country,]
  gdp<-dat$gdpPercap*dat$pop
  dat<-cbind(dat,gdp)
  return(dat)
}
newgap1952ghana<-calcgdp2(gapminder,1952, "Ghana")
newgap1960sghana<-calcgdp2(gapminder,1960:1969, "Ghana")
newgaprange<-calcgdp2(gapminder,1952:2007,c("Ghana","Afghanistan"))

#specifying default values
calcgdp3<-function(dat, year=NULL, country=NULL) {
  if (year=NULL & country = NULL) {
    dat<-dat
  } else if (year= NULL) {
      dat<-dat[dat$country %in% country,]
    } else if (country = NULL) {
        dat<-dat[dat$year %in% year,]
      } else {
        dat<-dat[dat$year %in% year & dat$country %in% country,]
      }     
  gdp<-dat$gdpPercap*dat$pop
  dat<-cbind(dat,gdp)
  return(dat)
}
newgap1952ghana<-calcgdp3(gapminder,, "Ghana")

#but use !isnull=country or something
# if(!is.null(year)){
  dat<-dat[dat$year %in% year,]
}

#but the variable storage used in your function is separate and does not interfere with your other variables
add3<-function(y){
  y+3
}
x<-10
y<-add3(x)

#relationship between life expectancy and year
plot(gapminder$year,gapminder$lifeExp)
lifeyear<-lm(lifeExp ~ year, data=gapminder)
str(lifeyear)
summary(lifeyear)

#plotting of life exp cs. gdp per cap
ggplot(data=gapminder, aes(x=lifeExp, y=gdpPercap)) + geom_point()

#plot life exp as a function of time
ggplot(data=gapminder, aes(x=year,y=lifeExp))+geom_point()
ggplot(data=gapminder, aes(x=year,y=lifeExp, color=continent))+geom_point()

pdf(file="rplot3.pdf", width = 6,height=3)
ggplot(data=gapminder, aes(x=year,y=lifeExp, color=continent))+geom_point()+geom_smooth(method=lm)
dev.off()

#plot gdppercap~lifeexp
#makepdf of plot
#fit line
#estimate the model

pdf(file="rplot5.pdf", width = 6,height=3)
ggplot(data=gapminder, aes(x=lifeExp, y=gdpPercap))+geom_point()+geom_smooth(method=lm)
dev.off()
lifeGDP<-lm(lifeExp ~ gdpPercap, data=gapminder)
str(lifeGDP)
summary(lifeGDP)

pdf(file="rplot6.pdf", width = 6,height=3)
ggplot(data=gapminder, aes(x=lifeExp, y=gdpPercap, color=continent))+geom_point()+scale_y_log10()+geom_smooth(method=lm)
dev.off()

pdf(file="rplot7.pdf", width = 6,height=3)
ggplot(data=gapminder, aes(x=lifeExp, y=gdpPercap))+geom_point()+scale_y_log10()+geom_smooth(method=lm)
dev.off()

#do a function to do this
makegraph<-function(dat, xvar, yvar){
  pdf("file.pdf", width = 6,height=3)
  plot1<-ggplot(data=dat, aes_string(x=xvar, y=yvar))+geom_point()+scale_y_log10()+geom_smooth(method=lm)
  print(plot1)
  dev.off()
}
makegraph(gapminder, "lifeExp", "gdpPercap")

model<-lm(lifeExp ~ year, data=gapminder)
str(summary(model))

gapmindermean <- gapminder %>% group_by(continent) %>% summarize(lifeExPmean = mean(lifeExp)) 
gapminderse <- gapminder %>% group_by(continent) %>% summarize(lifeExpSE = sd(lifeExp)/sqrt(length(lifeExp)))
gapminderlifeexpdata<-full_join(gapmindermean,gapminderse)                                                          
ggplot(data=gapminderlifeexpdata, aes(x=continent, y=lifeExPmean, color, fill=continent)) + scale_fill_viridis(discrete=TRUE) + geom_bar(stat = "identity", position = position_dodge()) +   geom_errorbar(aes(ymin=lifeExPmean-lifeExpSE, ymax=lifeExPmean+lifeExpSE), width=.2, position=position_dodge(.9))


pairwise.t.test(gapminder$lifeExp, gapminder$continent)  


#working on split-apply-divide-on march 2
calc_cont_pop<-function(continent, year){
  sum(gapminder[gapminder$year==year & gapminder$continent==continent, "pop"])
}

calc_cont_pop_2007<-function(continent){
  sum(gapminder[gapminder$year==2007 & gapminder$continent==continent, "pop"])
}
cont_pops<-lapply(unique(gapminder$continent), calc_cont_pop_2007)
names(cont_pops)<-unique(gapminder$continent)
cont_pops

cont_pops<-mapply(calc_cont_pop,unique(gapminder$continent), 2007)

cont_pops<-mapply(calc_cont_pop, unique(gapminder$continent), c(1952,1957,1962,1967,1972))
names(cont_pops)<-unique(gapminder$continent)
cont_pops

#function that finds mean gdpPercap for a given year, apply to all continents and years
#mapply to one year -- apply to all given years
gdpcontyear<-function(dat){
  x<-dat

cont_gdp<-function(continent,year){
  mean(x[x$year==year & x$continent==continent, "gdpPercap"])
}
  
mapply_cont_gdp<-function(year){
  mapply(cont_gdp, unique(x$continent), year)}


output1<-sapply(unique(x$year), mapply_cont_gdp)
colnames(output1)<-unique(x$year)
print(output1)
}
willthiswork<-gdpcontyear(gapminder)
willthiswork

#same thing for lifeexp
lifecontyear<-function(dat){
    x<-dat
    
    cont_life<-function(continent,year){
      mean(x[x$year==year & x$continent==continent, "lifeExp"])
    }
    
    mapply_cont_life<-function(year){
      mapply(cont_life, unique(x$continent), year)}
    
    
    output1<-sapply(unique(x$year), mapply_cont_life)
    colnames(output1)<-unique(x$year)
    print(output1)
}

lifeExp_contyear<-lifecontyear(gapminder)
lifeExp_contyear<-melt(lifeExp_contyear)
lifeExp_contyear<-data.frame(lifeExp_contyear)
colnames(lifeExp_contyear)<-c("continent", "year", "lifeExp")
ggplot(data=lifeExp_contyear, aes(x=year,y=lifeExp,color=continent)) +geom_point()+ geom_line() + scale_fill_viridis(discrete=TRUE) + scale_x_continuous(name="Life Expectancy") + scale_y_continuous(name="GDP per capita") + labs(title ="GDP per capita vs. Life Expectancy")

ggplot(data=gapminder, aes(x=year,y=lifeExp,color=continent)) +geom_point()+ geom_smooth(method=lm)+ scale_fill_viridis(discrete=TRUE) + scale_x_continuous(name="Life Expectancy") + scale_y_continuous(name="GDP per capita") + labs(title ="GDP per capita vs. Life Expectancy")
model1<-lm(lifeExp ~ year, continent=="Asia", data=gapminder)
summary(model1)$r.squared
summary(model1)
years<-unique(gapminder$year)
years[1]

library(plyr)
ddply(.data=calcgdp(gapminder),
      .variables=c("continent", "year"),
      .fun=function(x){
        meangdppercap<-mean (x$gdpPercap)
        print(paste("The mean gdp per cap for",unique(x$continent),
                    "is", format(meangdppercap,big.mark = ",")))
      }
)
