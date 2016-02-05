---
title: "Project 4: Field Goal Kicking in the NFL"
output: html_document
---




#Preliminary Data Analysis
```{r echo=FALSE, message=FALSE, warning=FALSE}

getwd()
setwd('C:/Users/Ricky/Documents/Udacity/P4')
fg<-read.csv('test4.csv')



```


This data set is from www.pro-football-reference.com/, it shows the progression of field goal kicking over the last 55 years in the National Football League(NFL). The data's categorical variable is tm(teams). The number of teams varied over the period, from 13 to 32, this is why the teams have different counts in the data set. The numbered variables indicate the yardage the kick was taken from. For instance, 20a indicated the number of kicks attempted between 20 and 29 yards for a given team in the year indicated in the observation. While 40m would indicate the number of kicks made between 40 and 49 yards. the 50m and 50a variables indicate kicks over 50 yards.

Here we look at a summary to see areas that may need to be wrangled and cleaned for the analysis. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
library(ggplot2)

summary(fg)

```

Here we separate the data into team data and wrangle the data in the following ways:

- Creating field goal perentage variables for different distances.

- Deleting outliers that are impossible(percentages greater than 100%).

- Partitioning the data into eras according to historical events in the variable "rules".

- Creating a variable that measures the points lost via misses called "ptfm".

- Creating a variable called "notm" that measures the number of teams in the league.

- Sorting through some team names that moved cities during the period of study in order to better analyze team data.

- finally subseting all variables into league and team data.

```{r}

fg$pct19<-fg$X19m/fg$X19a
fg$pct20<-fg$X20m/fg$X20a
fg$pct30<-fg$X30m/fg$X30a
fg$pct40<-fg$X40m/fg$X40a
fg$pct50<-fg$X50m/fg$X50a

fg<-subset(fg,xpct<=1)
fg<-subset(fg,pct30<=1)
fg<-subset(fg,pct40<=1)

fg$rules<-cut(fg$year,c(1959,1973,2014,2015),right=TRUE,dig.lab=10)
fg$decade<-cut(fg$year,c(1959,1969,1979,1989,1999,2009,2015),right=TRUE,dig.lab=10)

fg$ptfm<-with(fg, ((fga-fgm)*3+(xpa-xpm)*1))

fg$notm<-32
fg$notm[1999<=fg$year & fg$year<=2001]<-31
fg$notm[1995<=fg$year & fg$year<=1998]<-30
fg$notm[1976<=fg$year & fg$year<=1994]<-28
fg$notm[1976<=fg$year & fg$year<=1994]<-28
fg$notm[1970<=fg$year & fg$year<=1975]<-26
fg$notm[1967<=fg$year & fg$year<=1969]<-16
fg$notm[1966==fg$year]<-15
fg$notm[1961<=fg$year & fg$year<=1965]<-14
fg$notm[1960==fg$year]<-13

fg$tm[fg$tm=="Boston Patriots"]<-"New England Patriots"
fg$tm[fg$tm=="Tennessee Oilers"]<-"Tennessee Titans"
fg$tm[fg$tm=="Los Angeles Raiders"]<-"Oakland Raiders"
fg$tm[fg$tm=="Baltimore Colts"]<-"Indianapolis Colts"
fg$tm[fg$tm=="Los Angeles Rams"]<-"St. Louis Rams"
fg$tm[fg$tm=="St. Louis Cardinals"]<-"Arizona Cardinals"
fg$tm[fg$tm=="Phoenix Cardinals"]<-"Arizona Cardinals"


fgt<-subset(fg,tm!='Avg Tm/G')
fgt<-subset(fgt,tm!='Avg Team')
fgt<-subset(fgt,tm!='League Total')

fgl<-subset(fg,tm=='Avg Tm/G'|tm=='Avg Team'|tm=='League Total')
fglea<-subset(fgl,tm=='League Total')

```


###Is there a difference between the field goal(FG) and extra point(XP) percentage distribution?

```{r echo=FALSE, message=FALSE, warning=FALSE}

qplot(fgpct,data=fgt,binwidth=.01)

```

We see fgpct looks to be left skewed with the majority of datapoints lieing between 0.6 and 0.9

```{r echo=FALSE, message=FALSE, warning=FALSE}

qplot(xpct,data=fgt,binwidth=.01)
```

While the extra point percentage goes up exponentially and peaks at 100 percent.

###Are there any other unusual distributions of other variables?

```{r echo=FALSE, message=FALSE, warning=FALSE}
qplot(g,data=fgt,binwidth=1)

```

We see the vast majority of years 16 games were played in a season. This is something that will need to be accounted for when looking at statistics based on counts or quantity as apposed to percentages.

```{r echo=FALSE, message=FALSE, warning=FALSE}
qplot(notm,data=fglea,binwidth=1)

```

We observe a big gap in the number of teams in the league from 16 to 26. Where we see most of the data is in years with 28-32 teams in the league. Similar to games played this is another statistic to account for while looking at counting statistics.

```{r echo=FALSE, message=FALSE, warning=FALSE}
qplot(fga,data=fglea,binwidth=100)

```

As with other counting statistics, the number of field goals attempted is based both on the number of games played and number of teams in the league. looking at the table confirms this notion as the lowest fga totals are in the early years of the NFL where fewer teams and games were played.


###Now looking at FG and XP over time to observe any correlation between time and kicking accuracy trends over the last 55 years.

```{r echo=FALSE, message=FALSE, warning=FALSE}

cor.test(fglea$year,fglea$fgpct)

ggplot(aes(year,fgpct),data=subset(fgl,tm=='League Total'))+
  geom_point()+scale_x_continuous(breaks=seq(1960,2015,5))
```


We see league-wide field goal accuracy has a very strong positive correlation with the year variable(time). This indicates kickers are improving over time for reasons we will investigate later.

```{r echo=FALSE, message=FALSE, warning=FALSE}
cor.test(fgt$year,fgt$fgpct)

ggplot(aes(year,fgpct),data=fgt)+
  geom_point(alpha=0.4)+scale_x_continuous(breaks=seq(1960,2015,5))
```

Notice the weaker, but still positive, correltation figure when we examine all the teams from each year. This is because there is a fairely large range between the best and worst kicker every year, usually around 30-40 percentage points. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
cor.test(fglea$year,fglea$xpct)

ggplot(aes(year,xpct),data=subset(fgl,tm=='League Total'))+
  geom_point()+scale_x_continuous(breaks=seq(1960,2015,5))

```

We see a drastic decrease in correlation when we look at extra point correlation compared to time. Looking at the above figure we see that there is a large drop in the data in 1974, after this decrease kickers seem to make large improvements year after year.

```{r echo=FALSE, message=FALSE, warning=FALSE}
cor.test(fgt$year,fgt$xpct)

ggplot(aes(year,xpct),data=fgt)+
  geom_jitter(alpha=.1)+scale_x_continuous(breaks=seq(1960,2015,5))
```

Just like the field goal percentage we see the large range between the best and worst kickers result in a lower correlation statistic. We also notice a greater number of kickers are having perfect season kicking extra points as time goes on. Additionally, in 2015 we see some very low extra point numbers that will be addressed later on in the next section.




###FG success compared to XP of similar kicking distances after rule changes. 

In 1974, Kicks were moved from 10 yards to 20 yards away. In 2015, extra points were moved from 20 yards to 33 yards. In these figures I compare field goals of similar distances in order to see how kickers dealt with the changes to the rules according to historical data of similar kicks.

```{r echo=FALSE, message=FALSE, warning=FALSE}

ggplot(aes(year,pct20),data=fglea)+
  geom_line()+
  geom_point(data=subset(fglea,year==1974),aes(x=year,y=xpct,color='red'))+
  xlim(c(1964,1985))+guides(color=guide_legend(title=NULL,labels='2015 PAT success rate'))+
  scale_color_discrete(labels = paste("1974 PAT 
  success rate"))
```

I notice that while we see a large drop in extra point percentage due to the rule change kickers did very well in reality compared to historical data on field goals 10 years before and after the rule change.


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(year,pct30),data=fglea)+
  geom_line()+geom_point(data=subset(fglea,year==2015),aes(x=year,y=xpct,color='red'))+
  coord_cartesian(xlim = c(2000,2016))+ylab("FG% 30-39 Yards")+xlab("Year")+
  guides(color=guide_legend(title="",labels=""))+scale_color_discrete(labels = paste('2015 PAT success rate'))



```

A very comparable effect is repeated in 2015, where the drop in extra point percentage equates to much more accurate kicking than ever seen before from this distance.



###Is FG success rate dictated by the distance of kicks?

Kicking 10-19 yards
```{r echo=FALSE, message=FALSE, warning=FALSE}
library(gridExtra)

qplot(pct19,data=fgt,binwidth=.02)

```

A surprising amount of low percentages here. May because of the very few attempt after the goal posts were moved back in 1974.

Kicking 20-29 yards
```{r echo=FALSE, message=FALSE, warning=FALSE}
qplot(pct20,data=fgt,binwidth=.02)

```

A large number at 100% shows that the kicking in the twenties is all but a foramlity for the majority of NFL teams.

Kicking 30-39 yards
```{r echo=FALSE, message=FALSE, warning=FALSE}
qplot(pct30,data=fgt,binwidth=.02)
```

Pay attention to the scales on the sides the number of 100% teams dropped from nearly 800 in the twenties to below 300 in the thirties.


Kicking 40-49 yards
```{r echo=FALSE, message=FALSE, warning=FALSE}
qplot(pct40,data=fgt,binwidth=.02)

```

Now we are building to more normal distribtion as we get into the forties.

Kicking 50+ yards
```{r echo=FALSE, message=FALSE, warning=FALSE}
qplot(pct50,data=fgt,binwidth=.02)

```

With teams more cautious about even attempting kicks from more than 50 yards we find there are spikes at the 33,50 and 100 percentage points, due to kickers only having a handful of attempts per season from this range.


###Now we look at all the kicking ranges in one graph from the league wide data.

If what we saw in the histogram equates to a uniform sequence of closer kicks having a higher success rate than long kicks.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(x=year,y=pct19),data=subset(fgl,tm=='League Total'))+
  geom_line(color='black')+
  geom_line(aes(x=year,y=pct20),data=subset(fgl,tm=='League Total'),color='blue')+
  geom_line(aes(x=year,y=pct30),data=subset(fgl,tm=='League Total'),color='darkgreen')+
  geom_line(aes(x=year,y=pct40),data=subset(fgl,tm=='League Total'),color='orange')+
  geom_line(aes(x=year,y=pct50),data=subset(fgl,tm=='League Total'),color='red')+
  xlab('Year')+ylab("FG% from given ranges")+guides(color=guide_legend(labels='2015 PAT success rate'))+
  geom_text(mapping=aes(x=2016, y=.98, label='10-19'), size=4, angle=0, vjust=-0.4, hjust=0)+
  geom_text(mapping=aes(x=2016, y=.94, label='20-29'), size=4, angle=0, vjust=-0.4, hjust=0)+
  geom_text(mapping=aes(x=2016, y=.90, label='30-39'), size=4, angle=0, vjust=-0.4, hjust=0)+
  geom_text(mapping=aes(x=2016, y=.76, label='40-49'), size=4, angle=0, vjust=-0.4, hjust=0)+
  geom_text(mapping=aes(x=2016, y=.66, label='50+'), size=4, angle=0, vjust=-0.4, hjust=0)+
  xlim(c(1960,2018))


  
  

```

The result is expected, field goal percentage drops as the spot of the kick moves away from the goal posts.




###League-wide field goal attempts vs. field goal percentage. Here we'll see the relationship between the number of attempts and accuracy. 

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(fga,fgpct),data=subset(fgl,tm=='League Total'&year<2015))+
  geom_point()+stat_smooth(method='lm')
```

We see here the difficulties of pin pointing whether kickers are more accurate because they gain experiece through attempts or the better kickers are allowed more attempts because coaches have more confidence in them. 


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(fga,fgpct),data=subset(fgl,tm=='Avg Team'&year<2015))+
  geom_point()+stat_smooth(method='lm')
```

One issue with the previous plot is that the number of attempts were skewed because of the changing number of games each season. Now the correlation seems even more tenuous when number of games are controleed for.

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes((fga/g),fgpct),data=fgt)+
  geom_point(aes(color=decade))

```

We already know that kickers have become more accurate as time goes on. This figure was to see if kickers saw agreater number of attempts as time went on. It appears they do not for the most part.

###Here's a look at whether teams have similar distributions when you compare their field goal accracy over the years of the dataset.
```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(year,fgpct),data=fgt)+
  geom_point(aes(color=fgpct))+facet_wrap(~tm,ncol = 5)+ scale_colour_gradientn(colours=rainbow(4))+
  xlab("Year")+ylab("FG%")+ggtitle('FG Percentage By Team')
```

The upward trajectory of field goal accuracy is largely unaffected by the team. We notice that . We will look further into this in our final plots.


###Linear Regression Model

```{r echo=FALSE, message=FALSE, warning=FALSE}
library('bitops')
library('RCurl')
library('dplyr')
library("memisc")



#By team
m1<-lm(fgpct~year,data=fgt)
m2<-update(m1, ~ .+ I(X40a+X50a))
m3<-update(m2, ~ .+ xpct)
mtable(m1)
mtable(m2)
mtable(m3)



#By year
m1.1<-lm(fgpct~year,data=fglea)

mtable(m1.1)

ggplot(aes(year,fgpct),data=fglea)+geom_point()+
  geom_smooth(method='lm',formula=y~x)+
  xlab("Year")+ylab("FG%")+ggtitle("Year vs. FG% for League Averages")

```

Looking at the regression results we see that it is fairly difficult to make a model when the data set is decided by team. The r-squared values barely cracked 50%. In future studies, variables such as weather conditions and factoring in a kickers experience may be useful in predicting team success year to year. What was successful was looking at league wide field goal percentage through this regression. This is where the correlation between the year and field goal percentage is illustrated. With FG% as the independent variable and year as the dependent, 94% of the variation in the data was explained by this simple regression. This is a testament to the steady improvement of kicking overtime as kicking was taken more seriously over time and techniques were refined to make field goal kicking more accurate.





#Final Plots and Summary

##1.XP Percentage By Rule changes

graph of xp% refined by era
```{r echo=FALSE, message=FALSE, warning=FALSE}

ggplot(aes(year,xpct),data=fglea)+
  geom_point(aes(color=rules))+scale_x_continuous(breaks=seq(1960,2015,5))+
  geom_vline(xintercept = 1973,linetype=2)+
  geom_vline(xintercept = 2014,linetype=2)+ylab("XP Percentage")+
  geom_text(mapping=aes(x=1973, y=.9, label='goalposts moved behind endzone'), size=4, angle=90, vjust=-0.4, hjust=0)+
  geom_text(mapping=aes(x=2014, y=.9, label='PAT spot moved to 15-yard line'), size=4, angle=90, vjust=-0.4, hjust=0)


```

The history of the extra point can be separated into three different eras marked by structural changes to the rules of kicking.

1960-1973: The goalpost are on the goal line making the extra point a 10 yard attempt.
1974-2014:The goalpost were moved to the back of the end zone making the extra point a 20 yard attempt.
2015: The extra point attempt is moved back to the 15-yard line.

The largest drops in our data set are due to the first rule change in 1974 the extra ten yards caused a caused a drop in XP% of just under 6%. What may be just as unexpected is the subsequent jump in accuracy after this change. After 25 years NFL kickers were just as accurate from 20 yards as they were from ten yards in the 60's and early 70's.



##2.FG Percentage By Team


```{r echo=FALSE, message=FALSE, warning=FALSE}
library(plyr)

test <- ddply(fgt, "tm", summarise, mmp = median(fgpct))


ggplot(test,aes(x=factor(tm),y=mmp,fill=tm))+geom_bar(stat = "identity")+
  theme(panel.background = element_rect(fill = 'white'),legend.position = "none",
        plot.background = element_rect(fill = 'white'),
        panel.grid.major.y = element_line(color='grey'),
        axis.text.x=element_blank(),axis.ticks=element_blank(),
        axis.title.y=element_text(size=14,color="black", face="bold", vjust=2),
        title=element_text(size=20,color="black", face="bold", vjust=2))+
  scale_x_discrete(label=abbreviate)+xlab("Teams")+ylab("Median Field Goal Percentage")+
  scale_y_continuous(breaks=seq(0,1,.10))+ggtitle("Median FG Percentage Per Team")





```

This figure shows the reletive  uniformity of NFL team field goal percentage across the years. The team names are not shown to avoid clutter, but we can see the vast majority of teams are within 10 percent of each other. This shows the uniformity of kicking accuracy in the long run. Teams, for the most part, have not found a way to gain a competitive advantage over the long run. The top five teams are all formed in the last 20 years. 



##3.Distribution of FG Percentage Over Time
```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(aes(year,fgpct),data=fgt)+
  geom_jitter(alpha=.1)+xlim(c(1960,2015)) +
  geom_line(stat='summary',fun.y=median)+
  geom_line(stat='summary',fun.y=quantile, probs=.1, color='red')+
  geom_line(stat='summary',fun.y=quantile, probs=.9, color='dark green')+
  ylab("FG%")+xlab("Year")+ggtitle("FG Percentage vs. Year 1960-2015")+
  geom_text(mapping=aes(x=1983, y=.5, label='10th Percentile'), size=4, angle=0, vjust=-0.4, hjust=0)+
  geom_text(mapping=aes(x=1983, y=.68, label='Median'), size=4, angle=0, vjust=-0.4, hjust=0)+
  geom_text(mapping=aes(x=1983, y=.86, label='90th Percentile'), size=4, angle=0, vjust=-0.4, hjust=0)

  


```

The above figure looks at how the difference between the best and worst kickers have changed over the years. Surprisingly it hasn't changed as drastically as one might think. In the 1960s we observed very radical kicking accuracy. But ever since the 1970s the top 10 percent and lower 10 percent have been separated by 15-20 percentage points. Looking at the bottom 10% of kickers this year we see that they are kicking a similar percentage to the best kickers of the late 1970's. 






#Reflection

Overall, the project was more difficult than expected. The data wrangling from the Pro Football Reference website was easier than expected, but separating the data into the correct categories and finding the plots that told an interesting story proved to be more difficult. I learned that your analysis doesn't necessarily have to bring on a paradigm shift.

The remarkable fact of the history of NFL kicking is the mind numbing consistency in the distribution of the kickers' accuracy every year and their constant improvement for the last 55 years. Some may see this as boring data, but it might be one of the most miraculous improvements from any one position in sports. There just are no other circumstances where athletes double the production of their predecessors. This season's rule change moved the extra point back 13 yards, kickers responded by kicking at a higher percentage than they ever had in that range, as they did when rules were changed in the 1970's as well.

Of course, there is a limit to their production, the perfect 100 percent accurate kicking season has only been done 2 times in the history of NFL, if the 2015 season ended today, that number would double. The future is here and its going to be even more boring and predictable than ever.

As for future study, a more accurate data set could lead to more insightful conclusions. For example, statistics on field goals not from a range but a yard by yard distance marker and whether the kick was taken from the left, middle or right hash-mark on the field would be invaluable to future analysis. 

