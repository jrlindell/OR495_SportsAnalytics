###################### Part 1 ############################
##Name: C2C Jacob Lindell
##Documentation: Received EI from Professor Warner; he helped with adding the return column and the 
# defense td column into a new column. Luke Brantley and I worked on the overall structure of the
#project together, I was having some trouble just figuring out where to begin, and that is where Luke helped.
# C2C helped with Part 3 and Part 4, mainly just helping figure out the lm function. Cadet Haug and I
# were helping each other periodically with some of the formatting and just the general language of R.


## Purpose- Read in NFL data from the internet into a new file, then compact the data

library(XML)
library(utils)
library(fastR)


### 1. declare master data structures
all_Data = data.frame
new_Data = data.frame

### 2. URL address
first_year=2003
last_year=2006

#This loop gathers data for multiple years from the internet and stores the data into different data files, ultimately being combined into one data file (all_Data)
for(year in first_year:last_year) {
  
  Off_Game_Url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=GAME_STATS&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Def_Game_Url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=GAME_STATS&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")  
  
  Def_Rushing_Url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=RUSHING&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
 
  Def_Passing_Url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=TEAM_PASSING&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Off_Passing_Url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=TEAM_PASSING&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Off_Rushing_Url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=RUSHING&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Ret_Url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=TOUCHDOWNS&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  
  ### 3. grab entire webpage
  Off_Game_sitePage = htmlParse(Off_Game_Url)
  Def_Game_sitePage = htmlParse(Def_Game_Url)
  Off_Passing_sitePage = htmlParse(Off_Passing_Url)
  Def_Passing_sitePage = htmlParse(Def_Passing_Url)  
  Off_Rushing_sitePage = htmlParse(Off_Rushing_Url)
  Def_Rushing_sitePage = htmlParse(Def_Rushing_Url)
  Ret_sitePage = htmlParse(Ret_Url)
  
  
  Winning_PCT_Url = paste("http://www.nfl.com/standings?category=league&season=",year,"-REG&split=Overall",sep="")
  Winning_PCT_sitepage = htmlParse(Winning_PCT_Url)
    
  Teams = xpathSApply(Winning_PCT_sitepage,"//table//tr//td[1]",xmlValue)
  Winning_PCT = xpathSApply(Winning_PCT_sitepage,"//table//tr//td[7]",xmlValue)
  
  Teams = gsub("\\t","",Teams)
  Teams = gsub("\\n","",Teams)
  Teams = gsub("z- ","",Teams)
  Teams = gsub("*- ","",Teams)
  Teams = gsub("\\*","",Teams)
  Teams = gsub("x- ","",Teams)
  Teams = gsub("y- ","",Teams)

  Teams <- Teams[c(3:34)]
  
  Winning_PCT = gsub("\\t","",Winning_PCT)
  Winning_PCT = gsub("\\n","",Winning_PCT)
  Winning_PCT <- Winning_PCT[c(3:34)]
  Team_Winning_PCT = cbind(Teams,Winning_PCT)
  
  
  ### 4. extract table you want from webpage
  Off_Game_Stats = readHTMLTable(Off_Game_sitePage, header=F)$result
  Def_Game_Stats = readHTMLTable(Def_Game_sitePage, header=F)$result
  Off_Passing_Stats = readHTMLTable(Off_Passing_sitePage, header=F)$result
  Def_Passing_Stats = readHTMLTable(Def_Passing_sitePage, header=F)$result
  Off_Rushing_Stats = readHTMLTable(Off_Rushing_sitePage, header=F)$result
  Def_Rushing_Stats = readHTMLTable(Def_Rushing_sitePage, header=F)$result
  Ret_Stats = readHTMLTable(Ret_sitePage, header=F)$result
  #class(Off_Game_Stats)
  
  Off_Game_Temp = readHTMLTable(Off_Game_sitePage, header=T, trim=T)$result
  Def_Game_Temp = readHTMLTable(Def_Game_sitePage, header=T, trim=T)$result
  Off_Passing_Temp = readHTMLTable(Off_Passing_sitePage, header=T, trim=T)$result
  Def_Passing_Temp = readHTMLTable(Def_Passing_sitePage, header=T, trim=T)$result
  Off_Rushing_Temp = readHTMLTable(Off_Rushing_sitePage, header=T, trim=T)$result
  Def_Rushing_Temp = readHTMLTable(Def_Rushing_sitePage, header=T, trim=T)$result
  Ret_Temp = readHTMLTable(Ret_sitePage, header=T, trim=T)$result
  
  #get the column names of the tables online in order to use them in the tables
  names(Off_Game_Stats) = gsub("\n","", names(Off_Game_Temp))
  names(Def_Game_Stats) = gsub("\n","", names(Def_Game_Temp))
  names(Off_Passing_Stats) = gsub("\n","", names(Off_Passing_Temp))
  names(Def_Passing_Stats) = gsub("\n","", names(Def_Passing_Temp))
  names(Off_Rushing_Stats) = gsub("\n","", names(Off_Rushing_Temp ))
  names(Def_Rushing_Stats) = gsub("\n","", names(Def_Rushing_Temp))
  names(Ret_Stats) = gsub("\n","", names(Ret_Temp))
  
  
  #This is where the program merges the offensive game stats with the defensive game stats and organizes it by team name
  Game_Stats=merge(Off_Game_Stats[,c(2,5,12,16)],Def_Game_Stats[,c(2,5,12,16)],by=1)
  
  #This is where the program merges the offensive passing stats with the defensive passing stats and organizes it by team name
  Passing_Stats=merge(Off_Passing_Stats[,c(2,11,14)],Def_Passing_Stats[,c(2,11,14)],by=1)
  
  #This is where the program merges the offensive rush stats with the defensive rush stats and organizes it by team name
  Rushing_Stats=merge(Off_Rushing_Stats[,c(2,9,17)],Def_Rushing_Stats[,c(2,9,17)],by=1)
  
  #This helps organize the data because it displays the year for each iteration of the loop
  Season_Year=c(rep(year,32))
  Ret_Stats<-cbind(Ret_Stats,Year=Season_Year)
  
  #This allows us to put return td and defensive td into one column
  Ret_Stats<-cbind(Ret_Stats,RET_TOT=as.numeric(as.character(Ret_Stats$Ret))+as.numeric(as.character(Ret_Stats$Def)))
  TD_Stats<-merge(Ret_Stats[,c(2,11,12)],Team_Winning_PCT,by=1)
  
  
  Rushing_Passing_Stats=merge(Passing_Stats,Rushing_Stats,by=1)
  Other_Stats=merge(Game_Stats,TD_Stats,by=1)
  
  
  if(year==first_year) {
    all_Data =  merge(Rushing_Passing_Stats,Other_Stats,by=1)
  }
  else {
    new_Data= merge(Rushing_Passing_Stats,Other_Stats,by=1)
    #new_Data$PointDiff = c(as.numeric(as.character(new_Data$"OffTotPts"))-as.numeric(as.character(new_Data$"DefTotPts")))
    all_Data = rbind(all_Data,new_Data)
    
    
  }
}

#This gives the column names for the all_Data dataset
colnames(all_Data) <- c( "Team" , "Off_Passing_Avg", "Off_Int" , "Def_Passing_Avg" , "Def_Int" , "Off_Rushing_Avg" , "Off_Fum" , "Def_Rushing_Avg", "Def_Fum" , "Off_Total_Pts" , "Off_3rd_Dn_Pct" , "Off_Pens" , "Def_Total_Pts" , "Def_3rd_Dn_Pct" , "Def_Pens" , "Year","Ret_Total","Winning_PCT")
all_Data$Point_Diff = c(as.numeric(as.character(all_Data$"Off_Total_Pts"))-as.numeric(as.character(all_Data$"Def_Total_Pts")))

write.csv(all_Data,file = "HW1.csv")

#For some reason, there are some of my datasets that do not read in all 32 observations.
#It was working, and then I did something that made it not work, and now it does not work. The intercept
# matrix below should be matrix(1,128,1) but my all_Data only reads in 112 observations for a reason
#I have yet to figure out, so in order to get the rest to work it must be a matrix that is (1,112,1).

################################## Part 2 #######################################

summary(lm(as.numeric(as.character(all_Data$Winning_PCT))~as.numeric(as.character(all_Data$Point_Diff))))
#This gives us a Multiple R-squared value of 0.828, which means that winning percentage and point differential
#can be used to define each other

################################## Part 3 ########################################

Intercept=matrix(1,112,1)

# Read in X and Y from the csv as a matrix
nfl_data = read.csv("HW1.csv",header = TRUE)

# This is the regression from the book
book.model = lm(Point_Diff~.-Intercept, data=nfl_data)
summary(book.model)

# Are the regressors linearly independent?
pairs(nfl_data[,c(-1,-2)])
#These graphs do not portray any noticeable pattern or show any dependence, the regressors can be said
# to be linearly independent.

### Errors are unbiased (mean=0) 
# use residuals as an estimate for errors
mean(book.model$residuals)
# This number is basically 0

### Errors are Normally distributed
# test Normality of Residuals
qqPlot(book.model, main="QQ Plot")
# Errors seem to be normally distributed

### Errors have constant variance
# plot residuals vs fitted values, want to see a "shotgun" spread
# if variation (vertical) changesas you move horzontally, then the error is not constant
plot(book.model$fitted.value,book.model$residuals)
# This spread looks relatively like a shotgun spread, so the errors have constant variance



################################## Part 4 ########################################

#The data we did originally in Part 1 included offensive and defensive third down rates, so NoThird
# is the new data that does not include third down
No_Third= c(all_Data[,c(1:10,12,13,15:19)])
View(No_Third)

write.csv(No_Third, file = "Part_4.csv")

# Read in X and Y from the csv as a matrix
nfl_data2 = read.csv("Part_4.csv",header = TRUE)

# do regression from the book
book2.model = lm(Winning_PCT~.-Point_Diff, data=nfl_data2)
summary(book2.model)

# Are the regressors linearly independent?
pairs(nfl_data2[,c(-1,-2)])
#These graphs do not portray any noticeable pattern or show any dependence, the regressors can be said
# to be linearly independent.

### Errors are unbiased (mean=0) 
# use residuals as an estimate for errors
mean(book2.model$residuals)
# This number is basically 0

### Errors are Normally distributed
# test Normality of Residuals
qqPlot(book2.model, main="QQ Plot")
# Errors seem to be normally distributed

### Errors have constant variance
# plot residuals vs fitted values, want to see a "shotgun" spread
# if variation (vertical) changesas you move horzontally, then the error is not constant
plot(book2.model$fitted.value,book2.model$residuals)
# This spread looks relatively like a shotgun spread, so the errors have constant variance

###Does it improve the model?

# This is the model without third down rate
book2.model = lm(Winning_PCT~.-Point_Diff, data=nfl_data2)
summary(book2.model)
## Residuals:
##Min= -0.15  1Q= -0.04 Median= -0.02 3Q= 0.034 Max= 0.17
##Multiple R-squared: 0.902, p-value:<2.2e-16

# This is the model with third down rate
book3.model = lm(Winning_PCT~.-Point_Diff, data=nfl_data)
summary(book3.model)
##Residuals:
## Min= -0.17 1Q= -0.04 Median= -0.005  3Q= 0.037 Max= 0.17
##Multiple R-squared: 0.904, p-value: <2.2e-16

## Based on the R-squared and p-values, the third down rate does not affect the model very much,
## 0.02 to be exact


################################## Part 5 ########################################

# For this part, it was easier to just change the begin year and ending year of part 1, since our
# model already included third down rate. Looking at the information and thinking logically, more 
# info/data will always help make the model more accurate because it has now seen many different
# years that had varying occurences that were not seen from 2003-2006.
## The problem that we are having is that the Texans did not become a team until 2002, so when
# we try and row bind the years we reach an error with the colnames equation, and there is a problem
# because of the differing rows when the Texans were not a team. When I run the regression from 2002
# to 2014, the dataset worked. When talking to my classmate C2C Hatton, when he did the 2000-2014,
# his dataset worked, but for some unknown reason I cannot get mine to work with differing rows.At one
#point I got it to work, and the R-squared values were fairly similar,approximately 2%,
#which shows that there is not that much difference between 4 years and 15 years.


all_Data5 = data.frame
new_Data5 = data.frame

### 2. URL address
# These years are for the new dataset for part 5
first_year=2000
last_year=2014

#This loop gathers data for multiple years from the internet and stores the data into different data files, ultimately being combined into one data file (all_Data)
for(year in first_year:last_year) {
  
  Off_Game_Url5 = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=GAME_STATS&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Def_Game_Url5 = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=GAME_STATS&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")  
  
  Def_Rushing_Url5 = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=RUSHING&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Def_Passing_Url5 = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=TEAM_PASSING&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Off_Passing_Url5 = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=TEAM_PASSING&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Off_Rushing_Url5 = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=RUSHING&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  Ret_Url5 = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=TOUCHDOWNS&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  
  ### 3. grab entire webpage
  Off_Game_sitePage5 = htmlParse(Off_Game_Url5)
  Def_Game_sitePage5 = htmlParse(Def_Game_Url5)
  Off_Passing_sitePage5 = htmlParse(Off_Passing_Url5)
  Def_Passing_sitePage5 = htmlParse(Def_Passing_Url5)  
  Off_Rushing_sitePage5 = htmlParse(Off_Rushing_Url5)
  Def_Rushing_sitePage5 = htmlParse(Def_Rushing_Url5)
  Ret_sitePage5 = htmlParse(Ret_Url5)
  
  
  Winning_PCT_Url5 = paste("http://www.nfl.com/standings?category=league&season=",year,"-REG&split=Overall",sep="")
  Winning_PCT_sitepage5 = htmlParse(Winning_PCT_Url5)
  
  Teams = xpathSApply(Winning_PCT_sitepage5,"//table//tr//td[1]",xmlValue)
  Winning_PCT5 = xpathSApply(Winning_PCT_sitepage5,"//table//tr//td[7]",xmlValue)
  
  Teams = gsub("\\t","",Teams)
  Teams = gsub("\\n","",Teams)
  Teams = gsub("z- ","",Teams)
  Teams = gsub("*- ","",Teams)
  Teams = gsub("\\*","",Teams)
  Teams = gsub("x- ","",Teams)
  Teams = gsub("y- ","",Teams)
  
  Teams <- Teams[c(3:34)]
  
  Winning_PCT5 = gsub("\\t","",Winning_PCT5)
  Winning_PCT5 = gsub("\\n","",Winning_PCT5)
  Winning_PCT5 <- Winning_PCT5[c(3:34)]
  Team_Winning_PCT5 = cbind(Teams,Winning_PCT5)
  
  
  ### 4. extract table you want from webpage
  Off_Game_Stats5 = readHTMLTable(Off_Game_sitePage5, header=F)$result
  Def_Game_Stats5 = readHTMLTable(Def_Game_sitePage5, header=F)$result
  Off_Passing_Stats5 = readHTMLTable(Off_Passing_sitePage5, header=F)$result
  Def_Passing_Stats5 = readHTMLTable(Def_Passing_sitePage5, header=F)$result
  Off_Rushing_Stats5 = readHTMLTable(Off_Rushing_sitePage5, header=F)$result
  Def_Rushing_Stats5 = readHTMLTable(Def_Rushing_sitePage5, header=F)$result
  Ret_Stats5 = readHTMLTable(Ret_sitePage5, header=F)$result
  #class(Off_Game_Stats)
  
  Off_Game_Temp5 = readHTMLTable(Off_Game_sitePage5, header=T, trim=T)$result
  Def_Game_Temp5 = readHTMLTable(Def_Game_sitePage5, header=T, trim=T)$result
  Off_Passing_Temp5 = readHTMLTable(Off_Passing_sitePage5, header=T, trim=T)$result
  Def_Passing_Temp5 = readHTMLTable(Def_Passing_sitePage5, header=T, trim=T)$result
  Off_Rushing_Temp5 = readHTMLTable(Off_Rushing_sitePage5, header=T, trim=T)$result
  Def_Rushing_Temp5 = readHTMLTable(Def_Rushing_sitePage5, header=T, trim=T)$result
  Ret_Temp5 = readHTMLTable(Ret_sitePage5, header=T, trim=T)$result
  
  #get the column names of the tables online in order to use them in the tables
  names(Off_Game_Stats5) = gsub("\n","", names(Off_Game_Temp5))
  names(Def_Game_Stats5) = gsub("\n","", names(Def_Game_Temp5))
  names(Off_Passing_Stats5) = gsub("\n","", names(Off_Passing_Temp5))
  names(Def_Passing_Stats5) = gsub("\n","", names(Def_Passing_Temp5))
  names(Off_Rushing_Stats5) = gsub("\n","", names(Off_Rushing_Temp5))
  names(Def_Rushing_Stats5) = gsub("\n","", names(Def_Rushing_Temp5))
  names(Ret_Stats5) = gsub("\n","", names(Ret_Temp5))
  
  
  #This is where the program merges the offensive game stats with the defensive game stats and organizes it by team name
  Game_Stats5=merge(Off_Game_Stats5[,c(2,5,12,16)],Def_Game_Stats5[,c(2,5,12,16)],by=1)
  
  #This is where the program merges the offensive passing stats with the defensive passing stats and organizes it by team name
  Passing_Stats5=merge(Off_Passing_Stats5[,c(2,11,14)],Def_Passing_Stats5[,c(2,11,14)],by=1)
  
  #This is where the program merges the offensive rush stats with the defensive rush stats and organizes it by team name
  Rushing_Stats5=merge(Off_Rushing_Stats5[,c(2,9,17)],Def_Rushing_Stats5[,c(2,9,17)],by=1)
  
  #This loop should help split up the time when the Texans were not a team, but for some reason I continue
  #to get syntax errors, which I have been unable to figure out
  
  if(year<2002){
    season_year2=c(rep(year,31))
  }else{
    season_year2=c(rep(year,32))
  
  #This helps organize the data because it displays the year for each iteration of the loop
 
  #This allows us to put return td and defensive td into one column
  Ret_Stats5<-cbind(Ret_Stats5,RET_TOT=as.numeric(as.character(Ret_Stats5$Ret))+as.numeric(as.character(Ret_Stats5$Def)))
  TD_Stats5<-merge(Ret_Stats5[,c(2,11,12)],Team_Winning_PCT5,by=1)
  
  
  Rushing_Passing_Stats5=merge(Passing_Stats5,Rushing_Stats5,by=1)
  Other_Stats5=merge(Game_Stats5,TD_Stats5,by=1)
  
  
  if(year==first_year) {
    all_Data5 =  merge(Rushing_Passing_Stats5,Other_Stats5,by=1)
  }
  else {
    new_Data5= merge(Rushing_Passing_Stats5,Other_Stats5,by=1)
    #new_Data$PointDiff = c(as.numeric(as.character(new_Data$"OffTotPts"))-as.numeric(as.character(new_Data$"DefTotPts")))
    all_Data5 = rbind(all_Data5,new_Data5)
    
    
  }
}

#This gives the column names for the all_Data5 dataset
colnames(all_Data5) <- c( "Team" , "Off_Passing_Avg", "Off_Int" , "Def_Passing_Avg" , "Def_Int" , "Off_Rushing_Avg" , "Off_Fum" , "Def_Rushing_Avg", "Def_Fum" , "Off_Total_Pts" , "Off_3rd_Dn_Pct" , "Off_Pens" , "Def_Total_Pts" , "Def_3rd_Dn_Pct" , "Def_Pens" , "Year","Ret_Total","Winning_PCT")
all_Data5$Point_Diff = c(as.numeric(as.character(all_Data5$"Off_Total_Pts"))-as.numeric(as.character(all_Data5$"Def_Total_Pts")))

write.csv(all_Data,file = "HW1Pt5.csv")

################################## Part 6 ########################################

#An NFL team could use this analysis by figuring out what is more important to winning percentage/
#what has a higher correlation to winning percentage; passing vs. rushing, def. passing vs. def.
#rushing. I would make a graph that compared the winning percentage of each team to the stats and tried
# to figure out which stat was most important to a winning team. I was unable to figure out how to make such a graph,
#which is probably not very helpful that some of my datasets did not even have all 32 observations.

