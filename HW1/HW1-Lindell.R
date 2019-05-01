###################### Part 1 ############################

### Purpose: Read in NFL data into new file with compacted data

library(XML)
library(utils)


### 1. declare master data structure
allData = data.frame
newData = data.frame

### 2. set URL address
firstyear=2003
lastyear=2006

#the loop gathers data for multiple years and stores them in a data frame called allData
for(year in firstyear:lastyear) {
  
  OffGameUrl = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=GAME_STATS&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  DefGameUrl = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=GAME_STATS&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")  
  
  DefRushUrl = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=RUSHING&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
 
  DefPassUrl = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=TEAM_PASSING&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  OffPassUrl = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=TEAM_PASSING&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  OffRushUrl = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=RUSHING&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  RetUrl = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=TOUCHDOWNS&defensiveStatisticCategory=null&season=",year,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  
  
  ### 3. grab entire webpage
  OffGamesitePage = htmlParse(OffGameUrl)
  DefGamesitePage = htmlParse(DefGameUrl)
  OffPasssitePage = htmlParse(OffPassUrl)
  DefPasssitePage = htmlParse(DefPassUrl)  
  OffRushsitePage = htmlParse(OffRushUrl)
  DefRushsitePage = htmlParse(DefRushUrl)
  RetsitePage = htmlParse(RetUrl)
  
  
  WinPCTUrl = paste("http://www.nfl.com/standings?category=league&season=",year,"-REG&split=Overall",sep="")
  WinPCTsitepage = htmlParse(WinPCTUrl)
    
  Teams = xpathSApply(WinPCTsitepage,"//table//tr//td[1]",xmlValue)
  WinPCT = xpathSApply(WinPCTsitepage,"//table//tr//td[7]",xmlValue)
  
  Teams = gsub("\\t","",Teams)
  Teams = gsub("\\n","",Teams)
  Teams = gsub("x- ","",Teams)
  Teams = gsub("y- ","",Teams)
  Teams = gsub("z- ","",Teams)
  Teams = gsub("*- ","",Teams)
  Teams = gsub("\\*","",Teams)
  Teams <- Teams[c(3:34)]
  WinPCT = gsub("\\t","",WinPCT)
  WinPCT = gsub("\\n","",WinPCT)
  WinPCT <- WinPCT[c(2:33)]
  TeamWinPCT = cbind(Teams,WinPCT)
  
  
  ### 4. extract table you want from webpage
  OffGameStats = readHTMLTable(OffGamesitePage, header=F)$result
  DefGameStats = readHTMLTable(DefGamesitePage, header=F)$result
  OffPassStats = readHTMLTable(OffPasssitePage, header=F)$result
  DefPassStats = readHTMLTable(DefPasssitePage, header=F)$result
  OffRushStats = readHTMLTable(OffRushsitePage, header=F)$result
  DefRushStats = readHTMLTable(DefRushsitePage, header=F)$result
  RetStats = readHTMLTable(RetsitePage, header=F)$result
  #class(Stats)
  
  OffGameTemp = readHTMLTable(OffGamesitePage, header=T, trim=T)$result
  DefGameTemp = readHTMLTable(DefGamesitePage, header=T, trim=T)$result
  OffPassTemp = readHTMLTable(OffPasssitePage, header=T, trim=T)$result
  DefPassTemp = readHTMLTable(DefPasssitePage, header=T, trim=T)$result
  OffRushTemp = readHTMLTable(OffRushsitePage, header=T, trim=T)$result
  DefRushTemp = readHTMLTable(DefRushsitePage, header=T, trim=T)$result
  RetTemp = readHTMLTable(RetsitePage, header=T, trim=T)$result
  
  names(OffGameStats) = gsub("\n","",names(OffGameTemp))
  names(DefGameStats) = gsub("\n","",names(DefGameTemp))
  names(OffPassStats) = gsub("\n","",names(OffPassTemp))
  names(DefPassStats) = gsub("\n","",names(DefPassTemp))
  names(OffRushStats) = gsub("\n","",names(OffRushTemp ))
  names(DefRushStats) = gsub("\n","",names(DefRushTemp))
  names(RetStats) = gsub("\n","",names(RetTemp))
  
  
  #The next line merges offensive and defensive game stats into one table by team name.
  GameStats=merge(OffGameStats[,c(2,5,12,16)],DefGameStats[,c(2,5,12,16)],by=1)
  
  #The next line merges offensive and defensive passing stats into one table by team name.
  PassStats=merge(OffPassStats[,c(2,11,14)],DefPassStats[,c(2,11,14)],by=1)
  
  #The next line merges offensive and defensive rushing stats into one table by team name.
  RushStats=merge(OffRushStats[,c(2,9,17)],DefRushStats[,c(2,9,17)],by=1)
  
  #This block add a year column to the return stats which will be added to the entire table
  SeasonYear=c(rep(year,32))
  RetStats<-cbind(RetStats,Year=SeasonYear)
  
  RetStats<-cbind(RetStats,RET_TOT=as.numeric(as.character(RetStats$Ret))+as.numeric(as.character(RetStats$Def)))
  TDStats<-merge(RetStats[,c(2,11,12)],TeamWinPCT,by=1)
  
  
  RushPassStats=merge(PassStats,RushStats,by=1)
  ElseStats=merge(GameStats,TDStats,by=1)
  
  #allData$Differ = c(as.numeric(as.character((allData$"OffTotPts")))-as.numeric(as.character(allData$"DefTotPts")))
  
  if(year==firstyear) {
    allData =  merge(RushPassStats,ElseStats,by=1)
  }
  else {
    newData= merge(RushPassStats,ElseStats,by=1)
    #newData$PointDiff = c(as.numeric(as.character(newData$"OffTotPts"))-as.numeric(as.character(newData$"DefTotPts")))
    allData = rbind(allData,newData)
    
    
  }
}

#gives the alldata column names
colnames(allData) <- c( "Team" , "OffPassAvg", "OffInt" , "DefPassAvg" , "DefInt" , "OffRushAvg" , "OffFum" , "DefRushAvg", "DefFum" , "OffTotPts" , "Off3rdPct" , "OffPen" , "DefTotPts" , "Def3rdPct" , "DefPen" , "Year","RetTot","WinPCT")
allData$PointDiff = c(as.numeric(as.character(allData$"OffTotPts"))-as.numeric(as.character(allData$"DefTotPts")))

write.csv(allData,file = "NFL-Assignment1.csv")

################################## Part 2 #######################################

summary(lm(as.numeric(as.character(allData$WinPCT))~as.numeric(as.character(allData$PointDiff))))

################################## Part 3 ########################################

