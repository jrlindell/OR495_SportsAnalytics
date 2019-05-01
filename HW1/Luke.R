### HW 1 Brantley
### Purpose: Read in NFL data into new file with compacted data

library(XML)
library(utils)


### 1. declare master data structure
allData = data.frame
newData = data.frame

### 2. set URL address
begin_year=2003
end_year=2006

for(i in begin_year:end_year) {
  #Off-G-16,11
  OFF_G_url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=GAME_STATS&defensiveStatisticCategory=null&season=",i,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  #DEF-G-12,16
  DEF_G_url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=GAME_STATS&season=",i,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")  
  #DEF-R-9,14,17
  DEF_R_url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=RUSHING&season=",i,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  #DEF-P-11,14
  DEF_P_url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=OPP&offensiveStatisticCategory=null&defensiveStatisticCategory=TEAM_PASSING&season=",i,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  #OFF-P-9,11,14
  OFF_P_url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=TEAM_PASSING&defensiveStatisticCategory=null&season=",i,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  #OFF-R-9,17
  OFF_R_url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=RUSHING&defensiveStatisticCategory=null&season=",i,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")
  #RET TD-9+10
  RET_url = paste("http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=TOUCHDOWNS&defensiveStatisticCategory=null&season=",i,"&seasonType=REG&tabSeq=2&qualified=false&Submit=Go",sep="")

  WIN_url = paste("http://www.nfl.com/standings?category=div&season=",i,"-REG&split=Overall",sep="")

  ### 3. grab entire webpage
  OFF_G_sitePage = htmlParse(OFF_G_url)
  DEF_G_sitePage = htmlParse(DEF_G_url)
  OFF_P_sitePage = htmlParse(OFF_P_url)
  DEF_P_sitePage = htmlParse(DEF_P_url)  
  OFF_R_sitePage = htmlParse(OFF_R_url)
  DEF_R_sitePage = htmlParse(DEF_R_url)
  RET_url_sitePage = htmlParse(RET_url)
  WIN_url_sitePage = htmlParse(WIN_url)
  #class(WIN_url_sitePage) 


  ### 4. extract table you want from webpage
  OFF_G_Stats = readHTMLTable(OFF_G_sitePage, header=F)$result
  DEF_G_Stats = readHTMLTable(DEF_G_sitePage, header=F)$result
  OFF_P_Stats = readHTMLTable(OFF_P_sitePage, header=F)$result
  DEF_P_Stats = readHTMLTable(DEF_P_sitePage, header=F)$result
  OFF_R_Stats = readHTMLTable(OFF_R_sitePage, header=F)$result
  DEF_R_Stats = readHTMLTable(DEF_R_sitePage, header=F)$result
  RET_Stats = readHTMLTable(RET_url_sitePage, header=F)$result
  WIN_Stats = readHTMLTable(WIN_url_sitePage, header=F)$result
  #class(Stats)

  OFF_G_temp = readHTMLTable(OFF_G_sitePage, header=T, trim=T)$result
  DEF_G_temp = readHTMLTable(DEF_G_sitePage, header=T, trim=T)$result
  OFF_P_temp = readHTMLTable(OFF_P_sitePage, header=T, trim=T)$result
  DEF_P_temp = readHTMLTable(DEF_P_sitePage, header=T, trim=T)$result
  OFF_R_temp = readHTMLTable(OFF_R_sitePage, header=T, trim=T)$result
  DEF_R_temp = readHTMLTable(DEF_R_sitePage, header=T, trim=T)$result
  RET_temp = readHTMLTable(RET_url_sitePage, header=T, trim=T)$result
  WIN_temp = readHTMLTable(WIN_url_sitePage, header=T, trim=T)$result
  
  names(OFF_G_Stats) = gsub("\n","",names(OFF_G_temp))
  names(DEF_G_Stats) = gsub("\n","",names(DEF_G_temp))
  names(OFF_P_Stats) = gsub("\n","",names(OFF_P_temp))
  names(DEF_P_Stats) = gsub("\n","",names(DEF_P_temp))
  names(OFF_R_Stats) = gsub("\n","",names(OFF_R_temp))
  names(DEF_R_Stats) = gsub("\n","",names(DEF_R_temp))
  names(RET_Stats) = gsub("\n","",names(RET_temp))
  names(WIN_Stats) = gsub("\n","",names(WIN_temp))
  #names(Stats)

  ### 6. Store necessary data from web tables
  
  Game_Stats=merge(OFF_G_Stats[,c(2,5,12,16)],DEF_G_Stats[,c(2,5,12,16)],WIN_Stats[,2],by=1)
  Pass_Stats=merge(OFF_P_Stats[,c(2,11,14)],DEF_P_Stats[,c(2,11,14)],by=1)
  Run_Stats=merge(OFF_R_Stats[,c(2,9,17)],DEF_R_Stats[,c(2,9,17)],by=1)
  
  table_year=c(rep(i,32))
  RET_Stats<-cbind(RET_Stats,Year=table_year)
  RET_Stats<-cbind(RET_Stats,RET_TOT=as.numeric(as.character(RET_Stats$Ret))+as.numeric(as.character(RET_Stats$Def)))
  TD_Stats<-RET_Stats[,c(2,11,12)]
  
  R_P_Stats=merge(Pass_Stats,Run_Stats,by=1)
  Oth_Stats=merge(Game_Stats,TD_Stats,by=1)
  
  
  if(i==begin_year) {
    allData =  merge(R_P_Stats,Oth_Stats,by=1)
    allData<-cbind(allData,Scoring_Margin=as.numeric(as.character(allData$TotPts.x))-as.numeric(as.character(allData$TotPts.y)))
  }
  else {
    allData=rbind(allData,newData = merge(R_P_Stats,Oth_Stats,by=1))
    allData<-cbind(allData,Scoring_Margin=as.numeric(as.character(allData$TotPts.x))-as.numeric(as.character(allData$TotPts.y)))
  }
  
}


### 7. Generalize these steps to gather multiple tables over multiple years



### 8. save result to csv file for later use
write.csv(<ENTER CODE HERE>)