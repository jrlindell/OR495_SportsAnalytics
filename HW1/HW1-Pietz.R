library(XML)
library(utils)
library(car)

##Part 1

allTeamStats=data.frame()
for (Yr in 2000:2014){
  #Team Offense Data
  url=paste("http://www.nfl.com/stats/categorystats?archive=true&conference=
            null&role=TM&offensiveStatisticCategory=GAME_STATS&defensiveStatistic
            Category=null&season=",YR,"&seasonType=REG&tabSeq=2&qualified=false
            &Submit=Go",sep="")
  sitePage=htmlParse(url)
  oTeamYearStats = readHTMLParse(sitePage, trim=T, header=F, which=1)
  HeadTable=readHTMLTable(sitePage, trim=T, header=T, which=1)
  names(oTeamYearStats)=names(HeadTable)
  
  names(oTeamYearStats)[1]= "Year"
  oTeamYearStats[,1]= Yr
  
  #Offense Passing Data
  url=paste("http://www.nfl.com/stats/categorystats?archive=true&conference=
         null&role=TM&offensiveStatisticCategory=TEAM_PASSING&defensiveStatistic
         Category=null&season=",Yr,"&seasonType=REG&tabSeq=2&qualified=false
         &Submit=Go",sep="")
  sitePage=htmlParse(url)
  tmp = readHTMLParse(sitePage, trim=T, header=F, which=1)
  HeadTable=readHTMLTable(sitePage, trim=T, header=T, which=1)
  names(tmp)=names(HeadTable)
  
  names(tmp)[7]= "oPassAtt"
  names(tmp)[10]="oPassYds"
  names(tmp)[14]="oINT"
  names(tmp)[20]="oSack"
  oTeamYearStats = merge(oTeamYearStats, tmp[,c(2,7,10,14,20)],by="Team")
  
  #Offense Rushing Data
  url= paste("http://www.nfl.com/stats/categorystats?archive=true&conference=
             null&role=TM&offensiveStatisticCategory=RUSHING&defensiveStatistic
             Category=null&season=",Yr,"&seasonType=REG&tabSeq=2&qualified=false
             &Submit=Go",sep="")
  sitePage=htmlParse(url)
  tmp = readHTMLParse(sitePage, trim=T, header=F, which=1)
  HeadTable=readHTMLTable(sitePage, trim=T, header=T, which=1)
  names(tmp)=names(HeadTable)
  names(tmp)[6]= "oRushAtt"
  names(tmp)[8]= "oRushYds"
  oTeamYearStats = merge(oTeamYearStats, tmp[,c(2,6,8)], by="Team")
  
  #Offense return Scoring Data
  url= paste("http://www.nfl.com/stats/categorystats?archive=true&conference=
             null&role=TM&offensiveStatisticCategory=TOUCHDOWNS&defensiveStatistic
             Category=null&season=",Yr,"&seasonType=REG&tabSeq=2&qualified=false
             &Submit=Go",sep="")
  sitePage=htmlParse(url)
  tmp = readHTMLParse(sitePage, trim=T, header=F, which=1)
  HeadTable=readHTMLTable(sitePage, trim=T, header=T, which=1)
  names(tmp)=names(HeadTable)
  names(tmp)[2]= "Team"
  names(tmp)[10]= "oPuntTD"
  names(tmp)[11]= "oKickTD"
  names(tmp)[12]="oIntTD"
  names(tmp)[13]= "oFumTD"
  oTeamYearStats = merge(oTeamYearStats, tmp[,c(2,10,11,12,13)], by="Team")
  
  #Team Defense Data
  url= paste("http://www.nfl.com/stats/categorystats?archive=true&conference=
            null&role=OPP&offensiveStatisticCategory=null&defensiveStatistic
            Category=GAME_STATS&season=",Yr,"&seasonType=REG&tabSeq=2&qualified=false
            &Submit=Go",sep="")  
  sitePage=htmlParse(url)
  dTeamYearStats = readHTMLParse(sitePage, trim=T, header=F, which=1)
  HeadTable=readHTMLTable(sitePage, trim=T, header=T, which=1)
  names(dTeamYearStats)=names(HeadTable)
  
  dTeamYearStats = dTeamYearStats[-1]
  
  for (j in 1:length(names(dTeamYearStats))){
    if (names(dTeamYearStats)[j] !="Team"){
      names(dTeamYearStats)[j] = paste("d",names(dTeamYearStats)[j], sep="")
    }
  }
  
  #Defense Passing Stats
  url= paste("http://www.nfl.com/stats/categorystats?archive=true&conference=
            null&role=OPP&offensiveStatisticCategory=null&defensiveStatistic
            Category=TEAM_PASSING&season=",Yr,"&seasonType=REG&tabSeq=2&qualified=false
            &Submit=Go",sep="")
  sitePage=htmlParse(url)
  tmp = readHTMLParse(sitePage, trim=T, header=F, which=1)
  HeadTable=readHTMLTable(sitePage, trim=T, header=T, which=1)
  names(tmp)=names(HeadTable)
  names(tmp)[7]="dPassAtt"
  names(tmp)[10]="dPassYds"
  names(tmp)[14]="dINT"
  names(tmp)[20]="dSack"
  dTeamYearStats = merge(dTeamYearStats, tmp[,c(2,7,10,14,20)], by="Team")
  
  #Defense Rushing Data
  url= paste("http://www.nfl.com/stats/categorystats?archive=true&conference=
             null&role=OPP&offensiveStatisticCategory=null&defensiveStatistic
             Category=RUSHING&season=",Yr,"&seasonType=REG&tabSeq=2&qualified=false
             &Submit=Go",sep="")
  sitePage=htmlParse(url)
  tmp = readHTMLParse(sitePage, trim=T, header=F, which=1)
  HeadTable=readHTMLTable(sitePage, trim=T, header=T, which=1)
  names(tmp)=names(HeadTable)
  names(tmp)[6]= "dRushAtt"
  names(tmp)[8]= "dRushYds"
  dTeamYearStats= merge(dTeamYearStats, tmp[,c(2,6,8)], by="Team")
  
  
  
  
  