library(XML)
library(utils)
library(fastR)

setwd("C:/Users/C16Jacob.Lindell/Desktop/school/OR 495/OR 495/HW3")


TeamSchedules<- read.csv("HW3-TeamSchedules.csv")

TeamStats<-read.csv("HW3-TeamStats.csv")

FourFactorStats<-read.csv("Ex4-2 FourFactorStats.csv")


TourneyHist<-read.csv("HW3-NCAATournamentGameHistory.csv",header=T)

##Reads in RPI
RPI<-read.csv("RPI.csv",header=T)

##read in KenPom
KenPom=paste("http://kenpom.com/")
KP_sitePage=htmlParse(KenPom)
KP_Stats=readHTMLTable(KP_sitePage,header=F)$result


##Read in SoS
SoS_url= paste("http://www.cbssports.com/collegebasketball/bracketology/sos",sep="")

SoS_sitePage = htmlParse(SoS_url)

SoS_Stats = readHTMLTable(SoS_sitePage, header=F)$result

Off_Game_Temp = readHTMLTable(Off_Game_sitePage, header=T, trim=T)$result





TeamNames<-TeamSchedules[,3]
TeamNames<-unique(TeamNames)


    

