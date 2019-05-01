##This sets the working directory in order to make sure the r file and excel file are in the same place
setwd("C:/Users/C16Jacob.Lindell/Desktop/school/OR 495/OR 495/HW2")

##This reads in the Data from the provided excel folder. I could not figure out how to get the read.csv
##function to navigate to a specific sheet, so that is why I created my own datasheet that only has one sheet
Stats<- read.csv("HW2-1 Data(1).csv")

##Solver is another dataset I made which helped me determine how I was waiting each
##stat. I used Solver to minimize the variance and make sure the average PER was still
##within 14.99 and 15.01. These restrictions on the average can be changed, but the more they are changed
## and the closer they get to fifteen, the higher the variance. I then made it equal to 15 and the variance was still relatively
##low so I made the average=15, like with a normal PER.
Solver<- read.csv("SolverAttempt2.csv")

##This finds the R^2 value for the regression of the 12 variables I am using to predict the PER. It gives
## a 0.68 R^2 value, which is fairly good considering all of the work and variables that go into PER

Regression<- lm(PER ~ FGM + ST + X3M + FTM + BK + OR + AS + (TR-OR) + PF + (FTA-FTM) + TO, data=Stats)
summary(Regression)

##These are the weights used based on the Solver equation explained above. I read them from the sheet 
##because then you can change some of the values and restrictions on the spreadsheet and that will change it here
FGMWeight<- Solver[1,28]
StlWeight<- Solver[1,29]
ThrPTMWeight<- Solver[1,30]
FTMWeight<- Solver[1,31]
BlkWeight<- Solver[1,32]
OffRebWeight<- Solver[1,33]
AstWeight<- Solver[1,34]
DefRebWeight<- Solver[1,35]
FoulWeight<- Solver[1,36]
FTMissWeight<- Solver[1,37]
FGMissWeight<- Solver[1,38]
TOWeight<- Solver[1,39]

##The AdjPER is going to be a dataset that stores the AdjPER
AdjPER<-c()

##This for loop goes through each of the necessary stats in order to caclulate the AdjPER, then calculates it

for (i in 1:355) {
  FGM<- Stats[i,5]
  FGMiss<- (Stats[i,6]-Stats[i,5])
  ThrPTM<-Stats[i,7]
  FTM<-Stats[i,9]
  Blk<-Stats[i,16]
  OffReb<-Stats[i,11]
  Ast<-Stats[i,13]
  DefReb<- (Stats[i,12]- Stats[i,11])
  Foul<-Stats[i,17]
  FTMiss<- (Stats[i,10]-Stats[i,9])
  Stl<-Stats[i,14]
  TO<-Stats[i,15]
  Mins<-Stats[i,4]
  
#   =54*(($AC$2*E2)+($AD$2*N2)+($AE$2*G2)+($AF$2*I2)+($AG$2*P2)+($AH$2*K2)+($AI$2*M2)+
#          ($AJ$2*(L2-K2))-($AK$2*Q2)-($AL$2*(J2-I2))-($AM$2*(F2-E2))-($AN$2*O2))*(1/D2)
  
##This is the AdjPER equation  
  AdjPER1<- 54*((FGM*FGMWeight)+(Stl*StlWeight)+(ThrPTM*ThrPTMWeight)+(FTM*FTMWeight)+
                 (Blk*BlkWeight)+(OffReb*OffRebWeight)+(Ast*AstWeight)+(DefReb*DefRebWeight)-
                 (Foul*FoulWeight)-(FTMiss*FTMissWeight)-(FGMiss*FGMissWeight)-(TO*TOWeight))*(1/Mins)
  
##This stores all of the values calculated into AdjPER  
  AdjPER<-rbind(AdjPER,AdjPER1)
  
  
} 

#NewStats=cbind(Stats,AdjPER[,1])

Delta<-c()

##This for loop goes through and calculates the difference between PER and adjusted PER, which I will
##use later to find the overall variance and average
for (i in 1:355){
  x<- Stats[i,26]
  y<-AdjPER[i,1]

  Change<- abs((x-y))
  
  Delta<-rbind(Delta,Change)
  
}
  
AdjDelta<-cbind(AdjPER,Delta)

##This gets ride of the rownames on the dataset, which allows it to be cbinded
rownames(AdjDelta)<- NULL



NewStats<-cbind(Stats,AdjDelta)

colnames(NewStats)[27]<- "AdjPER"
colnames(NewStats)[28]<- "Delta"

##This solves for the variance
Variance<-var(Delta)


write.csv(NewStats,file = "HW2.csv")
