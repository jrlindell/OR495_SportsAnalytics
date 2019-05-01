library(rgl)

# read in CSV file player data
indata=read.csv("Ex3-4a Individual Rating Data.csv")


# store player names as character labels & initialize color vector
Label = as.character(indata$Player)
colors = as.character(indata$POS)


# loop over each player
for (i in 1:length(indata[,1])){
  # delete names that we don't want on the graph
  if (indata$OE[i]>1 | indata$PER[i]>27 | indata$PM[i]>12) { }
  else { Label[i]=" "}
  
  # assign colors based on position
  # see http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf for color options
  if (indata$POS[i]=="PG") {colors[i]="yellow"}
  if (indata$POS[i]=="SG") {colors[i]="red"}
  if (indata$POS[i]=="SF") {colors[i]="green"}
  if (indata$POS[i]=="PF") {colors[i]="blue"}
  if (indata$POS[i]=="C") {colors[i]="black"}
}


# make legend
bgplot3d(plot(indata$PER, indata$OE))
next3d(reuse = FALSE)
legend3d("bottomleft", c("PG", "SG","SF","PF","C"), pch=rep(19,5), col=c("yellow","red","green","blue","black"))

# plot PER vs +/- vs OR and labels
plot3d(indata[,2:4], size=7, col=colors, xlab="Offensive Efficiency (OE)",ylab="Player Efficiency Rating (PER)",zlab="Plus Minus (PM)", main="NBA Individual Player Rating 2013-14 Season")
text3d(indata[,2:4],texts=Label,font=1)


# orient the graph how you want it and save to file
snapshot3d(filename="Ex3-4a NBAPlayerStats3D.png",fmt="png",top=TRUE)


# OE seems to be bias toward big men...
# let's look at a boxplot
boxplot(OE ~ POS, data=indata, notch=FALSE, horizontal=TRUE,
        col=(c("grey","blue","yellow","green","red")), main="NBA Individual Player Rating 2013-14 Season", 
        ylab = "Position", xlab = "Offensive Efficiency (OE)")




