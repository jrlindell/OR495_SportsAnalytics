### Ex 2-3 Import NFL.com table
### Purpose: Illistrate web data import using readHTMLTable

library(XML)
library(utils)


### 1. declare master data structure
allData = data.frame


### 2. set URL address
url = "http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=GAME_STATS&defensiveStatisticCategory=null&season=2003&seasonType=REG&tabSeq=2&qualified=false&Submit=Go"
  

### 3. grab entire webpage and store as ???
sitePage = htmlParse(url)
class(sitePage) # let's take a look at what kind of variable this is!


### 4. extract table you want from webpage
Stats = readHTMLTable(sitePage)
class(Stats)
names(Stats)
Stats = readHTMLTable(sitePage)$result #choose results because it is the only thing give from names





### 5. IMPORTANT! -- look at the result and ensure the table was
### imported correctly

Stats = readHTMLTable(sitePage,header=F)$result #because it did not read in the first entry, then didn't read in headers
temp = readHTMLTable(sitePage,header=T)$result
names(Stats)=names(temp)


### 6. What data (columns) do you need from this table?
### Store only the data you need to allData
names(Stats)
allData = data.frame("Team"=Stats$"Team", "OffPts"=Stats$"\nTotPts")

gsub("\n"," ", names(temp)) #removes \n and replaces with space

names(Stats)=gsub("\n"," ", names(temp))

allData = data.frame("Team"=Stats$Team, "OffPts"=Stats$TotPts)



### 7. Generalize these steps to gather multiple tables over multiple years

Stats[1:2,] #only uses first two rows

scoringmargin= Stats[,c()]


### 8. save result to csv file for later use
#write.csv(allData,"C:\Users\C16Jacob.Lindell\Desktop\school\OR 495\OR 495\HW1.xlsx")
#write.csv2(allData,"C:\Users\C16Jacob.Lindell\Desktop\school\OR 495\OR 495\HW1 - Copy.csv")
write.csv2(Stats,file="HW1 - Copy.csv")
# library(xlsx)
#write.xlsx(allData,"C:\Users\C16Jacob.Lindell\Desktop\school\OR 495\OR 495\HW1.xlsx")
