### Ex 2-4 Import NFL.com table
### Purpose: Illistrate web data import using xpathSApply

library(XML)
library(utils)

### 1. set URL address
url = "http://www.nfl.com/stats/categorystats?archive=true&conference=null&role=TM&offensiveStatisticCategory=GAME_STATS&defensiveStatisticCategory=null&season=2003&seasonType=REG&tabSeq=2&qualified=false&Submit=Go"

### 2. grab entire webpage and store as HTMLInternalDocument
sitePage = htmlParse(url)

class(sitePage)

### 3. Select from table named ‘result’ all rows and only the second column
Teams = xpathSApply(sitePage,"//table[@id='result']//tr//td[2]",xmlValue)
Teams # let's take a look at the result...  YUCK!

### 4. CLEAN the data by removing special characters \t and \n
Teams = gsub("\\t","",Teams)
Teams = gsub("\\n","",Teams)

### 5. How would you generalize this to get all columns in multiple tables over many years?
ThdO = Teams= xpathSApply(sitePage,"//table[@id='result']//tr//td[12]",xmlValue)
ThdO = gsub("\\t","",Teams)
ThdO = gsub("\\n","",Teams)
ThdO= as.numeric(ThdO)
ThdO
allData= xpathSApply(sitePage,"//table[@id='result']//tr//td",xmlValue)
allData = gsub("\\t","",allData)
allData = gsub("\\n","",allData)
allData
