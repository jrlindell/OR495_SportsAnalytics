library(XML)

# get box score hyperlinks
url = "http://www.basketball-reference.com/leagues/NBA_2014_games.html"
sitePage<-htmlParse(paste(url, sep=""))
hlinks<-xpathSApply(sitePage,"//table[@id='games']//tr//a/@href[contains(.,'boxscores/2')]")


# get play-by-play data and remove specials characters
url=paste("http://www.basketball-reference.com",gsub("/boxscores/","/boxscores/pbp/",hlinks[1]),sep="")
tmp = try(readHTMLTable(url,header=T,trim=T,which=17), silent =TRUE)
tmp = as.data.frame(lapply(tmp,FUN=function(X) gsub("Â", "", X))) 

# save to titled data frame
boxdata = data.frame("TIME"=as.character(tmp[,1]),"TEAMA"=as.character(tmp[,2]),"SCORE"=as.character(tmp[,4]),"TeamB"=as.character(tmp[,6]))

# get lineup data
url=paste("http://www.basketball-reference.com",hlinks[1],sep="")
tm1 = as.character(try(readHTMLTable(url,header=T,trim=T,which=11), silent =TRUE)[1:5,1])
tm2 = as.character(try(readHTMLTable(url,header=T,trim=T,which=13), silent =TRUE)[1:5,1])

