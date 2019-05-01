library(XML)

# this is just for a single year (for now...)
year = 2015

# get list of team hyperlinks
url = "http://www.sports-reference.com/cbb/schools/"
sitePage = htmlParse(url)
hlinks = xpathSApply(sitePage,"//table//tr//td//a//@href[contains(.,'cbb/school')]")


#
# Part 1 - gather stats for first team; store to data frames
#

# Part 1A
# save team name 
url = paste("http://www.sports-reference.com",hlinks[1],sep="")
sitePage = htmlParse(url)
tmName = getNodeSet(sitePage,"//a[text()[contains(.,'[+]')] and @href[contains(.,'/cbb')]]")[[1]]
tmName = xmlValue(tmName)
# remove special characters and trailing white space
tmName = gsub("Â","",tmName)
tmName = gsub("[[]","",tmName)
tmName = gsub("[]]","",tmName)
tmName = gsub("[+]","",tmName)
tmName = gsub("^\\s+|\\s+$", "", tmName)


# Part 1B
# save team and opponent stats
url = paste("http://www.sports-reference.com",hlinks[1],year,".html",sep="")
alltmStats = readHTMLTable(url, id='team_stats')[[2]][c(1,3),-1]
alltmStats = data.frame(Year=rep(year,2),Team=rep(tmName,2),Which=c("Tm","Opp"),alltmStats)

#
# Part 2 - gather stats for remaining teams; augment data frames
#
missingtmStats = c("")
for(i in 2:length(hlinks)){
#for(i in 2:25){
  # Part 2A
  # save team name 
  tmStats=data.frame()
  tmSchedule=data.frame()
  url = paste("http://www.sports-reference.com",hlinks[i],sep="")
  sitePage = htmlParse(url)
  tmName = getNodeSet(sitePage,"//a[text()[contains(.,'[+]')] and @href[contains(.,'/cbb')]]")[[1]]
  tmName = xmlValue(tmName)
  # remove special characters and trailing white space
  tmName = gsub("Â","",tmName)
  tmName = gsub("[[]","",tmName)
  tmName = gsub("[]]","",tmName)
  tmName = gsub("[+]","",tmName)
  tmName = gsub("^\\s+|\\s+$", "", tmName)
  
  
  # Part 1B
  # save team and opponent stats
  url = paste("http://www.sports-reference.com",hlinks[i],year,".html",sep="")
  tmStats = try(readHTMLTable(url, id='team_stats')[[2]][c(1,3),-1], silent=TRUE)
  if(class(tmStats)=="try-error"){
    tmStats=data.frame()
  } else {
    if(length(tmStats[,1])==2 & length(tmStats[1,])==24){ #check to see if size is consistent
      tmStats = try(data.frame(Year=rep(year,2),Team=rep(tmName,2),Which=c("Tm","Opp"),tmStats), silent=TRUE)
      alltmStats = try(rbind(alltmStats,tmStats), silent=TRUE)
    } else { 
      print(paste("Size Mismatch in Team Stats for ",tmName,sep=""))
      missingtmStats = c(missingtmStats,hlinks[i])
    }
  }
}

# write to file
write.csv(alltmStats,"Ex4-2 FourFactorStats.csv")
