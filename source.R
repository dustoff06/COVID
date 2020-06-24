##############################################################
#download the dataset from the ECDC website to a local temporary file

mydata=read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

mydata$caserate=mydata$cases/(mydata$popData2018/1000000)     #calculate rates
mydata$deathrate=mydata$deaths/(mydata$popData2018/1000000)   #calculate rates
mydata$mortality=mydata$caserate/mydata$deathrate             #mortality for those diagnosed
mydata$mortality[is.na(mydata$mortality)]=0
mydata$date=as.Date(paste0(mydata$month,"/",mydata$day,"/", mydata$year), format="%m/%d/%Y")

mynames=as.character(unique(mydata$countriesAndTerritories))
mydate=as.character(unique(sort(mydata$date)))
mydata$dateRep=NULL
mydata$day=NULL
mydata$month=NULL
mydata$year=NULL
mydata$geoId=NULL
mydata$countryterritoryCode=NULL
mydata$popData2018=NULL
myagg=aggregate(mydata[,-c(3,7)], by=list(mydata$countriesAndTerritories, mydata$date), FUN=mean)




