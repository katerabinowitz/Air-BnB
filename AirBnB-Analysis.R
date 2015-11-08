library(reshape)
setwd("/Users/katerabinowitz/Documents/DataLensDC/Air-BnB/city-listings")

### Air BnB Per Capita in Cities w Available Data ###
### Air BnB Per Capita in Cities w Available Data ###
### Air BnB Per Capita in Cities w Available Data ###
#read-in
files = list.files(pattern="*.csv")
for (i in 1:length(files)) assign(files[i], 
    read.csv(files[i], stringsAsFactors=FALSE, strip.white=TRUE)
    [c('id','number_of_reviews','last_review')])

#define city
Austinbnb.csv$city<-rep("Austin",length(Austinbnb.csv$id))
Bostonbnb.csv$city<-rep("Boston",length(Bostonbnb.csv$id))
Chicagobnb.csv$city<-rep("Chicago",length(Chicagobnb.csv$id))
DCbnb.csv$city<-rep("Washington",length(DCbnb.csv$id))
LAbnb.csv$city<-rep("Los Angeles",length(LAbnb.csv$id))
Nashvillebnb.csv$city<-rep("Nashville",length(Nashvillebnb.csv$id))
NewOrleansbnb.csv$city<-rep("New Orleans",length(NewOrleansbnb.csv$id))
NYCbnb.csv$city<-rep("New York",length(NYCbnb.csv$id))
Portlandbnb.csv$city<-rep("Portland",length(Portlandbnb.csv$id))
SanDiegobnb.csv$city<-rep("San Diego",length(SanDiegobnb.csv$id))
SanFranbnb.csv$city<-rep("San Francisco",length(SanFranbnb.csv$id))
Seattlebnb.csv$city<-rep("Seattle",length(Seattlebnb.csv$id))

#combine, restrict to active
Cities<-rbind(Austinbnb.csv,Bostonbnb.csv,Chicagobnb.csv,DCbnb.csv,LAbnb.csv,
              Nashvillebnb.csv,NewOrleansbnb.csv,NYCbnb.csv,Portlandbnb.csv,
              SanDiegobnb.csv,SanFranbnb.csv,Seattlebnb.csv)
Cities$lastYr<-as.numeric(substr(Cities$last_review, 1,4))
CitiesBnB<-subset(Cities,Cities$number_of_reviews>0 & Cities$lastYr>2013)
CitiesSum<-ddply(CitiesBnB, c("city"), nrow)

#get city pop
PopRaw<-read.csv('http://www.census.gov/popest/data/cities/totals/2014/files/SUB-EST2014_ALL.csv',
                 strip.white=TRUE)[c(1,9,10,17)]
cityPop<-subset(PopRaw,SUMLEV==162)
cityPop<-cityPop[order(-cityPop$POPESTIMATE2014),]
cityPop<-head(cityPop,50)
cityPop$Ccity<-gsub(" city","",cityPop$NAME,ignore.case=T)
cityPop$city<-ifelse(cityPop$Ccity=="Nashville-Davidson metropolitan government (balance)","Nashville",cityPop$Ccity)

CitiesBnB<-merge(CitiesSum,cityPop,by="city")[c(1,2,6)]
CitiesBnB$Airpc<-(CitiesBnB$V1/CitiesBnB$POPESTIMATE2014)*1000
CitiesBnB<-CitiesBnB[order(-CitiesBnB$Airpc),]

write.csv(CitiesBnB,"/Users/katerabinowitz/Documents/DataLensDC/Air-BnB/BnBPerCapita.csv")

rm(list=setdiff(ls(), c("CitiesBnB")))

### Map AirBnB per capita for DC hoods ###
### Map AirBnB per capita for DC hoods ###
### Map AirBnB per capita for DC hoods ###
DClistings<-read.csv('DCbnb.csv',stringsAsFactors=FALSE,strip.white=TRUE) 
DClistings$lastYr<-as.numeric(substr(DClistings$last_review, 1,4))
DCbnb<-subset(DClistings,DClistings$number_of_reviews>0 & DClistings$lastYr>2013)
#Geocode to hood
clusterMap = readOGR("http://opendata.dc.gov/datasets/f6c703ebe2534fc3800609a07bad8f5b_17.geojson", "OGRGeoJSON")
location<-as.list(DCbnb[c(8,7)])
latlong<-SpatialPoints(location, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
ClusterID <- over(latlong, clusterMap )
DC<-cbind(DCbnb,ClusterID)
#Sum to hood
DCcount<-ddply(DC,c("NAME","room_type"),nrow)
DCcount<-cast(DCcount,NAME~room_type)
DCcount<-subset(DCcount,!is.na(DCcount$NAME))
DCcount[is.na(DCcount)] <- 0
colnames(DCcount)<-c("NAME","Unit","Private_Room","Shared_Room")
DCcount$Total<-DCcount$Unit+DCcount$Private_Room+DCcount$Shared_Room
#add per capita
setwd("/Users/katerabinowitz/Documents/DataLensDC/Air-BnB/")
hoodPop<-read.csv('popHood.csv',stringsAsFactors=FALSE,strip.white=TRUE) 
hoodPop$NAME<-paste("Cluster",hoodPop$X.built.in.function.id.)
hoodPop$pop10<-as.numeric(gsub(",","", hoodPop$pop10))

DCSum<-merge(DCcount,hoodPop,by="NAME")[c(1:5,8)]
DCSum$Per<-(DCSum$Total/DCSum$pop10)*1000
DCSum<-DCSum[order(DCSum$Per),]

bnbmap <- merge(clusterMap,DCSum, by="NAME",all.x=TRUE)

writeOGR(bnbmap, 'bnbmap.geojson','bnbmapmap', driver='GeoJSON',check_exists = FALSE)

