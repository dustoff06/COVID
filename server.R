###########################Libraries###########################
###############################################################
require(shiny)       #
require(tidyverse)   #
require(urbnmapr)    #
require(usmap)       #
require(ggplot2)     #
require(ggExtra)     #
require(gridExtra)   #
require(maptools)    #
require(rgdal)       #
require(tmaptools)   #
require(spdep)       #
require(spatialreg)  #
require(sf)          #
require(tmap)        #
require(sp)          #
require(spdep)       #
require(spData)      #
require(raster)      #
require(rgdal)       #
require(ggthemes)    #
require(tigris)      #
require(grid)        #
require(leaflet)     #
require(maptools)    #
require(RColorBrewer)#
require(rgeos)       #
require(elsa)        #
###############################################################
#########################Preset Data###########################
data("us_states") #US states data set                         #
counties2=readOGR("cb_2018_us_county_500k.shp") #shape file   #
shp<- st_read("cb_2018_us_county_500k.shp") #shape file       #
###############################################################
########################Read Online Files######################
myf=function(csv,column){                                     #
  temp=read.csv(csv, header=TRUE)                             #
  temp$GEOID = temp$ï..countyFIPS                             #
  temp$GEOID=as.character(temp$GEOID)                         #
  temp$ï..countyFIPS=NULL                                     #
  temp$County.Name=NULL                                       #
  temp$State=NULL                                             #
  temp$stateFIPS=NULL                                         #
  for (i in 1:nrow(temp))                                     #
  {if (nchar(temp$GEOID[i])==4){                              #
    temp$GEOID[i]=paste0("0", temp$GEOID[i])}}                #
  temp[,column]=temp[,c(ncol(temp)-1)]                        #
  temp=temp[,c(ncol(temp)-1, ncol(temp))]                     #
  return(temp)}                                               #
##############################################################################################################################
##############################################################################################################################
deaths=myf("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv","DeathSums")
cases=myf("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv", "CaseSums")
population=myf("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv", "population")
##############################################################################################################################
#########################Local Files###########################
mygeo=function(csv){
  df=read.csv(csv)
  df$GEOID=as.character(df$GEOID)
  for (i in 1:nrow(df))
  {if (nchar(df$GEOID[i])==4){df$GEOID[i]=paste0("0", df$GEOID[i])}}
  return(df)}
hospitals=mygeo("hospitaldata.csv")
partypolitics=mygeo("partypolitics.csv")
###############################################################
##########################Merge################################
total=merge(cases,deaths,by="GEOID", type="left" )
total=merge(population,total, by="GEOID", type="left")
total=merge(total, hospitals, by="GEOID", type="left")
total=merge(total,partypolitics, by="GEOID", type="left")
total$DeathRate=total$DeathSums/(total$population)
total$CaseRate=total$CaseSums/(total$population)
total[is.na(total)]=0
counties2@data=left_join(counties2@data, total, by="GEOID")
counties2@data$PopDensity=
  as.numeric(counties2@data$ALAND)/counties2@data$population
counties2@data[is.na(counties2@data)]=0
shp=left_join(shp,total,by="GEOID")
shp$PopDensity=as.numeric(shp$ALAND)/shp$population
###############################################################
###########################Map#################################
 
myplot=function(){
  states <- aggregate(counties2[, "STATEFP"], 
            by = list(ID = counties2@data$STATEFP),
            FUN = unique, dissolve = T) 
  qpal<-colorNumeric("Reds", counties2@data$DeathRate) 
  qpal2<-colorNumeric("Blues", counties2@data$CaseRate) 
  leaflet(counties2) %>%
    
  #Overlays
  addPolygons(stroke = FALSE, 
      fillOpacity = 1, 
      smoothFactor = 0.2, 
      color=~qpal(DeathRate),
      group="Deaths per Capita",
      popup = paste("County: ", counties2@data$NAME, "<br>",
              "Deaths / Capita: ", 
              round(counties2@data$DeathRate,4), "<br>")) %>%
  addPolygons(stroke = FALSE, 
      fillOpacity = 1, 
      smoothFactor = 0.2, 
      color=~qpal2(CaseRate),
      group="Cases per Capita",
      popup = paste("County: ", counties2@data$NAME, "<br>",
              "Cases / Capita: ", 
              round(counties2@data$CaseRate,4), "<br>")) %>%
  
  #Base Diagrams
  addPolylines(data = states, 
               color = "black", 
               opacity = 1, 
               weight = 2, 
               group="State Boundaries")%>%
  addPolylines(data = counties2, 
               color = "black", 
               opacity = .5, 
               weight = .5, 
               group="County Boundaries")%>%
  
  fitBounds(-124.8, -66.9, 24.4,49.4) %>% 
  setView(-98.6, 39.83, zoom = 4)%>%
  addLegend("bottomright", opacity=1, pal = qpal, 
            values = ~DeathRate,
            title = "Deaths per Capita"
            )%>%
  addLegend("bottomleft", opacity=1, pal = qpal2, 
            values = ~CaseRate,
            title = "Cases per Capita"
            )%>%
  addLayersControl(
    baseGroups=c("State Boundaries", "County Boundaries"),
    overlayGroups = c("Deaths per Capita", 
          "Cases per Capita"),
    options = layersControlOptions(collapsed = FALSE)
    )
}
###############################################################
###########################LM1#################################
fit_1 <- lm(scale(DeathRate)~scale(MDs)+scale(AcuteBeds)+
              scale(ICUs)+scale(StaffedBeds)+
              scale(Discharges)+scale(Census)+scale(ALOS)+scale(CMI)+
              scale(PopDensity)+scale(DemRep)+as.factor(WParty), 
            data=counties2@data)
mysum=summary(fit_1)
shp$res_fit1 <- residuals(fit_1)
shp$sd_breaks1 <- scale(shp$res_fit1)[,1] 
shp$fitted_fit1 <- fitted(fit_1)
#return(mysum)
###############################################################
###########################LM2#################################
fit_2 <- lm(scale(CaseRate)~scale(MDs)+scale(AcuteBeds)+
              scale(ICUs)+scale(StaffedBeds)+
              scale(Discharges)+scale(Census)+scale(ALOS)+scale(CMI)+
              scale(PopDensity)+scale(DemRep)+as.factor(WParty), 
            data=counties2@data)
mysum2=summary(fit_2)
shp$res_fit2 <- residuals(fit_2)
shp$sd_breaks2 <- scale(shp$res_fit2)[,1] 
shp$fitted_fit2 <- fitted(fit_2)
#return(mysum2)
###############################################################
####################Spatial Regression#########################
myspatial=function(shpfile, variable){
  list.queen<-poly2nb(shpfile, queen=TRUE)
  W<-nb2listw(list.queen, style="W", zero.policy=TRUE)
  wm <- nb2mat(list.queen, style='B', zero.policy=TRUE) #weight matrix
  rwm <- mat2listw(wm, style='W') #convert to list
  p1=lm.morantest(fit_1, rwm, alternative="two.sided", zero.policy=TRUE)
  p2=lm.LMtests(fit_1, rwm, test = 
                  c("LMerr","LMlag","RLMerr","RLMlag","SARMA"), 
                zero.policy=TRUE)
  p3=lm.morantest(fit_2, rwm, alternative="two.sided", zero.policy=TRUE)
  p4=lm.LMtests(fit_2, rwm, test = 
                  c("LMerr","LMlag","RLMerr","RLMlag","SARMA"), 
                zero.policy=TRUE)
  all=list(c(p1,p2,p3,p4))
    if (variable=="DeathRate"){
          mylag=stsls(scale(DeathRate)~scale(MDs)+scale(AcuteBeds)+
            scale(ICUs)+scale(StaffedBeds)+
            scale(Discharges)+scale(Census)+scale(ALOS)+scale(CMI)+
            scale(PopDensity)+scale(DemRep)+as.factor(WParty), 
            zero.policy=TRUE, data=shpfile@data, W)} 
    else {mylag=stsls(scale(CaseRate)~scale(MDs)+scale(AcuteBeds)+
            scale(ICUs)+scale(StaffedBeds)+
            scale(Discharges)+scale(Census)+scale(ALOS)+scale(CMI)+
            scale(PopDensity)+scale(DemRep)+as.factor(WParty), 
            zero.policy=TRUE, data=shpfile@data, W)} 
  sum1=summary(mylag)
  shpfile@data$res<-resid(mylag) #residual sar
  moran=moran.mc(shpfile@data$res, W, 50, zero.policy=TRUE) #LISA   
  mylisa=lisa(shpfile, d1=0, d2=2000, statistic="I",zcol=variable)  
return(sum1)
}

plot3=function(shapefile){
  my_breaks <- c(-4,-3,-2,-1,1,2,3,4,13)
  plota=
    tm_shape(shp, bbox=tmaptools::bb(xlim=c(-14, -5), 
    ylim=c(2, 9),width=5, ext=5, relative = TRUE)) + 
    tm_fill("sd_breaks1", title = "Residuals", style = "fixed", 
          breaks = my_breaks, palette = "-RdBu") + 
    tm_borders(alpha = 0.1) +
    tm_layout(main.title = "Residuals", main.title.size = 0.7 ,
          legend.position = c("right", "bottom"), legend.title.size = 0.8)
  return(plota)
}

plot4=function(shapefile){
  my_breaks <- c(-3,-2,-1,1,2,3,22)
  plotb=
    tm_shape(shp, bbox=tmaptools::bb(xlim=c(-14, -5), 
    ylim=c(2, 9),width=5, ext=5, relative = TRUE)) + 
    tm_fill("sd_breaks2", title = "Residuals", style = "fixed", 
          breaks = my_breaks, palette = "-RdBu") +
    tm_borders(alpha = 0.1) +
    tm_layout(main.title = "Residuals, ", main.title.size = 0.7 ,
          legend.position = c("right", "bottom"), legend.title.size = 0.8)
  return(plotb)
}


###############################################################
####################Shiny App Stuff############################
# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    myplot()
  })
  output$summary<-renderPrint({
    myspatial(counties2,"DeathRate")
  })
  output$summary2<-renderPrint({
    myspatial(counties2,"CaseRate")
  })
  output$plot3<-renderPlot({
  plot3(shp)
  })
  output$plot4<-renderPlot({
  plot4(shp)
  })
  
  
})


