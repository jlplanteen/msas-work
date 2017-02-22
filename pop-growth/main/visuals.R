#####Creation OF Choropleth Graphs for Data Visualization
##############################################################################

## Choropleth creation of raw data for population growth, predicted values of popoulation growth for best model,
## and residual values for best model.

## Jacey Planteen; R-Day 11/20/15

require(ggplot2); require(maps); require(mapproj); require(rgdal); require(RColorBrewer)
load("FinalModel.Rdata")


#Define the groupings for population change
raw.cuts <- seq(-0.3, 0.375, by=0.075)
raw.cuts <- c(min(c(model.data$Change, model.data$predix), na.rm=TRUE)-.0001, 
		raw.cuts, max(c(model.data$Change, model.data$predix), na.rm=TRUE)
		+0.0001)
model.data$raw.cuts <- cut(model.data$Change, raw.cuts)
model.data$pred.cuts <- cut(model.data$predix, raw.cuts)

resid.cuts <- seq(-0.3, 0.3, by=0.075)
resid.cuts <- c(min(model.data$resids, na.rm=TRUE)-.06, resid.cuts,
		max(model.data$resids, na.rm=TRUE)+0.06)
model.data$r.cuts <- cut(model.data$resids, resid.cuts)

#Define color buckets
buckets <- brewer.pal(11, "RdYlBu")
buckets <- buckets[length(buckets):1]
buckets2 <- brewer.pal(10, "PRGn")

##Read in GIS/geographic data
counties <- readOGR(dsn=".",layer="gz_2010_us_050_00_5m")
counties$FIPS <- paste(counties$STATE, counties$COUNTY)
FIPS.convert <- counties@data
FIPS.convert$id <- rownames(FIPS.convert)
temp <- merge(model.data, FIPS.convert, by="FIPS", all=TRUE)
geo.temp <- fortify(counties)
geo.data <- merge(geo.temp, temp, by="id", all=TRUE)

#Split into three sets of data > continental US, Alaska, & Hawaii
continental <- geo.data[!geo.data$STATE%in%c("02", "15", "72"),]
alaska <- geo.data[geo.data$STATE=="02",]
alaska$long[alaska$long>0&!is.na(alaska$long)] <- -alaska$long[
	alaska$long>0&!is.na(alaska$long)] 
hawaii <- geo.data[geo.data$STATE=="15",]

#Create maps of raw (actual) population change data
state.map <- map_data("state")

#Legend labels
leg.labs <- c("<-30%", "(-30%, -22.5%]", "(-22.5%, -15%]", "(-15%, -7.5%]", 
		"(-7.5%, 0%]", "(0%, 7.5%]", "(7.5%, 15%]", "(15%, 22.5%]", 
		"(22.5%, 30%]", "(30%, 37.5%]", ">37.5%")

#Residual labels
res.labs <- c("<-30%", "(-30%, -22.5%]", "(-22.5%, -15%]", "(-15%, -7.5%]", 
		"(-7.5%, 0%]", "(0%, 7.5%]", "(7.5%, 15%]", "(15%, 22.5%]", 
		"(22.5%, 30%]", ">30%")

AK.raw.i <- as.numeric(table(alaska$raw.cuts))>0
cont.raw.i <- as.numeric(table(continental$raw.cuts))>0
HI.raw.i <- as.numeric(table(hawaii$raw.cuts))>0

##Make choropleths of raw data
alaska.raw <- ggplot(alaska, aes(long, lat, group=group))+ coord_map()+
		geom_polygon(aes(fill=raw.cuts))+ scale_fill_manual(values=buckets[AK.raw.i]) +
		theme(panel.background=element_rect(fill="transparent", color=NA), 
		axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
		axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none")

cont.raw <- ggplot(continental, aes(long, lat, group=group))+ coord_map() + 
		geom_polygon(aes(fill=raw.cuts))+ scale_fill_manual(name="Population Change", 
		values=buckets, labels=leg.labs) +
	 	geom_path(data = state.map, colour = "black", size = .5) + 
		ggtitle("Figure 1. Actual Population Change in the United States from 2000 to 2010") +
		theme(panel.background=element_rect(fill="transparent", color=NA), 
		axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
		axis.title.x=element_blank(), axis.title.y=element_blank())

hawaii.raw <- ggplot(hawaii, aes(long, lat, group=group))+ coord_map()+
		geom_polygon(aes(fill=raw.cuts))+ scale_fill_manual(values=buckets[HI.raw.i])  +
		theme(panel.background=element_rect(fill="transparent", color=NA), 
		axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
		axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none")


##Make predicted value choropleths
alaska.pred <- ggplot(alaska, aes(long, lat, group=group))+ coord_map()+
		geom_polygon(aes(fill=pred.cuts))+ scale_fill_manual(values=buckets)
cont.pred <- ggplot(continental, aes(long, lat, group=group))+ coord_map() + 
		geom_polygon(aes(fill=pred.cuts))+ scale_fill_manual(values=buckets[3:11]) +
	 	geom_path(data = state.map, colour = "black", size = .5)+ 
		ggtitle("Estimated Population Change in the United States from 2000 to 2010") +
		theme(panel.background=element_rect(fill="transparent", color=NA), 
		axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
		axis.title.x=element_blank(), axis.title.y=element_blank())
hawaii.pred <- ggplot(hawaii, aes(long, lat, group=group))+ coord_map()+
		geom_polygon(aes(fill=pred.cuts))+ scale_fill_manual(values=buckets)


##Make residual choropleths
AK.res.i <- as.numeric(table(alaska$r.cuts))>0
cont.res.i <- as.numeric(table(continental$r.cuts))>0
HI.res.i <- as.numeric(table(hawaii$r.cuts))>0

cont.resid <- ggplot(continental, aes(long, lat, group=group))+ coord_map() + 
		geom_polygon(aes(fill=r.cuts))+ scale_fill_manual(name="Residual", 
		values=buckets2[cont.res.i], labels=res.labs) +
	 	geom_path(data = state.map, colour = "black", size = .5) + 
		ggtitle("Residual Error for Model Estimations") +
		theme(panel.background=element_rect(fill="transparent", color=NA), 
		axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
		axis.title.x=element_blank(), axis.title.y=element_blank())

alaska.resid <- ggplot(alaska, aes(long, lat, group=group))+ coord_map()+
		geom_polygon(aes(fill=r.cuts))+ scale_fill_manual(values=buckets2[AK.res.i]) +
		theme(panel.background=element_rect(fill="transparent", color=NA), 
		axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
		axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none")

hawaii.resid <- ggplot(hawaii, aes(long, lat, group=group))+ coord_map()+
		geom_polygon(aes(fill=r.cuts))+ scale_fill_manual(values=buckets2[HI.res.i])  +
		theme(panel.background=element_rect(fill="transparent", color=NA), 
		axis.ticks=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
		axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none")
