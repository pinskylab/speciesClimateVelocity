####################
## CEM by Strata ##
####################

#############
## Data prep
#############

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')
source('../allsubset.glm 2012-01-30.R')

datatrim = read.csv('Output/dataCEM_2012-02-09.csv', row.names=1)
	dim(datatrim)     

datastrat = with(datatrim, aggregate(list(wtcpue = WTCPUE, numcpue = NUMCPUE, julian = julian, lat = LATITUDE, long = long, depth = BOT_DEPTH, surftemp = SURF_TEMP, bottemp = BOT_TEMP, mintemp = mintemp, mintempmnth = mintempmnth, maxtemp = maxtemp, maxtempmnth = maxtempmnth, pres = pres), by=list(year = YEAR, stratum = STRATUM, spp = SCIENTIFIC), FUN=mean, na.rm=T))
	dim(datastrat)
datasd = with(datatrim, aggregate(list(wtcpue.sd = WTCPUE, numcpue.sd = NUMCPUE, julian.sd = julian, depth.sd = BOT_DEPTH, surftemp.sd = SURF_TEMP, bottemp.sd = BOT_TEMP, mintemp.sd = mintemp, mintempmnth.sd = mintempmnth, maxtemp.sd = maxtemp, maxtempmnth.sd = maxtempmnth, pres.sd = pres), by=list(year = YEAR, stratum = STRATUM, spp = SCIENTIFIC), FUN=sd, na.rm=T))
	dim(datasd)	
datastrat = merge(datastrat, datasd)
	dim(datastrat)
	
datastrat$present = datastrat$wtcpue>0 # because pres column indicates # presences in each stratum

datastrat$stratumfact = as.factor(datastrat$stratum)
	
write.csv(datastrat, paste('Output/dataCEMstrat_', Sys.Date(), '.csv', sep=''))

##################
## Examine data
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')
datastrat = read.csv('Output/dataCEMstrat_2012-02-09.csv', row.names=1)

# Range
range(datastrat$wtcpue)
range(datastrat$numcpue)

aggregate(list(min = datastrat$wtcpue), by=list(datastrat$spp), FUN=min) # all spp hit zero in some strata in some years (except Gadus macrocephalus, Hippoglossoides elassodon, and Theragra chalcogramma)

# Variance increases with the mean on a log-log plot
plot(datastrat$wtcpue, datastrat$wtcpue.sd, log='xy')
plot(datastrat$numcpue, datastrat$numcpue.sd, log='xy')

# Variance may decrease with an increasing mean
plot(datastrat$surftemp, datastrat$surftemp.sd, log='xy')
plot(datastrat$bottemp, datastrat$bottemp.sd, log='xy')

# Data availability
table(datastrat$year, !is.na(datastrat$bottemp))
table(datastrat$year, !is.na(datastrat$mintemp))

hist(datastrat$surftemp)
hist(datastrat$bottemp)
hist(datastrat$mintemp)
hist(datastrat$maxtemp) # a bit right-skewed
	hist(log(datastrat$maxtemp)) # looks better


######################################
## Calc lat and long by year by spp ##
######################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')
datastrat = read.csv('Output/dataCEMstrat_2012-02-09.csv', row.names=1)
strata = read.csv('AFSCAleutiansStrata.csv')

# Add stratum lat to dataframe
latlongmean = aggregate(list(latstrat = datastrat$lat, longstrat = datastrat$long), by = list(stratum = datastrat$stratum), FUN=mean, na.rm=T)
latlongmin = aggregate(list(latstratmin = datastrat$lat, longstratmin = datastrat$long), by = list(stratum = datastrat$stratum), FUN=min, na.rm=T)
latlongmax = aggregate(list(latstratmax = datastrat$lat, longstratmax = datastrat$long), by = list(stratum = datastrat$stratum), FUN=max, na.rm=T)

datastrat = merge(datastrat, latlongmean)
datastrat = merge(datastrat, latlongmin)
datastrat = merge(datastrat, latlongmax)

# Calculate max and min lat, mean lat (abundance), and mean lat (biomass)
	require(Hmisc)
	
	# also calc min and max as average of min and max 3 records
	# Doesn't calculate along-shelf distance, which may be more appropriate than lat
	yr = sort(unique(datastrat$year))
	spp = sort(unique(datastrat$spp))
	len = length(spp)
	minlat = data.frame(spp = spp)
	maxlat = data.frame(spp = spp)
	centbiolat = data.frame(spp = spp)
	centablat = data.frame(spp = spp)
	minlong = data.frame(spp = spp)
	maxlong = data.frame(spp = spp)
	centbiolong = data.frame(spp = spp)
	centablong = data.frame(spp = spp)
	rangesurvey = data.frame(yr = yr, minlat = numeric(length(yr)), maxlat=numeric(length(yr)), minlong = numeric(length(yr)), maxlong=numeric(length(yr))) # records the extent of the survey each year
	biomass = data.frame(spp = spp)
	abundance = data.frame(spp = spp)
	
	# values in col 1, weights in col 2
	wgtmean = function(x) wtd.mean(x=x[,1], weights=x[,2])

	# values in col 1, strata in col 2
	# Need stratum areas and towed area stats to do this right (see Ricard 2010)
	# Towed area from Access DB? Stratum areas from AFSC?
	stratmean = function(x, stratid, stratarea, na.rm=TRUE){
		xh = aggregate(x[,1], by=list(stratum=x[,2]), FUN=mean, na.rm=na.rm)
		xh = merge(xh, data.frame(stratum = stratid, Ah = stratarea))
		A = sum(xh$Ah)
		return(sum((xh$Ah/A)*xh$x))
	}
	
	# Fill the matrices by year
	for(i in 1:length(yr)){
		inds = datastrat$year == yr[i]
		
		# Max lat: only use lats where spp is present
		temp = aggregate(datastrat$latstratmax[inds & datastrat$wtcpue>0], by = list(spp = datastrat$spp[inds & datastrat$wtcpue>0]), FUN = max)
			names(temp)[2] = yr[i]
		maxlat = merge(maxlat, temp, all.x=TRUE)
	
		# Min lat
		temp = aggregate(datastrat$latstratmin[inds & datastrat$wtcpue>0], by = list(spp = datastrat$spp[inds & datastrat$wtcpue>0]), FUN = min)
			names(temp)[2] = yr[i]
		minlat = merge(minlat, temp, all.x=TRUE)
	
		# Center of biomass lat
		temp = summarize(datastrat[inds, c('latstrat', 'wtcpue')], by = list(spp = datastrat$spp[inds]), FUN = wgtmean) # have to use the Hmisc function since input is a dataframe
			names(temp)[2] = yr[i]
		centbiolat = merge(centbiolat, temp, all.x=TRUE)

		# Center of abundance lat
		temp = summarize(datastrat[inds, c('latstrat', 'numcpue')], by = list(spp = datastrat$spp[inds]), FUN = wgtmean) # have to use the Hmisc function since input is a dataframe
			names(temp)[2] = yr[i]
		centablat = merge(centablat, temp, all.x=TRUE)
		
		# Max long
		temp = aggregate(datastrat$longstratmax[inds & datastrat$wtcpue>0], by = list(spp = datastrat$spp[inds & datastrat$wtcpue>0]), FUN = max)
			names(temp)[2] = yr[i]
		maxlong = merge(maxlong, temp, all.x=TRUE)
	
		# Min long
		temp = aggregate(datastrat$longstratmin[inds & datastrat$wtcpue>0], by = list(spp = datastrat$spp[inds & datastrat$wtcpue>0]), FUN = min)
			names(temp)[2] = yr[i]
		minlong = merge(minlong, temp, all.x=TRUE)
	
		# Center of biomass long
		temp = summarize(datastrat[inds, c('longstrat', 'wtcpue')], by = list(spp = datastrat$spp[inds]), FUN = wgtmean) # have to use the Hmisc function since input is a dataframe
			names(temp)[2] = yr[i]
		centbiolong = merge(centbiolong, temp, all.x=TRUE)

		# Center of abundance long
		temp = summarize(datastrat[inds, c('longstrat', 'numcpue')], by = list(spp = datastrat$spp[inds]), FUN = wgtmean) # have to use the Hmisc function since input is a dataframe
			names(temp)[2] = yr[i]
		centablong = merge(centablong, temp, all.x=TRUE)

		# Full survey extent
		rangesurvey$minlat[i] = min(datastrat$latstratmin[datastrat$year == yr[i]])
		rangesurvey$maxlat[i] = max(datastrat$latstratmax[datastrat$year == yr[i]])
		rangesurvey$minlong[i] = min(datastrat$longstratmin[datastrat$year == yr[i]])
		rangesurvey$maxlong[i] = max(datastrat$longstratmax[datastrat$year == yr[i]])

		# Biomass
		temp = summarize(datastrat[inds, c('wtcpue', 'stratum')], by=list(spp=datastrat$spp[inds]), FUN=stratmean, stratid=strata$StratumCode, stratarea = strata$Areakm2)
			names(temp)[2] = yr[i]
		biomass = merge(biomass, temp, all.x=TRUE)
		
		# Abundance
		temp = summarize(datastrat[inds, c('numcpue', 'stratum')], by=list(spp=datastrat$spp[inds]), FUN=stratmean, stratid=strata$StratumCode, stratarea = strata$Areakm2)
			names(temp)[2] = yr[i]
		abundance = merge(abundance, temp, all.x=TRUE)
	}
	
# Calculate mean temperature as a stratum average
	temp = summarize(datastrat[,c('bottemp', 'stratum')], by=list(spp=datastrat$spp, year = datastrat$year), FUN=stratmean, stratid=strata$StratumCode, stratarea = strata$Areakm2, na.rm=TRUE)
		names(temp)[3] = 'bottemp'
	temp2 = reshape(temp, direction='wide', idvar = 'spp', timevar = 'year', v.names = 'bottemp')
		for(i in 2:ncol(temp2)){
			print(all(temp2[,i] == temp2[1,i])) # should return all true
		}
	btemp = temp2[1,2:ncol(temp2)]

	temp = summarize(datastrat[,c('surftemp', 'stratum')], by=list(spp=datastrat$spp, year = datastrat$year), FUN=stratmean, stratid=strata$StratumCode, stratarea = strata$Areakm2)
		names(temp)[3] = 'surftemp'
	temp2 = reshape(temp, direction='wide', idvar = 'spp', timevar = 'year', v.names = 'surftemp')
		for(i in 2:ncol(temp2)){
			print(all(temp2[,i] == temp2[1,i])) # should return all true
		}
	sstemp = temp2[1,2:ncol(temp2)]

# Mean temp after Mueter & Litzow (also need spatially autocorrelated error)
	require(mgcv)
	inds = !duplicated(datastrat[,c('stratum', 'year')]) # get just one spp

	btempgam = gam(bottemp ~ s(latstrat) + s(longstrat) + s(julian) + as.factor(year), data=datastrat, subset=inds)
		summary(btempgam)
		plot(btempgam, pages=1, se=TRUE, all.terms=TRUE)
	c = coef(btempgam)
	i = grep('year', names(c))
	btemp2 = c(as.numeric(c)[1], as.numeric(c)[i] + as.numeric(c)[1]) # first is the intercept, and it's also 1981

	stempgam = gam(surftemp ~ s(latstrat) + s(longstrat) + s(julian) + as.factor(year), data=datastrat, subset=inds)
		summary(stempgam)
		plot(stempgam, pages=1, se=TRUE, all.terms=TRUE)
	c = coef(stempgam)
	i = grep('year', names(c))
	sstemp2 = c(as.numeric(c)[1], as.numeric(c)[i] + as.numeric(c)[1]) # add in intercept (it's also 1981)
		
save(yr, spp, minlat, maxlat, centbiolat, centablat, minlong, maxlong, centbiolong, centablong, rangesurvey, biomass, abundance, btemp, sstemp, sstemp2, btemp2, file=paste('Output/Range_locations_strat', Sys.Date(), '.RData', sep=''))



################################################
## Plots of species distribution through time ##
################################################

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')
load('Output/Range_locations_strat2012-02-09.RData')

	length(spp)
	
	# Latitude
	quartz(width = 10, height = 8)
	# pdf(paste('Figures/min,max,mean lat by spp and year_strat_', Sys.Date(), '.pdf', sep=''), width=10, height=8)
	ncol = ncol(maxlat)
	ylims = c(min(rangesurvey$minlat), max(rangesurvey$maxlat))
	par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8)
	for(j in 1:ceiling(length(spp)/36)){
		ind = (j-1)*36
		for(i in 1:36){
			plot(yr, maxlat[ind+i,2:ncol], col='blue', type='o', xlab='', ylab='', main=maxlat$spp[ind+i], ylim = ylims, pch=16, cex=0.5)
			points(yr, minlat[ind+i,2:ncol], col='red', pch=16, cex=0.5, type='o')
			points(yr, centbiolat[ind+i,2:ncol], col='black', pch=16, cex=0.5, type='o')
			points(yr, centablat[ind+i,2:ncol], col='grey', pch=16, cex=0.5, type='o')
			points(yr, rangesurvey$minlat, col='grey', lty=2, type='l')
			points(yr, rangesurvey$maxlat, col='grey', lty=2, type='l')
		}
		#readline(prompt = "Pause. Press <Enter> to continue...") 
	}
	
	dev.off()

	# Longitude
	quartz(width = 10, height = 8)
	# pdf(paste('Figures/min,max,mean long by spp and year_strat_', Sys.Date(), '.pdf', sep=''), width=10, height=8)
	ncol = ncol(maxlong)
	ylims = c(min(rangesurvey$minlong), max(rangesurvey$maxlong))
	par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8)
	for(j in 1:ceiling(length(spp)/36)){
		ind = (j-1)*36
		for(i in 1:36){
			plot(yr, maxlong[ind+i,2:ncol], col='blue', type='o', xlab='', ylab='', main=maxlong$spp[ind+i], ylim = ylims, pch=16, cex=0.5)
			points(yr, minlong[ind+i,2:ncol], col='red', pch=16, cex=0.5, type='o')
			points(yr, centbiolong[ind+i,2:ncol], col='black', pch=16, cex=0.5, type='o')
			points(yr, centablong[ind+i,2:ncol], col='grey', pch=16, cex=0.5, type='o')
			points(yr, rangesurvey$minlong, col='grey', lty=2, type='l')
			points(yr, rangesurvey$maxlong, col='grey', lty=2, type='l')
		}
		#readline(prompt = "Pause. Press <Enter> to continue...") 
	}
	
	dev.off()
		
	# Biomass and abundance
	quartz(width = 10, height = 8)
	# pdf(paste('Figures/bio and abund by spp and year_strat_', Sys.Date(), '.pdf', sep=''), width=10, height=8)
	ncol = ncol(maxlong)
	par(mfrow = c(6,6), mai=c(0.3, 0.2, 0.2, 0.2), cex.main=0.7, cex.axis=0.8, mgp=c(1.5,0.4,0), tcl=-0.3)
	for(j in 1:ceiling(length(spp)/36)){
		ind = (j-1)*36
		for(i in 1:36){
			if(ind+i <= nrow(biomass)){
				b = biomass[ind+i,2:ncol]
				a = abundance[ind+i,2:ncol]
				ylims = c(0, max(b, na.rm=T))
				plot(yr, b, col='black', type='o', xlab='', ylab='', main=biomass$spp[ind+i], ylim = ylims, pch=16, cex=0.5)
				scale = max(b, na.rm=T)/max(a, na.rm=T)
				points(yr, a*scale, col='grey', pch=16, cex=0.5, type='o')
				at = axTicks(4)
				lab = prettyNum(at/scale, digits=2, format='g')
				axis(4, at=at, labels=lab, col='grey', col.ticks='grey', col.axis='grey', col.main='grey')
			}
		}
		#readline(prompt = "Pause. Press <Enter> to continue...") 
	}
	
	dev.off()

################################
## Rate of shift ###############
################################

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians/')
load('Output/Range_locations_strat2012-02-09.RData')
lh = read.csv('../../../Stanford/Life History & Collapse/LifeHistory Data/output/lh2011-02-10.csv')

require(nlme)
source('../gls.ar.aic 2012-01-18.R')

# Calc p-values from arima object
arimap <- function(object){
# https://stat.ethz.ch/pipermail/r-help/2009-June/202173.html
# cwp <--> ``coefficients with p-values''
#
     coef <- coef(object)
     if (length(coef) > 0) {
         mask <- object$mask
         sdev <- sqrt(diag(vcov(object)))
         t.rat <- rep(NA, length(mask))
         t.rat[mask] <- coef[mask]/sdev
         pt <- 2 * pnorm(-abs(t.rat))
         setmp <- rep(NA, length(mask))
         setmp[mask] <- sdev
         sum <- rbind(coef, setmp, t.rat, pt)
         dimnames(sum) <- list(c("coef", "s.e.", "t ratio", "p-value"),
             names(coef))
         return(sum)
     } else return(NA)
}

# p-values from lm object
lmp <- function(object){
	i = summary(object)$coef
	if(nrow(i)>0){
		return(i[,ncol(i)])
	} else {
		return(numeric(0))
	}
}

# Rate of shift calc
	# Set up dataframe
	len = length(spp)
	shift = data.frame(spp = spp)
	vars = c('min', 'max', 'centbio', 'centab')
	latlong = c('lat', 'long')
	mods = c('.a.lm', '.b.lm', '.p.lm', '.a.gls', '.b.gls', '.p.gls', '.type.gls')
	for(i in 1:length(vars)){
		for(ii in 1:length(latlong)){
			for(k in 1:length(mods)){
				shift[[paste(vars[i], latlong[ii], mods[k], sep='')]] = NA
			}
		}
	}
	vars = c('abundance', 'biomass')
	mods = c('.a.lm', '.b.lm', '.p.lm', '.a.gls', '.b.gls', '.p.gls', '.type.gls')
	for(i in 1:length(vars)){
		for(k in 1:length(mods)){
			shift[[paste(vars[i], mods[k], sep='')]] = NA
		}
	}
	
	
	# Set some parameter values
	options(warn=1) # print as they occur (0 is default)
	#options(warn=2) # stop if they occur (0 is default)
	lattol = 0.5 # how close to the edge of sampling range can a spp range limit be before I don't trust it
	longtol = 0.5 # how close to the edge of sampling range can a spp range limit be before I don't trust it
	minpts = 8 # minimum number of points necessary before calculating a trend. use 1 less for min number of differences

	vars = list(minlat=minlat, maxlat=maxlat, centbiolat=centbiolat, centablat=centablat, minlong=minlong, maxlong=maxlong, centbiolong=centbiolong, centablong=centablong, biomass=biomass, abundance=abundance)

	# Fill dataframe with model fits
	for(i in 1:length(spp)){
		print(paste(i, spp[i]))
		
		for(j in 1:length(vars)){
			nm = names(vars)[j]
			print(nm)
			lcols = grep('[[:digit:]]', names(vars[[j]])) # columns with latitude or other data

			# Set up dependent and independent variables for regression
			y = as.numeric(vars[[j]][i,lcols])
					
			# test whether <1/4 of pts are close to edge of sampling. test depends on whether lat or long
			if(length(grep('lat', nm))>0) test = sum(abs(y-rangesurvey$minlat)<lattol, na.rm=T) < sum(!is.na(y))/4 & sum(abs(y-rangesurvey$maxlat)<lattol, na.rm=T) < sum(!is.na(y))/4

			if(length(grep('long', nm))>0) test = sum(abs(y-rangesurvey$minlong)<longtol, na.rm=T) < sum(!is.na(y))/4 & sum(abs(y-rangesurvey$maxlong)<longtol, na.rm=T) < sum(!is.na(y))/4
			
			if(test & sum(!is.na(y))>=minpts){ # if test is true and <25% of values are < lattol away from min and max lat, and at least minpts points
				mod = lm(y ~ yr)
				shift[[paste(nm, '.a.lm', sep='')]][i] = coef(mod)[1]
				shift[[paste(nm, '.b.lm', sep='')]][i] = coef(mod)[2]
				shift[[paste(nm, '.p.lm', sep='')]][i] = lmp(mod)[2]

				if(all(y == y[1])){ # if perfectly flat
					shift[[paste(nm, '.a.gls', sep='')]][i] = coef(mod)[1]
					shift[[paste(nm, '.b.gls', sep='')]][i] = 0
					shift[[paste(nm, '.p.gls', sep='')]][i] = NA					
				} else {
					modg = gls.ar.aic(y ~ yr, abs=0.2, quiet=TRUE, maxiter=30) # see function above to find a best temporal autocorrelation structure
	
					shift[[paste(nm, '.a.gls', sep='')]][i] = coef(modg$mod)[1]
					shift[[paste(nm, '.b.gls', sep='')]][i] = coef(modg$mod)[2]
					shift[[paste(nm, '.p.gls', sep='')]][i] = summary(modg$mod)$tTable[2,4]
					
					shift[[paste(nm, '.type.gls', sep='')]][i] = modg$type
				}
			}
		}	
	}


# Add overall average rate of shift (lat and long)
# For lm and gls models, center of biomass and center of abundance
shift$kmyr.bio.lm = NA
shift$kmyr.bio.gls = NA
shift$kmyr.ab.lm = NA
shift$kmyr.ab.gls = NA

	invars = c('bio', 'bio', 'ab', 'ab')
	invars2 = c('lm', 'gls', 'lm', 'gls')
	for(i in 1:4){
		# Mean starting and ending locations, from the trends
		lat1 = shift[[paste('cent',invars[i],'lat.a.',invars2[i],sep='')]] + shift[[paste('cent',invars[i],'lat.b.',invars2[i],sep='')]]*yr[1]
		lat2 = shift[[paste('cent',invars[i],'lat.a.',invars2[i],sep='')]] + shift[[paste('cent',invars[i],'lat.b.',invars2[i],sep='')]]*yr[length(yr)]
		long1 = shift[[paste('cent',invars[i],'long.a.',invars2[i],sep='')]] + shift[[paste('cent',invars[i],'long.b.',invars2[i],sep='')]]*yr[1]
		long2 = shift[[paste('cent',invars[i],'long.a.',invars2[i],sep='')]] + shift[[paste('cent',invars[i],'long.b.',invars2[i],sep='')]]*yr[length(yr)]
		
		# To positive radians
		lat1 = lat1/180*pi
		lat2 = lat2/180*pi
		long1 = long1/180*pi + 2*pi
		long2 = long2/180*pi + 2*pi
		
		# Spherical law of cosines
		# 6371 is radius of earth in km
		dist = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(long2-long1))*6371
	
		# Haversine formula: equivalent answer to at least 7 decimal places
		#a = sin((lat2-lat1)/2)^2 + cos(lat1)*cos(lat2)*sin((long2-long1)/2)^2
		#dist2 = 6371*(2*atan2(sqrt(a), sqrt(1-a)))
		
		shift[[paste('kmyr.', invars[i], '.', invars2[i], sep='')]] = dist/(yr[length(yr)] - yr[1])
	}

# Prep spp names
# NOTE: check that subspp are in the right location (or choose a different one)
lh$spp = as.character(lh$spp)
lh$spp[grep('Acipenser oxyrinchus oxyrinchus', lh$spp)] = 'Acipenser oxyrinchus'
lh$spp[grep('Bathylagus stilbius', lh$spp)] = 'Leuroglossus stilbius'
lh$spp[grep('Clupea harengus harengus', lh$spp)] = 'Clupea harengus'
lh$spp[grep('Clupea pallasii pallasii', lh$spp)] = 'Clupea pallasii'
lh$spp[grep('Gasterosteus aculeatus aculeatus', lh$spp)] = 'Gasterosteus aculeatus'
lh$spp[grep('Helicolenus dactylopterus dactylopterus', lh$spp)] = 'Helicolenus dactylopterus'
lh$spp[grep('Hoplostethus mediterraneus mediterraneus', lh$spp)] = 'Hoplostethus mediterraneus'
lh$spp[grep('Myliobatis freminvillii', lh$spp)] = 'Myliobatis freminvillei'
lh$spp[grep('Osmerus mordax mordax', lh$spp)] = 'Osmerus mordax'
lh$spp[grep('Rhinoraja taranetzi', lh$spp)] = 'Bathyraja taranetzi'
lh$spp[grep('Scomberesox saurus saurus', lh$spp)] = 'Scomberesox saurus'
lh$spp[grep('Stomias boa boa', lh$spp)] = 'Stomias boa'

# Try to match lh names to shift names (latter has no spaces between genus and spp)
shift$spp_old = as.character(shift$spp)
shift$spp = NA
egginds = grep('eggcase', shift$spp_old)
for(i in 1:nrow(lh)){
	nm = unlist(strsplit(split=' ', lh$spp[i]))
	j = grep(nm[1], shift$spp_old)
	k = grep(nm[2], shift$spp_old)
	ii = intersect(j, k)
	ii = setdiff(ii, egginds) # remove matches to eggcases
	if(length(ii)==1){
		shift$spp[ii] = lh$spp[i]
	}
	if(length(ii)>1){
		print(paste(lh$spp[i], 'matched', shift$spp_old[ii]))
	}
}

# Spp that we haven't matched yet
shift$spp_old[is.na(shift$spp)]

shift$spp[shift$spp_old=='Bathylagusmilleri'] = 'Pseudobathylagus milleri'
shift$spp[shift$spp_old=='Bathyrajaparmifera'] = 'Bathyraja parmifera'
shift$spp[shift$spp_old=='Careproctusrastrinus'] = 'Careproctus rastrinus'
shift$spp[shift$spp_old=='Careproctuscandidus'] = 'Temnocora candida'
shift$spp[shift$spp_old=='Clupeapallasi'] = 'Clupea pallasii'
shift$spp[shift$spp_old=='Eumicrotremusbirulai'] = 'Eumicrotremus asperrimus'
shift$spp[shift$spp_old=='Leptagonusfrenatus'] = 'Sarritor frenatus'
shift$spp[shift$spp_old=='Leptagonusleptorhynchus'] = 'Sarritor leptorhynchus'
shift$spp[shift$spp_old=='Lumpenusmaculatus'] = 'Leptoclinus maculatus'
shift$spp[shift$spp_old=='Lumpenusmedius'] = 'Anisarchus medius'
shift$spp[shift$spp_old=='Myoxocephalusquadricornis'] = 'Triglopsis quadricornis'
shift$spp[shift$spp_old=='Platichthysstellatus'] = 'Platichthys stellatus'
shift$spp[shift$spp_old=='Pleuronectesquadrituberculatus'] = 'Pleuronectes quadrituberculatus'
shift$spp[shift$spp_old=='Percisjaponicus'] = 'Percis japonica'
shift$spp[shift$spp_old=='Triglopsforficata'] = 'Triglops forficatus'
shift$spp[shift$spp_old=='Triglopspingeli'] = 'Triglops pingelii'

# Spp that we still haven't matched yet
shift$spp_old[is.na(shift$spp)]
	# None

shiftlh = merge(shift, lh[,c('spp', 'FBname', 'AnaCat', 'MaxLengthTL', 'Troph', 'LongevityWild', 'Weight', 'DemersPelag', 'K', 'ComDepthMin', 'ComDepthMax', 'RepGuild1', 'RepGuild2', 'tmave2', 'fecundity', 'eggdiamave')], all.x=TRUE)
	shiftlh = droplevels(shiftlh)
	dim(shiftlh)

# Sample size
cols = grep("b.lm", names(shiftlh), fixed=TRUE)
for(i in 1:length(cols)){
	print(paste(names(shiftlh)[cols[i]], '   ', sum(!is.na(shiftlh[,cols[i]]))))
}

# Which spp shift significantly?
shiftlh[shiftlh$centbiolat.p.lm<0.05 & !is.na(shiftlh$centbiolat.p.lm), c('spp', 'FBname', 'centbiolat.b.lm')] # by lm

shiftlh[shiftlh$centbiolat.p.gls<0.05 & !is.na(shiftlh$centbiolat.p.gls), c('spp', 'FBname', 'centbiolat.b.gls')] # by gls


## Write out
write.csv(shiftlh, paste('Output/shiftlh_strat_', Sys.Date(), '.csv', sep=''))



##############################
## Climate Envelope Analysis
##############################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')
source('../allsubset.glm 2012-01-30.R')
source('../revgrep 2012-02-07.R')
datastrat = read.csv('Output/dataCEMstrat_2012-02-09.csv', row.names=1)
	datastrat$stratumfact = as.factor(datastrat$stratumfact)
	datastrat$yrfact = as.factor(datastrat$year)

	require(MASS)
	#require(pscl)
	require(mgcv)


# Set near-zero abundance
datastrat$wtcpuena = datastrat$wtcpue
datastrat$wtcpuena[datastrat$wtcpuena == 0] = 1e-5
	range(datastrat$wtcpuena)
	
# Set stratum to factor
datastrat$stratumfact = as.factor(datastrat$stratumfact)
	
## Pick spp
spp=sort(unique(datastrat$spp));spp # 28 spp

## Pick the training years
yrs = sort(unique(datastrat$year)); yrs; length(yrs)
trainingyrs = yrs # all years
trainingyrs = yrs[1:5]

options(warn=0)
for(i in 1:length(spp)){
	#inds = datastrat$year %in% trainingyrs & complete.cases(datastrat[,c('surftemp', 'bottemp', 'mintemp', 'maxtemp', 'julian', 'stratum')]) & datastrat$spp == spp[i]
	inds = datastrat$year %in% trainingyrs & complete.cases(datastrat[,c('surftemp', 'bottemp', 'julian', 'stratum')]) & datastrat$spp == spp[i]
	
	print(paste(spp[i], ': ', sum(datastrat$present[inds]), ' presences in training set', sep=''))

	# GLM with gaussian error (don't account for zeros)
	print('GLM gaussian')
	#glm1 = glm(wtcpue ~ surftemp + I(surftemp^2) + bottemp + I(bottemp^2) + mintemp + I(mintemp^2) + maxtemp + I(maxtemp^2) + julian + I(julian^2), family=gaussian, data=datastrat[inds,])
	glm1 = glm(wtcpue ~ surftemp + I(surftemp^2) + bottemp + I(bottemp^2) + julian + I(julian^2) + stratumfact + yrfact, family=gaussian, data=datastrat[inds,])
	#	summary(glm1)
	#	hist(resid(glm1)) # far too many zeros, right skewed
	glm2 = step(glm1)
	#	summary(glm2)
		
	# Hurdle glm gamma (two-part: pres/abs (glmhp) and then abund|pres (glmha))
	print('GLM presence poisson')
	#glmhp2 = allsubsets.glm(formula='present ~ surftemp + I(surftemp^2) + bottemp + I(bottemp^2) + mintemp + I(mintemp^2) + maxtemp + I(maxtemp^2) + julian + I(julian^2)', family='poisson', data=datastrat[inds,])	
	glmhp2 = allsubsets.glm(formula='present ~ surftemp + I(surftemp^2) + bottemp + I(bottemp^2) + julian + I(julian^2) + stratumfact + yrfact', family='poisson', data=datastrat[inds,])	
	#	summary(glmhp2)

	print('GLM abundance gamma')
	#glmha2 = allsubsets.glm(formula='wtcpue ~ surftemp + I(surftemp^2) + bottemp + I(bottemp^2) + mintemp + I(mintemp^2) + maxtemp + I(maxtemp^2) + julian + I(julian^2)', family=Gamma, data=datastrat[inds & datastrat$present,])	# will try all subsets that converge
	glmha2 = allsubsets.glm(formula='wtcpuena ~ surftemp + I(surftemp^2) + bottemp + I(bottemp^2) + julian + I(julian^2) + stratumfact + yrfact', family=Gamma, data=datastrat[inds,])
	#	summary(glmha2)
				
	# GAM gaussian
	print('GAM gaussian')
	#gam2 = gam(wtcpue ~ s(surftemp) + s(bottemp) + s(mintemp) + s(maxtemp) + s(julian), family=gaussian, data=datastrat[inds,], select=TRUE) # mgcv has a built-in model selection function
	gam2 = gam(wtcpue ~ s(surftemp) + s(bottemp) + s(julian) + stratumfact + yrfact, family=gaussian, data=datastrat[inds,], select=TRUE)
	#	summary(gam2)
	#	plot(gam2, se=TRUE, shade=TRUE, pages=1)

	# GAM Hurdle gamma (pres/abs and abundance)
	print('GAM presence poisson')
	#gamhp2 = gam(present ~ s(surftemp) + s(bottemp) + s(mintemp) + s(maxtemp) + s(julian), family=poisson, data=datastrat[inds,], select=TRUE) # mgcv has a built-in model selection function
	gamhp2 = gam(present ~ s(surftemp) + s(bottemp) + s(julian) + stratumfact + yrfact, family=poisson, data=datastrat[inds,], select=TRUE)
	#	summary(gamhp2)
	#	plot(gamhp2, se=TRUE, shade=TRUE, pages=1)

	#gamha2 = gam(wtcpue ~ s(surftemp) + s(bottemp) + s(mintemp) + s(maxtemp) + s(julian), family=Gamma, data=datastrat[inds & datastrat$present,], select=TRUE) 
	gamha2 = gam(wtcpuena ~ s(surftemp) + s(bottemp) + s(julian) + stratumfact + yrfact, family=Gamma, data=datastrat[inds,], select=TRUE)
	# mgcv has a built-in model selection function
	#	summary(gamha2)
	#	hist(resid(gamha2))
	#	plot(gamha2, se=TRUE, shade=TRUE, pages=1)


	# Save models
	mods = list(glm2=glm2, glmhp2=glmhp2, glmha2=glmha2,gam2=gam2, gamhp2=gamhp2, gamha2=gamha2)
	save(mods, file=paste('Output/CEMStrataModels/CEmodstratum_', spp[i], '_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.RData', sep=''))
}

##########################################
######## Plots and basic analysis ########
##########################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')
require(mgcv)
datastrat = read.csv('Output/dataCEMstrata_2012-02-09.csv', row.names=1)
	spp=sort(unique(datastrat$spp));spp # 24 spp



## Plot GAM fits for all the abundance models (multipage pdf)
	# folder = 'CEMStrataModels 2012-02-09'; daterange = '_1983to2010_'
	folder = 'CEMStrataModels 2012-02-16 half yrfact'; daterange = '_1983to1997_'
	pdf(width=6, height=6, file=paste('Figures/GAMabundstrata_fits', daterange, Sys.Date(), '.pdf', sep=''))
	for(i in 1:length(spp)){
		load(file=paste('Output/', folder, '/CEmodstratum_', spp[i], daterange, '2012-02-16.RData', sep=''))	
		plot(mods$gam2, shade=TRUE, se=TRUE, pages=1, main=spp[i], all.terms=TRUE)
		sum = summary(mods$gam2)	
		mtext(side=3, paste(signif(sum$dev.expl,2)*100, '% dev', sep=''))
	
	}
	
	dev.off()


## Calculate mean lat by year in observations and in models
	yrs = sort(unique(datastrat$year))
	folder = 'CEMStrataModels 2012-02-16 half yrfact'; daterange = c(1983,1997)

	files=list.files(paste('Output/', folder, sep='')) # only works if only one round of CEM models are in the directory

	len = length(files)
	a = numeric(len)

	# save observed and predicted positions for lat and long
	meanpos = list(0)
	
	# set up new data with yrfact set to latest fitted year
	newdata = datastrat
	newdata$yrfact[newdata$year>daterange[2]] = daterange[2]

	for(s in 1:length(files)){
		print(s)
		load(paste('Output/', folder, '/',files[s], sep=''))
		thisspp=strsplit(files[s], split=paste('CEmodstratum_|_', daterange[1], '|_', daterange[2], sep=''))[[1]][2] # extract spp name from file name
	
		thesemods = list(glm = list(mods$glm2), glmh = list(mods$glmhp2, mods$glmha2), gam = list(mods$gam2), gamh = list(mods$gamhp2, mods$gamha2))
		meanlat = list(rep(NA, length(yrs)))
		meanlong = list(rep(NA, length(yrs)))
		for(i in 1:(length(thesemods))) meanlat[[i+1]]=rep(NA,length(yrs))
		for(i in 1:(length(thesemods))) meanlong[[i+1]]=rep(NA,length(yrs))
		names(meanlat) = c('obs', 'glm', 'glmhurdle', 'gam', 'gamhurdle')
		names(meanlong) = names(meanlat)
	
		# Calculate mean lat and long by year for observed and for each model
		for(i in 1:length(yrs)){
			#inds = datastrat$year == yrs[i] & complete.cases(dataspp[,c('surftemp', 'bottemp', 'mintemp', 'maxtemp', 'julian')]) & datastrat$spp == thisspp
			inds = newdata$year == yrs[i] & complete.cases(newdata[,c('surftemp', 'bottemp', 'julian', 'stratum')]) & newdata$spp == thisspp
			if(sum(inds)>0){
				meanlat[[1]][i] = weighted.mean(newdata$lat[inds],w= newdata$wtcpue[inds], na.rm=T)
				meanlong[[1]][i] = weighted.mean(newdata$long[inds],w= newdata$wtcpue[inds], na.rm=T)
		
				for(j in 1:length(thesemods)){
					if(length(thesemods[[j]]) == 1){
						preds = predict(thesemods[[j]][[1]], newdata = newdata[inds,], type='response')
					}
					if(length(thesemods[[j]]) == 2){ # if a two-stage model (e.g., hurdle)
						preds = predict(thesemods[[j]][[1]], newdata = newdata[inds,], type='response')*predict(thesemods[[j]][[2]], newdata = newdata[inds,], type='response')			
					}
					preds[preds<0] = 0
					meanlat[[j+1]][i] = weighted.mean(newdata$lat[inds][!is.na(preds)], w = preds[!is.na(preds)])		
					meanlong[[j+1]][i] = weighted.mean(newdata$long[inds][!is.na(preds)], w = preds[!is.na(preds)])		
				}
			}
		}
	
		meanpos[[s]] = list(meanlat = meanlat, meanlong = meanlong)
		names(meanpos)[s] = thisspp
	}	

	### Save meanpos and direction
	save(meanpos, file = paste('Output/meanpos_strata_', daterange[1], 'to', daterange[2], '_', Sys.Date(), '.RData', sep=''))


### Plot mean position in observations and in models
### Also calculate trends in obs and predicted
	load('Output/meanpos_strata_2012-02-09.RData')
	load('Output/meanpos_strata_1983to1997_2012-02-16 yrfact.RData'); daterange=c(1983, 1997)

	# save observed and predicted slopes for lat and long
	direction = data.frame(spp=character(len), obslat = a, glmlat=a, glmhlat=a, gamlat=a, gamhlat=a, obslong=a, glmlong=a, glmhlong=a, gamlong=a, gamhlong=a, stringsAsFactors=FALSE)

	quartz(width=5, height=7)
	# pdf(paste('Figures/Meanlat&long_CEMstrata_', daterange[1], 'to', daterange[2], '_', Sys.Date(), '.pdf', sep=''), width=5, height=7)
	par(mfrow=c(4,2), mai=c(0.4,0.5,0.2,0.1), mgp = c(2,1,0))
	for(s in 1:length(files)){
		direction$spp[s] = as.character(spp[s])

		cols = c('black', 'blue', 'green', 'red', 'purple')
		ylims = range(unlist(lapply(meanpos[[s]]$meanlat, FUN=range, na.rm=T)))*c(0.99,1.01)
		plot(yrs, y<-meanpos[[s]]$meanlat$obs, type='o', pch=16, xlab='Year', ylab = 'Mean latitude', ylim=ylims, main=spp[s], col=cols[1])
			abline(t<-lm(y ~ yrs), col=cols[1], lty=2)
			abline(h = mean(y, na.rm=T), col='grey', lty=1)
			direction$obslat[s] = coef(t)[2]
		points(yrs, y<-meanpos[[s]]$meanlat$glm, type='o', pch=16, col=cols[2])
			abline(t<-lm(y ~ yrs), col=cols[2], lty=2)
			direction$glmlat[s] = coef(t)[2]
		points(yrs, y<-meanpos[[s]]$meanlat$glmhurdle, type='o', pch=16, col=cols[3])
			abline(t<-lm(y ~ yrs), col=cols[3], lty=2)
			direction$glmhlat[s] = coef(t)[2]
		points(yrs, y<-meanpos[[s]]$meanlat$gam, type='o', pch=16, col=cols[4])
			abline(t<-lm(y ~ yrs), col=cols[4], lty=2)
			direction$gamlat[s] = coef(t)[2]
		points(yrs, y<-meanpos[[s]]$meanlat$gamhurdle, type='o', pch=16, col=cols[5])
			abline(t<-lm(y ~ yrs), col=cols[5], lty=2)
			direction$gamhlat[s] = coef(t)[2]
		#legend('topleft', legend=c('Observed', 'Poisson GLM', 'Hurdle GLM', 'GAM', 'Hurdle GAM'), col= cols, pch=16, lty=1, bty='n')
		
		ylims = range(unlist(lapply(meanpos[[s]]$meanlong, FUN=range, na.rm=T)))*c(1.01,0.99)
		plot(yrs, y<-meanpos[[s]]$meanlong$obs, type='o', pch=16, xlab='Year', ylab = 'Mean longitude', ylim=ylims, main=spp[s], col=cols[1])
			abline(t<-lm(y ~ yrs), col=cols[1], lty=2)
			abline(h = mean(y, na.rm=T), col='grey', lty=1)
			direction$obslong[s] = coef(t)[2]
		points(yrs, y<-meanpos[[s]]$meanlong$glm, type='o', pch=16, col=cols[2])
			abline(t<-lm(y ~ yrs), col=cols[2], lty=2)
			direction$glmlong[s] = coef(t)[2]
		points(yrs, y<-meanpos[[s]]$meanlong$glmhurdle, type='o', pch=16, col=cols[3])
			abline(t<-lm(y ~ yrs), col=cols[3], lty=2)
			direction$glmhlong[s] = coef(t)[2]
		points(yrs, y<-meanpos[[s]]$meanlong$gam, type='o', pch=16, col=cols[4])
			abline(t<-lm(y ~ yrs), col=cols[4], lty=2)
			direction$gamlong[s] = coef(t)[2]
		points(yrs, y<-meanpos[[s]]$meanlong$gamhurdle, type='o', pch=16, col=cols[5])
			abline(t<-lm(y ~ yrs), col=cols[5], lty=2)
			direction$gamhlong[s] = coef(t)[2]
	}
	plot(0,0, bty='n', col='white', xlab='', ylab='', xaxt='n', yaxt='n')
	legend('center', legend=c('Observed', 'Poisson GLM', 'Hurdle GLM', 'GAM', 'Hurdle GAM'), col= cols, pch=16, lty=1, bty='n')
	
	dev.off()

	# Write out direction	
	write.csv(direction, file=paste('Output/direction_strata_', daterange[1], 'to', daterange[2], '_', Sys.Date(), '.csv', sep=''))


## Are any model trends correlated to observed trends in lat or long?
	cor.test(direction$obslat, direction$glmlat)
	cor.test(direction$obslat, direction$glmhlat)
	cor.test(direction$obslat, direction$gamlat)
	cor.test(direction$obslat, direction$gamhlat)
	cor.test(direction$obslong, direction$glmlong)
	cor.test(direction$obslong, direction$glmhlong)
	cor.test(direction$obslong, direction$gamlong)
	cor.test(direction$obslong, direction$gamhlong)

	plot(direction$gamhlat, direction$obslat)
		abline(a=0,b=1)
	plot(direction$gamlat, direction$obslat)
		abline(a=0,b=1)
	plot(direction$gamlong, direction$obslong)
		abline(a=0,b=1)

# Are any model/spp predictions correlated to observed?
	cor.p = direction # for p-values
		cor.p = cor.p[,names(cor.p)[!(names(cor.p) %in% c('obslat', 'obslong'))]] # drop obslat and obslong columns
	cor = cor.p # for correlation coefficient
	for(s in 1:length(spp)){
		cor.p$glmlat[s] = (c <- cor.test(meanpos[[s]]$meanlat$obs, meanpos[[s]]$meanlat$glm))$p.value
		cor$glmlat[s] = c$estimate
		cor.p$glmhlat[s] = (c <- cor.test(meanpos[[s]]$meanlat$obs, meanpos[[s]]$meanlat$glmhurdle))$p.value
		cor$glmhlat[s] = c$estimate
		cor.p$gamlat[s] = (c <- cor.test(meanpos[[s]]$meanlat$obs, meanpos[[s]]$meanlat$gam))$p.value
		cor$gamlat[s] = c$estimate
		cor.p$gamhlat[s] = (c <- cor.test(meanpos[[s]]$meanlat$obs, meanpos[[s]]$meanlat$gamhurdle))$p.value
		cor$gamhlat[s] = c$estimate
		cor.p$glmlong[s] = (c <- cor.test(meanpos[[s]]$meanlong$obs, meanpos[[s]]$meanlong$glm))$p.value
		cor$glmlong[s] = c$estimate
		cor.p$glmhlong[s] = (c <- cor.test(meanpos[[s]]$meanlong$obs, meanpos[[s]]$meanlong$glmhurdle))$p.value
		cor$glmhlong[s] = c$estimate
		cor.p$gamlong[s] = (c <- cor.test(meanpos[[s]]$meanlong$obs, meanpos[[s]]$meanlong$gam))$p.value
		cor$gamlong[s] = c$estimate
		cor.p$gamhlong[s] = (c <- cor.test(meanpos[[s]]$meanlong$obs, meanpos[[s]]$meanlat$gamhurdle))$p.value
		cor$gamhlong[s] = c$estimate
		print(paste(spp[s], ': ', paste(names(cor.p)[cor.p[s,]<0.05 & cor[s,] > 0], collapse='; '), sep=''))
	}
	
	colSums(cor.p[,2:ncol(cor.p)]<0.05) # how many significant fits per model type?

	colMeans(cor[,2:ncol(cor)]) # average correlation per model
	apply(cor[,2:ncol(cor)], MARGIN=2, FUN=max) # max correlation per model


	
##############################################
## Correlate obs trends to predicted and LH ##
##############################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')

direction = read.csv('Output/direction_strata_1983to1997_2012-02-16 yrfact.csv', row.names=1, stringsAsFactors=FALSE)
	dim(direction)
shiftlh = read.csv('Output/shiftlh_strat_2012-02-09.csv', row.names=1, stringsAsFactors=FALSE)
dim(shiftlh)
	shiftlh$spptrunc = sub(" ", '', shiftlh$spp)
	
## Fix spp names and merge
direction$spptrunc = direction$spp
direction$spptrunc[direction$spptrunc=='Clupeapallasi'] = 'Clupeapallasii'
direction$spptrunc[direction$spptrunc=='Leptagonusfrenatus'] = 'Sarritorfrenatus'
direction$spptrunc[direction$spptrunc=='Triglopsforficata'] = 'Triglopsforficatus'
	
shiftlhpred = merge(shiftlh, direction[,-(names(direction)=='spp')], by='spptrunc')
	dim(shiftlhpred)
	setdiff(direction$spptrunc, shiftlhpred$spptrunc)
	
# Write out
	write.csv(shiftlhpred, paste('Output/shiftlhpred_', Sys.Date(), '.csv', sep=''))
	
## Correlations
	shiftlhpred = read.csv('Output/shiftlhpred_2012-02-09.csv')
	
	i = complete.cases(shiftlhpred[,c('gamhlat', 'MaxLengthTL', 'K', 'Troph', 'AnaCat')])
	mod = lm(obslat ~ gamhlat + MaxLengthTL + K + Troph + AnaCat, data=shiftlhpred[i,])
		summary(mod)
	mod2 = step(mod)
		summary(mod2)

	i = complete.cases(shiftlhpred[,c('gamhlat', 'MaxLengthTL', 'K', 'Troph')]) # & shiftlhpred$spptrunc != 'Lycodesbrevipes'
	mod = lm(obslat ~ gamhlat + MaxLengthTL + K + Troph, data=shiftlhpred[i,])
		summary(mod)
	mod2 = step(mod)
	# 	i = complete.cases(shiftlhpred[,c('gamhlat', 'Troph')])
	# mod2 = lm(obslat ~ gamhlat + Troph, data=shiftlhpred[i,])
		summary(mod2)
		plot(fitted(mod2), residuals(mod))
		plot(x<-fitted(mod2), y<-shiftlhpred$obslat[i], xlab='Predicted rate of shift (° lat/yr)', ylab = 'Observed rate of shift (° lat/yr)', main = 'AFSC Aleutians, obslat ~ gamhlat + K, n = 28 spp')
			abline(0,1)

	library(leaps)
	library(car)
	leaps = regsubsets(obslat ~ gamhlat + MaxLengthTL + K + Troph + AnaCat, data=shiftlhpred, nbest = 1, method='exhaustive')
	
	plot(leaps, scale='bic')
	s = summary(leaps)
	subsets(leaps, statistic='rsq')
	subsets(leaps, statistic='bic')
	
	mod = lm(obslat ~ AnaCat + K + Troph, data=shiftlhpred)
		summary(mod)

	# Pure prediction
	i = shiftlhpred$spptrunc != 'Lycodesbrevipes'
	mod = lm(obslat ~ gamhlat, data=shiftlhpred[i,])
		summary(mod)
		plot(x<-fitted(mod), y<-shiftlhpred$obslat[i], xlab='Predicted rate of shift (° lat/yr)', ylab = 'Observed rate of shift (° lat/yr)', main = 'AFSC EBS, obslat ~ gamhlat, n = 24 spp')
			abline(0,1)

