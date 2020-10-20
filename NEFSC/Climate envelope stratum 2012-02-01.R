# Climate-envelope models

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')

#################
### DATA PREP ###
#################

# FIRST: Prep datatrim file in Climate envelope 2011-12-12.R

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')

datatrim = read.csv('Output/dataspp_Brosme brosme_Spring_2011-12-08.csv', row.names=1); spp='Brosme brosme'; season='Spring'
datatrim = read.csv('Output/dataspp_Conger oceanicus_Spring_2011-12-08.csv', row.names=1); spp='Conger oceanicus'; season='Spring'
datatrim = read.csv('Output/dataspp_Gadus morhua_Spring_2012-02-01.csv', row.names=1); spp='Gadus morhua'; season='Spring'
datatrim = read.csv('Output/dataspp_Merluccius bilinearis_Spring_2011-12-08.csv', row.names=1); spp='Merluccius bilinearis'; season='Spring'
datatrim = read.csv('Output/dataspp_Merluccius albidus_Spring_2011-12-08.csv', row.names=1); spp='Merluccius albidus'; season='Spring'
datatrim = read.csv('Output/dataspp_Homarus americanus_Spring_2011-12-08.csv', row.names=1); spp='Homarus americanus'; season='Spring'
datatrim = read.csv('Output/dataspp_Clupea harengus_Spring_2011-12-08.csv', row.names=1); spp='Clupea harengus'; season='Spring'

# Summarize by stratum
datastrat = with(datatrim, aggregate(list(expcatnum = expcatnum, cpue = cpue, lat = lat, lon = lon, surftemp = surftemp, bottemp = bottemp, surfsal = surfsal, botsal = botsal, mintemp = mintemp, mintempmnth = mintempmnth, maxtemp = maxtemp, maxtempmnth = maxtempmnth, month = month, julian = julian), by = list(spp = spp_name, stratum = stratum, year = year), FUN=mean, na.rm=T))
	dim(datastrat)
	table(datastrat$stratum, datastrat$year) # one for almost all strata: good

	# add variance calculation
var = with(datatrim, aggregate(list(expcatnumvar = expcatnum, cpuevar = cpue, surftempvar = surftemp, bottempvar = bottemp, surfsalvar = surfsal, botsalvar = botsal, mintempvar = mintemp, mintempmnthvar = mintempmnth, maxtempvar = maxtemp, maxtempmnthvar = maxtempmnth, monthvar = month, julianvar = julian), by = list(spp = spp_name, stratum = stratum, year = year), FUN=var, na.rm=T))
	dim(var)
	datastrat = merge(datastrat, var)
	dim(datastrat)

write.csv(datastrat, paste('Output/datastrat_', spp, '_', season, '_', Sys.Date(), '.csv', sep=''))

##################
## Examine data ##
##################
datastrat = read.csv("Output/datastrat_Gadus morhua_Spring_2012-02-01.csv", row.names=1); spp = 'Gadus morhua'; season = 'Spring'


## When do we have data?
	table(datastrat$year, !is.na(datastrat$surftemp))
	table(datastrat$year, !is.na(datastrat$bottemp))
	table(datastrat$year, !is.na(datastrat$mintemp))
	table(datastrat$year, !is.na(datastrat$maxtemp))
	table(datastrat$year, !is.na(datastrat$botsal))

	i = !is.na(datastrat$surftemp)
	table(datastrat$stratum[i], datastrat$year[i])
	i = !is.na(datastrat$bottemp)
	table(datastrat$stratum[i], datastrat$year[i])
	i = !is.na(datastrat$mintemp)
	table(datastrat$stratum[i], datastrat$year[i])
	i = !is.na(datastrat$maxtemp)
	table(datastrat$stratum[i], datastrat$year[i])
	i = !is.na(datastrat$botsal)
	table(datastrat$stratum[i], datastrat$year[i])


# Examine data distributions
	hist(datastrat$expcatnum)
	pairs(~ expcatnum + surftemp + bottemp + mintemp + maxtemp, data=datastrat)
	
	hist(datastrat$surftemp)
		hist(log(datastrat$surftemp))
	hist(datastrat$bottemp)
		hist(log(datastrat$bottemp))
	hist(datastrat$mintemp)
		hist(log(datastrat$mintemp))
	hist(datastrat$maxtemp)
		hist(log(datastrat$maxtemp))

	pairs(~ expcatnum + lsurftemp + lbottemp + lmintemp + maxtemp+lat+lon, data=datastrat)

# Plot variance vs. mean
	range(datastrat$expcatnum)
	plot(datastrat$expcatnum, datastrat$expcatnumvar, log='xy')

	plot(datastrat$cpue, datastrat$cpuevar, log='xy')

	plot(datastrat$cpue, datastrat$expcatnum) # what's the difference between expcatnum and cpue?



# Plot maps of environmental variables
make3Dgrid = function(x,y,z,xbreaks=20,ybreaks=xbreaks){
		xgrid = seq(min(x), max(x), length.out=xbreaks)
		ygrid = seq(min(y), max(y), length.out=ybreaks)
		xmids = ((c(NA,xgrid) + c(xgrid,NA))/2)[2:length(xgrid)]
		ymids = ((c(NA,ygrid) + c(ygrid,NA))/2)[2:length(ygrid)]
		zout = matrix(NA, nrow=length(xmids), ncol=length(ymids))
		for(i in 2:length(xgrid)){
			for(j in 2:length(ygrid)){
				k = x>xgrid[i-1] & x<xgrid[i] & y>ygrid[j-1] & y<ygrid[j]
				zout[i-1,j-1] = mean(z[k], na.rm=T)
			}
		}
		return(out = list(x=xmids, y=ymids, z=zout))
	}

	# All years
	
	z= datatrim$bottemp; levs = seq(1,15,by=1)
	z=datatrim$surftemp; levs = seq(1,12,by=0.5)
	z=datatrim$maxtemp; levs = seq(14,30,by=1)
	z=datatrim$mintemp; levs = seq(2,15,by=0.5)
	out = make3Dgrid(datatrim$lon, datatrim$lat, z, xbreaks=20)
	filled.contour(out, levels=levs, col=rev(heat.colors(length(levs)-1)))

	# 5-yr chunks
	z= datatrim$bottemp; levs = seq(1,15,by=1); var = 'bottemp'
	z=datatrim$surftemp; levs = seq(1,12,by=0.5); var = 'surftemp'
	z=datatrim$maxtemp; levs = seq(14,30,by=1); var = 'maxtemp'
	z=datatrim$mintemp; levs = seq(2,15,by=0.5); var = 'mintemp'
	
	# pdf(width=5, height=3, paste('Figures/Maps_', var, '_', season, '_', Sys.Date(), '.pdf', sep=''))
	yrs = seq(1968,2012, by=5)
	for(j in 2:length(yrs)){
		#quartz(width=5,height=3)
		par(mai=c(0.5,0.5,0.2,0.5))
		i = datatrim$year >= yrs[j-1] & datatrim$year < yrs[j]
		out = make3Dgrid(datatrim$lon[i], datatrim$lat[i], z[i], xbreaks=25)
		filled.contour(out, levels=levs, col=rev(heat.colors(length(levs)-1)), main=yrs[j])
	}
	
	dev.off()

# Calculate mean temperature by stratum in each year
		
	btemp = reshape(datastrat[,c('stratum', 'year', 'bottemp')], v.names = 'bottemp', idvar = 'stratum', timevar = 'year', direction = 'wide')
	sstemp = reshape(datastrat[,c('stratum', 'year', 'surftemp')], v.names = 'surftemp', idvar = 'stratum', timevar = 'year', direction = 'wide')

	yr = sort(unique(datastrat$year))
	tcols = grep('[[:digit:]]', names(btemp))
	btemp$trend = NA; sstemp$trend = NA
	btemp$p = NA; sstemp$p = NA
	for(i in 1:nrow(btemp)){
		mod = lm(as.numeric(btemp[i,tcols]) ~ yr)
		btemp$trend[i] = coef(mod)[2]
		btemp$p[i] = summary(mod)$coefficients[2,4]
		mod = lm(as.numeric(sstemp[i,tcols]) ~ yr)
		sstemp$trend[i] = coef(mod)[2]		
		sstemp$p[i] = summary(mod)$coefficients[2,4]
	}
	dim(btemp); dim(sstemp)
	range(btemp$trend)
	range(sstemp$trend)

	latlon = with(datastrat, aggregate(list(lat= lat, lon = lon), by=list(stratum = stratum), FUN=mean, na.rm=T))
	
	btemp = merge(btemp, latlon)
	sstemp = merge(sstemp, latlon)
	dim(btemp); dim(sstemp)

	# Plot map of temperature trends
	par(mfrow=c(1,2))
	plot(btemp$lon, btemp$lat, col = c('red', 'blue')[1+ (btemp$trend<0)], pch=c(1,16)[1+(btemp$p<0.05)], cex=1.5, main='Bottom temperature trend by stratum 68-09', xlab='Longitude', ylab = 'Latitude')
	plot(sstemp$lon, sstemp$lat, col = c('red', 'blue')[1+ (sstemp$trend<0)], pch=c(1,16)[1+(sstemp$p<0.05)], cex=1.5, main='SST trend by stratum 68-09', xlab='Longitude', ylab = 'Latitude')
	legend('bottomright', legend=c('Warmer', 'Cooler', 'p<0.05', 'p>0.05'), pch=c(16,16,16,1), col=c('red', 'blue', 'black', 'black'))

# Plot Mean change in temperature?
	source('../../gls.ac.aic 2012-01-18.R')
	# btemp
	bmod = lm(btemp ~ yr)	
	bmodg = gls.ar.aic(btemp ~ yr, abs=0.2, quiet=TRUE, maxiter=30)

	summary(bmod)
	coef(bmod)[2]*(max(yr)-min(yr))
	summary(bmodg$mod)

	# SST
	smod = lm(sstemp ~ yr)	
	smodg = gls.ar.aic(sstemp ~ yr, abs=0.2, quiet=TRUE, maxiter=30)

	summary(smod)
	coef(smod)[2]*(max(yr)-min(yr))
	summary(smodg$mod)

	# Plot
	quartz(width=6, height=5)
	# pdf(paste('Figures/Temp trends ', Sys.Date(), '.pdf', sep=''), width=6, height=5)
	plot(yr, btemp, ylim=c(0,12), type='o', ylab='Temperature (°C)', xlab='Year', main='Northeast US Spring')
	points(yr, sstemp, col='blue', type='o')
	legend('topright', legend=c('SST', 'Bottom'), col=c('blue', 'black'), lty=1, pch=1, bty='n')
	abline(smod, lty=2, col='blue')
	abline(bmod, lty=2)	
	
	dev.off()


	
#############
### MODEL ###	
#############
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/Range shifts')

datatrim = read.csv('Output/dataspp_Brosme brosme_Spring_2011-12-08.csv', row.names=1); spp='Brosme brosme'; season='Spring'
datatrim = read.csv('Output/dataspp_Conger oceanicus_Spring_2011-12-08.csv', row.names=1); spp='Conger oceanicus'; season='Spring'
datatrim = read.csv('Output/dataspp_Gadus morhua_Spring_2011-12-08.csv', row.names=1); spp='Gadus morhua'; season='Spring'
datatrim = read.csv('Output/dataspp_Merluccius bilinearis_Spring_2011-12-08.csv', row.names=1); spp='Merluccius bilinearis'; season='Spring'
datatrim = read.csv('Output/dataspp_Merluccius albidus_Spring_2011-12-08.csv', row.names=1); spp='Merluccius albidus'; season='Spring'
datatrim = read.csv('Output/dataspp_Homarus americanus_Spring_2011-12-08.csv', row.names=1); spp='Homarus americanus'; season='Spring'
datatrim = read.csv('Output/dataspp_Clupea harengus_Spring_2011-12-08.csv', row.names=1); spp='Clupea harengus'; season='Spring'
     
## Models
	require(MASS)
	require(pscl)
	require(mgcv)

	# GLM with poisson errors
	inds = datatrim$year %in% 1968:1979 & complete.cases(datatrim[,c('surftemp', 'bottemp', 'mintemp', 'maxtemp')])
	# inds = complete.cases(datatrim[,c('surftemp', 'bottemp', 'mintemp', 'maxtemp')]) # the full dataset
		sum(inds)
		nrow(datatrim)
		sum(datatrim$expcatnum[inds])
	
	# GLM poisson error to run aic
	glm1 = glm(expcatnum ~ lsurftemp + I(lsurftemp^2) + lbottemp + I(lbottemp^2) + lmintemp + I(lmintemp^2) + maxtemp + I(maxtemp^2), family=poisson, data=datatrim[inds,])
		summary(glm1)
	glm2 = step(glm1)
		summary(glm2)
		
	#glmb = glm(expcatnum ~ lbottemp + I(lbottemp^2), family=poisson, data=datatrim[inds,]) # only bottom temp
#		summary(glmb)
#
#	glms = glm(expcatnum ~ lsurftemp + I(lsurftemp^2), family=poisson, data=datatrim[inds,]) # only bottom temp
#		summary(glms)
#		x = seq(0,5, length.out=50)
#		plot(x,predict(glms, newdata=data.frame(lsurftemp=x), type='response'), ylim=c(0,5))
#		points(datatrim$lsurftemp[inds], datatrim$expcatnum[inds], col='red')
#
#
#	glmmin = glm(expcatnum ~ lmintemp + I(lmintemp^2), family=poisson, data=datatrim[inds,]) # only bottom temp
#		summary(glmmin)
#		x = seq(0,10,length.out=50)
#		plot(x,predict(glmmin, newdata=data.frame(lmintemp=x), type='response'), ylim=c(0,5))
#		points(datatrim$lmintemp[inds], datatrim$expcatnum[inds], col='red')
#
#	glmmax = glm(expcatnum ~ maxtemp + I(maxtemp^2), family=poisson, data=datatrim[inds,]) # only bottom temp
#		summary(glmmax)
#		x = seq(15,22, length.out=50)
#		plot(x,predict(glmmax, newdata=data.frame(maxtemp=x), type='response'), ylim=c(0,5))
#		points(datatrim$maxtemp[inds], datatrim$expcatnum[inds], col='red')

	
	# GLM with quasipoisson (same formula as poisson errors, same coefs, different p-values)
	glmq1 = glm(formula(glm2), family=quasipoisson, data=datatrim[inds,])
		summary(glmq1)
		
	# Zero-inflated poisson
	glmz1 = zeroinfl(expcatnum ~ lsurftemp + I(lsurftemp^2) + lbottemp + I(lbottemp^2) + lmintemp + I(lmintemp^2) + maxtemp + I(maxtemp^2), dist='poisson', data=datatrim[inds,], control=zeroinfl.control(trace=FALSE))
		summary(glmz1)
	glmz2 = step(glmz1)
		summary(glmz2)
		
	# GAM
	gam1 = gam(expcatnum ~ s(lsurftemp) + s(lbottemp) + s(lmintemp) + s(maxtemp), family=poisson, data=datatrim[inds,])
		summary(gam1)
		par(mfrow=c(2,2)); plot(gam1, se=TRUE, shade=TRUE)
	# gam2 = step.gam(gam1, scope=list('lsurftemp' = ~1+lsurftemp+s(lsurftemp), 'lbottemp' = ~1+lbottemp+s(lbottemp), 'lmintemp' = ~1+lmintemp+s(lmintemp), 'maxtemp'=~1+maxtemp+s(maxtemp))) # for package gam

	gam2 = gam(expcatnum ~ s(lsurftemp) + s(lbottemp) + s(lmintemp) + s(maxtemp), family=poisson, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function
		summary(gam2)
		par(mfrow=c(2,2)); plot(gam2, se=TRUE, shade=TRUE)


	# Save models
	mods = list(glm1=glm1, glm2=glm2, glmq1=glmq1, glmz1=glmz1, glmz2=glmz2, gam1=gam1, gam2=gam2)
	save(mods, file=paste('Output/CEmods_', spp, '_', season, '_', Sys.Date(), '.RData', sep=''))

#################
## Plot models ##
#################

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')
require(MASS)
require(pscl)
require(mgcv)
source('../../mave 2012-01-24.R')

load('Output/CEmods_Brosme brosme_Spring_2011-12-09.RData'); spp='Brosme brosme'; season='Spring'
load('Output/CEmods_Merluccius bilinearis_Spring_2011-12-09.RData'); spp='Merluccius bilinearis'; season='Spring'
load('Output/CEmods_Gadus morhua_Spring_2011-12-09.RData'); spp='Gadus morhua'; season='Spring'; datatrim = read.csv('Output/dataspp_Gadus morhua_Spring_2011-12-08.csv', row.names=1)

# If mods was saved without names
names(mods)
glm1 = mods[[1]]
glm2 = mods[[2]]
glmq1 = mods[[3]]
glmz1 = mods[[4]]
glmz2 = mods[[5]]
gam1 = mods[[6]]
gam2 = mods[[7]]


# Calculate mean lat and long
	yrs = 1968:2004

	 mods = list(glm2, glmz2, gam2)
	# mods = list(glmb, glmz2, gam2) # bottom temp glm instead of regular glm
	# mods = list(glmb, glms, glmmin, glmmax) # bottom temp glm instead of regular glm
	meanlat = list(rep(NA, length(yrs)))
	meanlong = list(rep(NA, length(yrs)))
	for(i in 1:(length(mods))) meanlat[[i+1]]=rep(NA,length(yrs))
	for(i in 1:(length(mods))) meanlong[[i+1]]=rep(NA,length(yrs))
	names(meanlat) = c('obs', 'glm', 'zip glm', 'gam')
	names(meanlong) = c('obs', 'glm', 'zip glm', 'gam')

	for(i in 1:length(yrs)){
		inds = datatrim$year == yrs[i] & complete.cases(datatrim[,c('surftemp', 'bottemp', 'mintemp', 'maxtemp')])
		meanlat[[1]][i] = weighted.mean(datatrim$lat[inds],w= datatrim$expcatnum[inds], na.rm=T)
		meanlong[[1]][i] = weighted.mean(datatrim$lon[inds],w= datatrim$expcatnum[inds], na.rm=T)

		for(j in 1:length(mods)){
			preds = predict(mods[[j]], newdata = datatrim[inds,], type='response')
			preds[preds<0] = 0
			meanlat[[j+1]][i] = weighted.mean(datatrim$lat[inds][!is.na(preds)], w = preds[!is.na(preds)])		
			meanlong[[j+1]][i] = weighted.mean(datatrim$lon[inds][!is.na(preds)], w = preds[!is.na(preds)])		
		}
	}


##########################
######## Plots ###########
##########################


# Plot predictions and reality as maps
	quartz(width=12, height=7)
	# pdf(paste('Figures/Maps_CEM_', spp, '_', Sys.Date(), '.pdf', sep=''), width=12, height=7)
	yrtoplot = c(1970, 1975, 1980, 1985, 1990, 1995, 2000, 2004)
	c = datatrim$expcatnum
	a = max(log(c), na.rm=T)
	cols = rev(rainbow(n=ceiling(a)+1, start=0, end=4/6))
	xlims = c(-76,-65)
	ylims = c(35,45)

	mods = list(glm2, glmz2, gam2)
	par(mfcol=c(3,length(yrtoplot)), mai=c(0.3, 0.3, 0.2, 0.1)) # use mfcol to fill by columns
	for(i in 1:length(yrtoplot)){
		for(j in 1:length(mods)){
			inds = datatrim$year == yrtoplot[i] & datatrim$expcatnum > 0 & complete.cases(datatrim[,c('surftemp', 'bottemp', 'mintemp', 'maxtemp')])
			c = datatrim$expcatnum[inds]
			plot(datatrim$lon[inds], datatrim$lat[inds], pch=16, xlab='', cex=ceiling(log(c))/a+0.8, ylab='', xlim=xlims, ylim=ylims, col = 'black', main=yrtoplot[i])
			meanlat = weighted.mean(datatrim$lat[inds],w= datatrim$expcatnum[inds])
			points(-76, meanlat, cex=2)
			if(i==1) baselat = meanlat
			abline(h=baselat, lty=2, col='grey')

			preds = predict(mods[[j]], newdata = datatrim[inds,], type='response')
			preds[preds<0] = 0
			points(datatrim$lon[inds], datatrim$lat[inds], pch=16, xlab='', cex=ceiling(log(preds))/a+0.5, ylab='', xlim=xlims, ylim=ylims, col = 'red')
			points(-76, weighted.mean(datatrim$lat[inds][!is.na(preds)], w = preds[!is.na(preds)]), cex=2, col='red')		
		}
	}
	
	dev.off()
	

# Plot predictions and reality as graphs of mean lat
	
	# Plot mean lat and long over time
	quartz(width=7, height=7)
	# pdf(paste('Figures/Meanlat&long_CEM_', spp, '_', season, '_', Sys.Date(), '.pdf', sep=''), width=7, height=6)

	par(mfrow=c(2,1), mai=c(0.7,0.6,0.5, 0.3), mgp = c(2, 0.7, 0))
	cols = c('black', 'blue', 'green', 'red')
	ylims = range(unlist(lapply(meanlat, FUN=range, na.rm=T)))*c(0.99,1.01)
	plot(yrs, meanlat[[1]], type='o', pch=16, xlab='Year', ylab = 'Mean latitude', ylim=ylims, main=spp, col=cols[1])
		abline(lm(meanlat[[1]] ~ yrs), col=cols[1], lty=2)
		abline(h = mean(meanlat[[1]], na.rm=T), col='grey', lty=1)
	points(yrs, meanlat[[2]], type='o', pch=16, col=cols[2])
		abline(lm(meanlat[[2]] ~ yrs), col=cols[2], lty=2)
	points(yrs, meanlat[[3]], type='o', pch=16, col=cols[3])
		abline(lm(meanlat[[3]] ~ yrs), col=cols[3], lty=2)
	points(yrs, meanlat[[4]], type='o', pch=16, col=cols[4])
		abline(lm(meanlat[[4]] ~ yrs), col=cols[4], lty=2)
	legend('topleft', legend=c('Observed', 'Poisson GLM', 'Zero-inflated poisson GLM', 'GAM'), col= cols, pch=16, lty=1, bty='n')
	
	ylims = range(unlist(lapply(meanlong, FUN=range, na.rm=T)))*c(1.01,0.99)
	plot(yrs, meanlong[[1]], type='o', pch=16, xlab='Year', ylab = 'Mean longitude', ylim=ylims, main=spp, col=cols[1])
		abline(lm(meanlong[[1]] ~ yrs), col=cols[1], lty=2)
		abline(h = mean(meanlong[[1]], na.rm=T), col='grey', lty=1)
	points(yrs, meanlong[[2]], type='o', pch=16, col=cols[2])
		abline(lm(meanlong[[2]] ~ yrs), col=cols[2], lty=2)
	points(yrs, meanlong[[3]], type='o', pch=16, col=cols[3])
		abline(lm(meanlong[[3]] ~ yrs), col=cols[3], lty=2)
	points(yrs, meanlong[[4]], type='o', pch=16, col=cols[4])
		abline(lm(meanlong[[4]] ~ yrs), col=cols[4], lty=2)

		dev.off()
	
	# Plot mean lat and long over time with moving averages (2-yr)
	quartz(width=7, height=7)
	w = 2
	# pdf(paste('Figures/Meanlat&long_CEM_mave', w, '_', spp, '_', season, '_', Sys.Date(), '.pdf', sep=''), width=7, height=6)
	

	par(mfrow=c(2,1), mai=c(0.7,0.6,0.5, 0.3), mgp = c(2, 0.7, 0))
	cols = c('black', 'blue', 'green', 'red')
	ylims = range(unlist(lapply(meanlat, FUN=range, na.rm=T)))*c(0.99,1.01)
	plot(mave(yrs,w), mave(meanlat[[1]],w), type='o', pch=16, xlab='Year', ylab = 'Mean latitude', ylim=ylims, main=paste(spp, 'moving average', w, 'yrs'), col=cols[1])
		abline(lm(mave(meanlat[[1]],w) ~ mave(yrs,w)), col=cols[1], lty=2)
		abline(h = mean(meanlat[[1]], na.rm=T), col='grey', lty=1)
	points(mave(yrs,w), mave(meanlat[[2]],w), type='o', pch=16, col=cols[2])
		abline(lm(mave(meanlat[[2]],w) ~ mave(yrs,w)), col=cols[2], lty=2)
	points(mave(yrs,w), mave(meanlat[[3]],w), type='o', pch=16, col=cols[3])
		abline(lm(mave(meanlat[[3]],w) ~ mave(yrs,w)), col=cols[3], lty=2)
	points(mave(yrs,w), mave(meanlat[[4]],w), type='o', pch=16, col=cols[4])
		abline(lm(mave(meanlat[[4]],w) ~ mave(yrs,w)), col=cols[4], lty=2)
	legend('topleft', legend=c('Observed', 'Poisson GLM', 'Zero-inflated poisson GLM', 'GAM'), col= cols, pch=16, lty=1, bty='n')
	
	ylims = range(unlist(lapply(meanlong, FUN=range, na.rm=T)))*c(1.01,0.99)
	plot(mave(yrs,w), mave(meanlong[[1]],w), type='o', pch=16, xlab='Year', ylab = 'Mean longitude', ylim=ylims, main=paste(spp, 'moving average', w, 'yrs'), col=cols[1])
		abline(lm(mave(meanlong[[1]],w) ~ mave(yrs,w)), col=cols[1], lty=2)
		abline(h = mean(meanlong[[1]], na.rm=T), col='grey', lty=1)
	points(mave(yrs,w), mave(meanlong[[2]],w), type='o', pch=16, col=cols[2])
		abline(lm(mave(meanlong[[2]],w) ~ mave(yrs,w)), col=cols[2], lty=2)
	points(mave(yrs,w), mave(meanlong[[3]],w), type='o', pch=16, col=cols[3])
		abline(lm(mave(meanlong[[3]],w) ~ mave(yrs,w)), col=cols[3], lty=2)
	points(mave(yrs,w), mave(meanlong[[4]],w), type='o', pch=16, col=cols[4])
		abline(lm(mave(meanlong[[4]],w) ~ mave(yrs,w)), col=cols[4], lty=2)

		dev.off()
	
	# Plot obs vs. predicted lat
	require(smatr)
	quartz(width=9, height=3)
	par(mfrow=c(1,3))
	x<-meanlat[[2]]; y<-meanlat[[1]]
		summary(m <- sma(y~x))
		plot(m, main='Poisson GLM', xlab='Predicted', ylab = 'Observed')
		abline(0,1, col='grey')
	x<-meanlat[[3]]; y<-meanlat[[1]]
		summary(m <- sma(y~x))
		plot(m, main='Zero-inflated poisson GLM', xlab='Predicted', ylab = 'Observed')
		abline(0,1, col='grey')
	x<-meanlat[[4]]; y<-meanlat[[1]]
		summary(m <- sma(y~x))
		plot(m, main='GAM', xlab='Predicted', ylab = 'Observed')
		abline(0,1, col='grey')
	
	# Plot obs vs. predicted lat after taking a moving average (just GAM)
	require(smatr)
	w = c(1,2,5) # window
	quartz(width=9, height=3)
	par(mfrow=c(1,3))
	x<-mave(meanlat[[4]],w[1]); y<-mave(meanlat[[1]],w[1])
		summary(m <- sma(y~x))
		plot(m, main='GAM no average', xlab='Predicted', ylab = 'Observed')
		abline(0,1, col='grey')
		text(x=42.2, y=41.4, labels = paste('p =',signif(m$pval[[1]],2)))
	x<-mave(meanlat[[4]],w[2]); y<-mave(meanlat[[1]],w[2])
		summary(m <- sma(y~x))
		plot(m, main='GAM 2-yr ave', xlab='Predicted', ylab = 'Observed')
		abline(0,1, col='grey')
		text(x=42.1, y=41.7, labels = paste('p =',signif(m$pval[[1]],2)))
	x<-mave(meanlat[[4]],w[3]); y<-mave(meanlat[[1]],w[3])
		summary(m <- sma(y~x))
		plot(m, main='GAM 5-yr average', xlab='Predicted', ylab = 'Observed')
		abline(0,1, col='grey')
		text(x=42, y=41.75, labels = paste('p =',signif(m$pval[[1]],2)))

	# Plot obs vs. predicted lat (after detrending)
	quartz(width=11, height=4)
	par(mfrow=c(1,3))
	plot(x<-resid(lm(meanlat[[2]] ~ yrs)), y<-resid(lm(meanlat[[1]]~yrs)), main='Poisson GLM')
		abline(mod<-lm(y~x), col='grey', lty=2)
		summary(mod)
	plot(x<-resid(lm(meanlat[[3]] ~ yrs)), y<-resid(lm(meanlat[[1]]~yrs)), main='Zero-inflated poisson GLM')
		abline(mod<-lm(y~x), col='grey', lty=2)
		summary(mod)
	plot(x<-resid(lm(meanlat[[4]] ~ yrs)), y<-resid(lm(meanlat[[1]]~yrs)), main='GAM')
		abline(mod<-lm(y~x), col='grey', lty=2)
		summary(mod)
		
	# Plot Cross-correlation of observed and predicted
	ccf(meanlat[[1]], meanlat[[2]])
	ccf(meanlat[[1]], meanlat[[3]])
	ccf(meanlat[[1]], meanlat[[4]])
	
	# Plot mean long over time
	quartz(width=7, height=6)
	# pdf(paste('Figures/Meanlong_CEM_', spp, '_', season, '_', Sys.Date(), '.pdf', sep=''), width=7, height=6)

	cols = c('black', 'blue', 'green', 'red')
	ylims = range(unlist(lapply(meanlong, FUN=range, na.rm=T)))*c(1.01,0.99)
	plot(yrs, meanlong[[1]], type='o', pch=16, xlab='Year', ylab = 'Mean longitude', ylim=ylims, main=spp, col=cols[1])
		abline(lm(meanlong[[1]] ~ yrs), col=cols[1], lty=2)
		abline(h = mean(meanlong[[1]], na.rm=T), col='grey', lty=1)
	points(yrs, meanlong[[2]], type='o', pch=16, col=cols[2])
		abline(lm(meanlong[[2]] ~ yrs), col=cols[2], lty=2)
	points(yrs, meanlong[[3]], type='o', pch=16, col=cols[3])
		abline(lm(meanlong[[3]] ~ yrs), col=cols[3], lty=2)
	points(yrs, meanlong[[4]], type='o', pch=16, col=cols[4])
		abline(lm(meanlong[[4]] ~ yrs), col=cols[4], lty=2)
	legend('topleft', legend=c('Observed', 'Poisson GLM', 'Zero-inflated poisson GLM', 'GAM'), col= cols, pch=16, lty=1, bty='n')
	
		dev.off()

	# Plot mean lat and long simultaneously
	quartz(width=6, height=6)
	
		# pdf(paste('Figures/Meanlatlong_CEM_', spp, '_', season, '_', Sys.Date(), '.pdf', sep=''), width=7, height=6)

	cols = c('black', 'blue', 'green', 'red')
	ylims = range(unlist(lapply(meanlat, FUN=range, na.rm=T)))*c(0.999,1.001)
	xlims = range(unlist(lapply(meanlong, FUN=range, na.rm=T)))*c(1.001,0.999)
	plot(meanlong[[1]], meanlat[[1]], type='o', pch=16, xlab='Mean longitude', ylab = 'Mean latitude', ylim=ylims, xlim=xlims, main=spp, col=cols[1])
	points(meanlong[[2]], meanlat[[2]], type='o', pch=16, col=cols[2])
	points(meanlong[[3]], meanlat[[3]], type='o', pch=16, col=cols[3])
	points(meanlong[[4]], meanlat[[4]], type='o', pch=16, col=cols[4])
	legend('topleft', legend=c('Observed', 'Poisson GLM', 'Zero-inflated poisson GLM', 'GAM'), col= cols, pch=16, lty=1, bty='n', cex=0.8)
	
		dev.off()
		
########################		
## Analysis of models ##
########################		


# Compare obs vs. predicted lat after taking a moving average
	require(smatr)
	w = 1:5
	out = data.frame(w = w, glmr = numeric(length(w)), glmp = numeric(length(w)), zipr = numeric(length(w)), zipp = numeric(length(w)), gamr = numeric(length(w)), gamp = numeric(length(w)))
	for(i in 1:5){ # window
		m <- sma(mave(meanlat[[1]],w[i]) ~ mave(meanlat[[2]],w[i]))
		out$glmr[i] = m$r2
		out$glmp[i] = m$pval

		m <- sma(mave(meanlat[[1]],w[i]) ~ mave(meanlat[[3]],w[i]))
		out$zipr[i] = m$r2
		out$zipp[i] = m$pval

		m <- sma(mave(meanlat[[1]],w[i]) ~ mave(meanlat[[4]],w[i]))
		out$gamr[i] = m$r2
		out$gamp[i] = m$pval
	}