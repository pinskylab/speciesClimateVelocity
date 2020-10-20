# Climate-envelope models

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Gulf of Alaska')

#################
### DATA PREP ###
#################

## Read in data
load('../HadISST 1.1/Output/hadisst_2011-12-01.RData') # load SST by month

data = read.csv('Output/datatrimwzeros_2012-03-11.csv', row.names=1)
	dim(data)
	
## Figure out which names we need to use and trim the data
# only keep spp that are caught > 10 times/yr
	tab = aggregate(data$WTCPUE>0, by=list(spp=data$SCIENTIFIC, year = data$YEAR), FUN=sum, na.rm=T)
	tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
	tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
	sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
	sppnames # These are spp to consider using: 42
	
	# Trim data to right spp
	datatrim = data[data$SCIENTIFIC %in% sppnames,]
	dim(datatrim)
			
## Add HadISST data (min and max temp)
	datatrim$mintemp = NA
	datatrim$mintempmnth = NA
	datatrim$maxtemp = NA
	datatrim$maxtempmnth = NA
	datatrim$month = NA

	# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
	datatrim$latgrid = floor(datatrim$LATITUDE)+0.5
	datatrim$longgrid = floor(datatrim$LONGITUDE)+0.5
	dates = unlist(strsplit(as.character(datatrim$DATETIME), split='/'))
	datatrim$month = as.numeric(dates[seq(1,length(dates),by=3)])
	inds = which(!duplicated(datatrim[,c('latgrid', 'longgrid', 'YEAR', 'month')]))
		length(inds)
	for(i in 1:length(inds)){
		if(i %% 50 == 0) print(i)
		lat = as.character(datatrim$latgrid[inds[i]]) # to match hadisst grid
		long = as.character(datatrim$longgrid[inds[i]]) # to match hadisst grid
		yr = as.character(datatrim$YEAR[inds[i]])
		lastyr = as.character(as.numeric(yr)-1)
		thesemons = as.character(1:datatrim$month[inds[i]]) # months in this year, converted to char
		lastmons = as.character((datatrim$month[inds[i]]+1):12) # months we want from last year

		j = datatrim$latgrid == datatrim$latgrid[inds[i]] & datatrim$longgrid == datatrim$longgrid[inds[i]] & datatrim$YEAR == datatrim$YEAR[inds[i]] & datatrim$month == datatrim$month[inds[i]]

		if(as.numeric(yr)<=2004){ # only have hadisst data through 2004
			# since above are char, we can use them as indices into hadisst
			temps = c(hadisst[lat,long,lastmons,lastyr], hadisst[lat,long,thesemons,yr]) # use the previous 12 months, including months in previous year
			if(all(is.na(temps[c('7', '8', '9')]))==TRUE){
				warning(paste('WARNING: No summer temps for i=', i))
			} else {
				datatrim$maxtemp[j] = max(temps, na.rm=T)
				datatrim$maxtempmnth[j] = as.numeric(names(temps)[which.max(temps)])
			}
			if(all(is.na(temps[c('1', '2', '3', '4')]))==TRUE){
				warning(paste('WARNING: No winter temps for i=', i))
			} else {
				datatrim$mintemp[j] = min(temps, na.rm=T)
				datatrim$mintempmnth[j] = as.numeric(names(temps)[which.min(temps)])
			}
		}
	}

# Add a date and a time column (rather than DATETIME)
datatrim$date = paste(formatC(datatrim$MONTH, width=2, flag=0), '/', formatC(datatrim$DAY, width=2, flag=0), '/', datatrim$YEAR, sep='')

datatrim$time = substr(as.character(datatrim$DATETIME), 11,15)

# Turn -9999 to NA
	datatrim$SURF_TEMP[datatrim$SURF_TEMP==-9999] = NA
	datatrim$BOT_TEMP[datatrim$BOT_TEMP==-9999] = NA

# Transform data
	datatrim$lsurftemp = log(datatrim$SURF_TEMP+1)
	datatrim$lbottemp = log(datatrim$BOT_TEMP+1)
	datatrim$lmintemp = log(datatrim$mintemp+1)	
	
# Add pres/abs
	datatrim$pres = datatrim$NUMCPUE>0 | (is.na(datatrim$NUMCPUE) & !is.na(datatrim$WTCPUE) & datatrim$WTCPUE>0)

## Add mean biomass as a predictor
bm = aggregate(list(biomassmean=datatrim$WTCPUE), by=list(YEAR=datatrim$YEAR, SCIENTIFIC=datatrim$SCIENTIFIC), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm)
	dim(datatrim)

bm = aggregate(list(nummean=datatrim$NUMCPUE), by=list(YEAR=datatrim$YEAR, SCIENTIFIC=datatrim$SCIENTIFIC), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm, all.x=T)
	dim(datatrim)

# Have a cpue that never goes to zero (useful for fitting log-links)
datatrim$wtcpuena = datatrim$WTCPUE
datatrim$wtcpuena[datatrim$wtcpuena == 0] = 1e-4
datatrim$wtcpuenal = log(datatrim$wtcpuena)

datatrim$numcpuena = datatrim$NUMCPUE
datatrim$numcpuena[datatrim$numcpuena == 0] = 1e-4
datatrim$numcpuenal = log(datatrim$numcpuena)

# Add a region column
datatrim$region = "AFSC_GOA"

# Add blank salinity columns
datatrim$surfsal = NA
datatrim$botsal = NA

# Rename columns
names(datatrim)[names(datatrim)=='VESSEL'] = 'svvessel'
names(datatrim)[names(datatrim)=='CRUISE'] = 'cruise'
names(datatrim)[names(datatrim)=='STATION'] = 'station'
names(datatrim)[names(datatrim)=='STRATUM'] = 'stratum'
names(datatrim)[names(datatrim)=='HAUL'] = 'tow'
names(datatrim)[names(datatrim)=='YEAR'] = 'year'
names(datatrim)[names(datatrim)=='DAY'] = 'day'
names(datatrim)[names(datatrim)=='LATITUDE'] = 'lat'
names(datatrim)[names(datatrim)=='LONGITUDE'] = 'lon'
names(datatrim)[names(datatrim)=='longgrid'] = 'longrid' # use the adjusted longitude
names(datatrim)[names(datatrim)=='BOT_DEPTH'] = 'depth'
names(datatrim)[names(datatrim)=='SURF_TEMP'] = 'surftemp'
names(datatrim)[names(datatrim)=='BOT_TEMP'] = 'bottemp'
names(datatrim)[names(datatrim)=='SCIENTIFIC'] = 'spp'
names(datatrim)[names(datatrim)=='WTCPUE'] = 'wtcpue'
names(datatrim)[names(datatrim)=='NUMCPUE'] = 'numcpue'


# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'tow', 'date', 'time', 'year', 'month', 'day', 'julian', 'lat', 'lon', 'latgrid', 'longrid', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'mintemp', 'maxtemp', 'mintempmnth', 'maxtempmnth', 'lsurftemp', 'lbottemp', 'lmintemp', 'spp', 'biomassmean', 'nummean', 'wtcpue', 'numcpue', 'wtcpuena', 'numcpuena', 'wtcpuenal', 'numcpuenal', 'pres')
	dim(datatrim)
	length(nm)
	
	setdiff(nm, names(datatrim))
	setdiff(names(datatrim), nm)
	 	 
datatrim = datatrim[,nm]
	dim(datatrim)


## Write out for this species
	write.csv(datatrim, file=paste('Output/dataCEM_', Sys.Date(), '.csv', sep=''))


##################
## Examine data ##
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Eastern Bering Shelf')

datatrim = read.csv('Output/dataCEM_2012-02-17.csv', row.names=1)

## When do we have data?
	table(datatrim$YEAR, !is.na(datatrim$SURF_TEMP))
	table(datatrim$YEAR, !is.na(datatrim$BOT_TEMP))
	table(datatrim$YEAR, !is.na(datatrim$mintemp))
	table(datatrim$YEAR, !is.na(datatrim$maxtemp))

# Examine data distributions
	hist(datatrim$NUMCPUE)
	table(datatrim$expcatnum)
	pairs(~ NUMCPUE + lsurftemp + lbottemp + mintemp + maxtemp, data=datatrim)
	
	hist(datatrim$WTCPUE)
	hist((datatrim$WTCPUE)^(1/7))

	hist(datatrim$SURF_TEMP)
		hist(log(datatrim$SURF_TEMP))
	hist(datatrim$bottemp)
		hist(log(datatrim$bottemp))
	hist(datatrim$mintemp)
		hist(log(datatrim$mintemp))
	hist(datatrim$maxtemp)
		hist(log(datatrim$maxtemp))

# Pairs
	pairs(~ lsurftemp + BOT_TEMP + mintemp + maxtemp, data=datatrim[runif(5000, 1, nrow(datatrim)),]) # pick 5000 random points

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
	
	# pdf(width=5, height=3, paste('Figures/Maps_', var, '_', Sys.Date(), '.pdf', sep=''))
	yrs = seq(1968,2012, by=5)
	for(j in 2:length(yrs)){
		#quartz(width=5,height=3)
		par(mai=c(0.5,0.5,0.2,0.5))
		i = datatrim$year >= yrs[j-1] & datatrim$year < yrs[j]
		out = make3Dgrid(datatrim$lon[i], datatrim$lat[i], z[i], xbreaks=25)
		filled.contour(out, levels=levs, col=rev(heat.colors(length(levs)-1)), main=yrs[j])
	}
	
	dev.off()
	
#############
### MODEL ###	
#############
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Eastern Bering Shelf')
source('../allsubset.glm 2012-01-30.R')
source('../revgrep 2012-02-07.R')

datatrim = read.csv('Output/dataCEM_2012-02-17.csv', row.names=1)
    datatrim$stratumfact = as.factor(datatrim$STRATUM)
    datatrim$yrfact = as.factor(datatrim$YEAR)


## Models

	# require(MASS)
	# require(pscl)
	require(mgcv)


## Pick a spp
spp=sort(unique(datatrim$SCIENTIFIC));spp # 24 spp

	# Plot cpue vs. environmental variables
	par(mfrow=c(1,4))
	i = datatrim$SCIENTIFIC == spp[2]
	i = datatrim$SCIENTIFIC == spp[2] & datatrim$pres
	i = datatrim$SCIENTIFIC == spp[2] & datatrim$presfit
	plot(log(wtcpuena) ~ SURF_TEMP, data=datatrim[i,])
	plot(log(wtcpuena) ~ BOT_TEMP, data=datatrim[i,])
	plot(log(wtcpuena) ~ stratumfact, data=datatrim[i,])
	plot(log(wtcpuena) ~ biomassmean, data=datatrim[i,])
	
	plot(datatrim$mintemp[i], datatrim$WTCPUE[i])
	plot(datatrim$maxtemp[i], datatrim$WTCPUE[i])

## Pick the training years
yrs = sort(unique(datatrim$YEAR)); yrs
#trainingyrs = yrs # all years
#trainingyrs = yrs[1:6] # first 6 of 25 years (82-92)
#trainingyrs = yrs[1:12] # first 12 of 25 years (82-98)
#trainingyrs = yrs[seq(1,length(yrs), by=2)]; trainingyrs # subset of 12 years
trainingyrs = yrs[1:12] # last 13 of 25 years (99-11)
#trainingyrs = yrs[13:length(yrs)] # last 13 of 25 years (99-11)

# Indicator for where present, but also has at least one TRUE value per spp per stratum within the training years, for fitting stratum effects
## OR: Skip this and read it in below
	strat = sort(unique(datatrim$STRATUM))
	datatrim$presfit = datatrim$WTCPUE > 0
	presfitspp = character(0)
	for(i in 1:length(spp)){
		for(j in 1:length(strat)){
			inds = datatrim$SCIENTIFIC == spp[i] & datatrim$STRATUM==strat[j] & datatrim$YEAR %in% trainingyrs & complete.cases(datatrim[,c('SURF_TEMP', 'BOT_TEMP')]) # also has to have complete data, otherwise useless for model fitting
			if(sum(datatrim$presfit[inds]) == 0){
				print(paste(spp[i],strat[j]))
				k = sample(which(inds),1) # pick a random record in this stratum
				datatrim$presfit[k] = TRUE # set it to TRUE (even though wtcpue==0)
				print(paste(datatrim$presfit[k], datatrim$WTCPUE[k], datatrim$wtcpuena[k], datatrim$YEAR[k]))
				presfitspp = c(presfitspp, as.character(spp[i]))
			}
		}
	}
	sum(datatrim$presfit & !datatrim$pres)
	presfitspp = unique(presfitspp); presfitspp

	write.csv(datatrim[,c('haulid', 'SCIENTIFIC', 'presfit')], file=paste('Output/CEModels/datatrimpresfit_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.csv', sep=''))

	# Read in presfit from before:
	#presfit = read.csv('Output/CEModels 2012-02-18 half biomassmean/datatrimpresfit_1982to2011_2012-02-18.csv', row.names=1)
		dim(datatrim)
	#datatrim = merge(datatrim, presfit)
		dim(datatrim) # should add one column
	

options(warn=0)
for(i in 1:length(spp)){
	print(i)

	# Subset to training years
	#inds = datatrim$YEAR %in% trainingyrs & complete.cases(datatrim[,c('SURF_TEMP', 'BOT_TEMP', 'mintemp', 'maxtemp')]) & datatrim$SCIENTIFIC == spp[i]
	#inds = datatrim$YEAR %in% trainingyrs & complete.cases(datatrim[,c('SURF_TEMP', 'BOT_TEMP')]) & datatrim$SCIENTIFIC == spp[i]
	inds = datatrim$YEAR %in% trainingyrs & complete.cases(datatrim[,c('BOT_TEMP')]) & datatrim$SCIENTIFIC == spp[i]
	
	print(paste(spp[i], ': ', sum(datatrim$pres[inds]), ' presences in training set', sep=''))
	
	if(sum(datatrim$pres[inds])>50){

		# GLM with gaussian error (don't account for zeros)
		print('GLM gaussian')
		#glm1 = glm(WTCPUE ~ SURF_TEMP + I(SURF_TEMP^2) + BOT_TEMP + I(BOT_TEMP^2) + mintemp + I(mintemp^2) + maxtemp + I(maxtemp^2) + julian + I(julian^2) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds,])
		#glm1 = glm(WTCPUE ~ SURF_TEMP + I(SURF_TEMP^2) + BOT_TEMP + I(BOT_TEMP^2) +  julian + I(julian^2) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds,])#	summary(glm1)
		#	hist(resid(glm1)) # far too many zeros, right skewed
		#glm2 = step(glm1)
		#	summary(glm2)
			
		# Hurdle glm gamma (two-part: pres/abs (glmhp) and then abund|pres (glmha))
		print('GLM presence poisson')
		#glmhp1 = glm(pres ~ SURF_TEMP + I(SURF_TEMP^2) + BOT_TEMP + I(BOT_TEMP^2) + mintemp + I(mintemp^2) + maxtemp + I(maxtemp^2), family='binomial', data=datatrim[inds,])
		#	summary(glmhp1)
		#	par(mfrow=c(2,2));plot(glmhp1)
		#glmhp2 = step(glmhp1, scope='pres ~ .')	
		#glmhp2 = allsubsets.glm(formula='pres ~ SURF_TEMP + I(SURF_TEMP^2) + BOT_TEMP + I(BOT_TEMP^2) + mintemp + I(mintemp^2) + maxtemp + I(maxtemp^2) + julian + I(julian^2) + stratumfact + biomassmean', family='binomial', data=datatrim[inds,])	
		#	summary(glmhp2)
	
		print('GLM abundance gamma')
		#glmha1 = glm(WTCPUE ~ SURF_TEMP + I(SURF_TEMP^2) + BOT_TEMP + I(BOT_TEMP^2) + mintemp + I(mintemp^2) + maxtemp + I(maxtemp^2), family=Gamma, data=datatrim[inds & datatrim$pres,], control=list(trace=TRUE, maxit=50), start=c(1,1,-1,1,-1,1,-1,1,1)*0.5)
		#	summary(glmha1)
		#	par(mfrow=c(2,2));plot(glmha1)
		#glmha2 = allsubsets.glm(formula='NUMCPUE ~ SURF_TEMP + I(SURF_TEMP^2) + BOT_TEMP + I(BOT_TEMP^2) + mintemp + I(mintemp^2) + maxtemp + I(maxtemp^2)', family=Gamma, data=datatrim[inds & datatrim$pres,])	# will try all subsets that converge
		#	summary(glmha2)
			
			# Plot predicted vs. observed
		#	y = predict(glmhp2, newdata=datatrim[inds,], type='response')*predict(glmha2, newdata=datatrim[inds,], type='response')
		#	plot(datatrim$NUMCPUE[inds], y)
		
		# GAM gaussian
		print('GAM gaussian')
		#gam1 = gam(WTCPUE ~ s(SURF_TEMP) + s(BOT_TEMP) + s(mintemp) + s(maxtemp), family=gaussian, data=datatrim[inds,])
		#	summary(gam1)
		#	par(mfrow=c(2,2)); plot(gam1, se=TRUE, shade=TRUE)
		#	plot(fitted(gam1), resid(gam1))
	
		#gam2 = gam(WTCPUE ~ s(SURF_TEMP) + s(BOT_TEMP) + s(mintemp) + s(maxtemp) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function
		# gam2 = gam(WTCPUE ~ s(SURF_TEMP) + s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function#	summary(gam2)
		gam2 = gam(WTCPUE ~ s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function#	summary(gam2)
		#	summary(gam2)
		#	plot(gam2, se=TRUE, shade=TRUE, pages=1, all.terms=TRUE)
	
		# GAM Hurdle gamma (pres/abs and abundance)
		print('GAM presence binomial')
		#gamhp1 = gam(pres ~ s(SURF_TEMP) + s(BOT_TEMP) + s(mintemp) + s(maxtemp), family=poisson, data=datatrim[inds,])
		#	summary(gamhp1)
		#	par(mfrow=c(2,2)); plot(gamhp1, se=TRUE, shade=TRUE)
		#	plot(fitted(gamhp1), resid(gamhp1))

		#gamhp2 = gam(pres ~ s(SURF_TEMP) + s(BOT_TEMP) + s(mintemp) + s(maxtemp) + s(julian), family=poisson, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function
		# gamhp2 = gam(pres ~ s(SURF_TEMP) + s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=binomial, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function#	summary(gamhp2)
		gamhp2 = gam(pres ~ s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=binomial, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function#	summary(gamhp2)
		#	plot(gamhp2, se=TRUE, shade=TRUE, pages=1, all.terms=TRUE)
	
		print('GAM abundance gaussian(log)')
		#gamha1 = gam(WTCPUE ~ s(SURF_TEMP) + s(BOT_TEMP) + s(mintemp) + s(maxtemp), family=Gamma, data=datatrim[inds & datatrim$pres,])
		#	summary(gamha1)
		#	par(mfrow=c(2,2)); plot(gamha1, se=TRUE, shade=TRUE)
		#	plot(fitted(gamha1), resid(gamha1))
		# gamha2 = gam(wtcpuenal ~ s(SURF_TEMP) + s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds & datatrim$presfit,], select=TRUE) # mgcv has a built-in model selection function
		gamha2 = gam(wtcpuenal ~ s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds & datatrim$presfit,], select=TRUE) # mgcv has a built-in model selection function
		#	summary(gamha2)
		#	hist(resid(gamha2))
		#	plot(gamha2, se=TRUE, shade=TRUE, pages=1, all.terms=TRUE)
		#	plot(fitted(gamha2), resid(gamha2))
		#	plot(fitted(gamhp2)[inds & datatrim$presfit], exp(fitted(gamha2)))
		# 	preds = exp(predict(gamha2, newdata = datatrim[inds,], type='response'))
		#	plot(fitted(gamhp2), preds)
		
		# Save models
		#mods = list(glm1=glm1, glm2=glm2, glmhp1=glmhp1, glmhp2=glmhp2, glmha1=glmha1, glmha2=glmha2, gam1=gam1, gam2=gam2, gamhp1=gamhp1, gamhp2=gamhp2, gamha1=gamha1, gamha2=gamha2)
		#mods = list(glm2=glm2, glmhp2=glmhp2, gam2=gam2, gamhp2=gamhp2, gamha2 = gamha2) # without full models or glm Gamma
		mods = list(gam2=gam2, gamhp2=gamhp2, gamha2 = gamha2) # without full models or glms
		save(mods, inds, file=paste('Output/CEModels/CEmods_', spp[i], '_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.RData', sep=''))
	}
}

##########################################
######## Plots and basic analysis ########
##########################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Eastern Bering Shelf')
require(mgcv)
datatrim = read.csv('Output/dataCEM_2012-02-17.csv', row.names=1)
    datatrim$stratumfact = as.factor(datatrim$STRATUM)
    datatrim$yrfact = as.factor(datatrim$YEAR)
	spp=sort(unique(datatrim$SCIENTIFIC));spp # 24 spp
	yrs = sort(unique(datatrim$YEAR)); yrs

	# Read in presfit from before (since it's unique to each set of training year sthat I use):
	#presfit = read.csv('Output/CEModels 2012-02-18 half biomassmean/datatrimpresfit_1982to1998_2012-02-18.csv', row.names=1)
	presfit = read.csv('Output/CEModels 2012-02-23 2ndhalf/datatrimpresfit_1999to2011_2012-02-23.csv', row.names=1)
		dim(datatrim)
		datatrim = datatrim[,-grep('presfit', names(datatrim))]
	datatrim = merge(datatrim, presfit)
		dim(datatrim) # should add one column
		names(datatrim)	


## Plot GAM fits for all the abundance models (multipage pdf)
	# folder = 'CEModels 2012-02-16 half biomassmean'; daterange = '_1982to1998_'
	# folder = 'CEModels 2012-02-17 halfinter biomassmean'; trainingyrs = yrs[seq(1,length(yrs), by=2)] # subset of 12 years
	#folder = 'CEModels 2012-02-18 half biomassmean'; trainingyrs = yrs[1:12]; vars=c('BOT_TEMP', 'SURF_TEMP') # subset of 12 years
	# folder = 'CEModels 2012-02-23 2ndhalf'; trainingyrs = yrs[13:length(yrs)]; date='2012-02-23'; vars=c('SURF_TEMP', 'BOT_TEMP') # subset of 12 years
	folder = 'CEModels 2012-02-24 half nosurf'; trainingyrs = yrs[1:12]; date='2012-02-24'; vars=c('BOT_TEMP') # subset of 12 years

	pdf(width=6, height=6, file=paste('Figures/GAMabund_fits_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.pdf', sep=''))
	for(i in 1:length(spp)){
		load(file=paste('Output/', folder, '/CEmods_', spp[i], '_', min(trainingyrs), 'to', max(trainingyrs), '_', date, '.RData', sep=''))	
		inds = datatrim$YEAR %in% trainingyrs & complete.cases(datatrim[,vars]) & datatrim$SCIENTIFIC == spp[i]
	
		plot(mods$gam2, shade=TRUE, se=TRUE, pages=1, main=spp[i], all.terms=TRUE)
		sum = summary(mods$gam2); print(sum)
		mtext(side=3, paste(signif(sum$dev.expl,2)*100, '% dev', sep=''))
	
	}
	
	dev.off()

## Plot GAM Hurdle fits for all the abundance models (multipage pdf)
	# folder = 'CEModels 2012-02-16 half biomassmean'; daterange = '_1982to1998_'
	#folder = 'CEModels 2012-02-17 halfinter biomassmean'; trainingyrs = yrs[seq(1,length(yrs), by=2)] # subset of 12 years
	#folder = 'CEModels 2012-02-18 half biomassmean'; date = '2012-02-18'; trainingyrs = yrs[1:12] # subset of 12 years
	# folder = 'CEModels 2012-02-23 2ndhalf'; trainingyrs = yrs[13:length(yrs)]; date='2012-02-23'; vars=c('SURF_TEMP', 'BOT_TEMP') # subset of 12 years
	#folder = 'CEModels 2012-02-23 2ndhalf nosurf'; trainingyrs = yrs[13:length(yrs)]; date='2012-02-23'; vars=c('BOT_TEMP') # subset of 12 years
	folder = 'CEModels 2012-02-24 half nosurf'; trainingyrs = yrs[1:12]; date='2012-02-24'; vars=c('BOT_TEMP') # subset of 12 years

	pdf(width=6, height=6, file=paste('Figures/GAMhurdle_fits_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.pdf', sep=''))
	for(i in 1:length(spp)){
		print(spp[i])
		load(file=paste('Output/', folder, '/CEmods_', spp[i], '_', min(trainingyrs), 'to', max(trainingyrs), '_', date, '.RData', sep=''))
		inds = datatrim$YEAR %in% trainingyrs & complete.cases(datatrim[,vars]) & datatrim$SCIENTIFIC == spp[i]
		
		plot(mods$gamhp2, shade=TRUE, se=TRUE, pages=1, main=paste(spp[i], '\npresence'), all.terms=TRUE)
		sum = summary(mods$gamhp2); print(sum)
		mtext(side=3, paste(signif(sum$dev.expl,2)*100, '% dev', sep=''))

		plot(mods$gamha2, shade=TRUE, se=TRUE, pages=1, main=paste(spp[i], '\nabundance'), all.terms=TRUE)
		sum = summary(mods$gamha2); print(sum)	
		mtext(side=3, paste(signif(sum$dev.expl,2)*100, '% dev', sep=''))
		
		rm(mods, inds)
	
	}
	
	dev.off()


# Plot predictions and reality as maps
	quartz(width=12, height=7)
	# pdf(paste('Figures/Maps_CEM_', spp, '_', Sys.Date(), '.pdf', sep=''), width=12, height=7)
	yrtoplot = c(1983, 1986, 1991, 1994, 1997, 2000, 2002, 2004)
	c = dataspp$NUMCPUE
	a = max(log(c), na.rm=T)
	cols = rev(rainbow(n=ceiling(a)+1, start=0, end=4/6))
	xlims = c(-195,-160)
	ylims = c(50,57)

	mods = list(glm=list(glm2), glmhurdle=list(glmhp2, glmha2), gam=list(gam2), gamhurdle=list(gamhp2, gamha2))
	par(mfcol=c(length(mods),length(yrtoplot)), mai=c(0.3, 0.3, 0.2, 0.1)) # use mfcol to fill by columns
	for(i in 1:length(yrtoplot)){
		for(j in 1:length(mods)){
			inds = dataspp$YEAR == yrtoplot[i] & complete.cases(dataspp[,c('lsurftemp', 'lbottemp', 'lmintemp', 'maxtemp')])
			c = dataspp$NUMCPUE[inds]
			plot(dataspp$long[inds], dataspp$LATITUDE[inds], pch=16, xlab='', cex=ceiling(log(c))/a+0.8, ylab='', xlim=xlims, ylim=ylims, col = 'black', main=paste(yrtoplot[i], names(mods)[j]))
			meanlat = weighted.mean(dataspp$LATITUDE[inds],w= dataspp$NUMCPUE[inds])
			points(min(xlims), meanlat, cex=2)
			if(i==1) baselat = meanlat
			abline(h=baselat, lty=2, col='grey')

			if(length(mods[[j]]) == 1){
				preds = predict(mods[[j]][[1]], newdata = dataspp[inds,], type='response')
			}
			if(length(mods[[j]]) == 2){
				preds = predict(mods[[j]][[1]], newdata = dataspp[inds,], type='response')*predict(mods[[j]][[2]], newdata = dataspp[inds,], type='response')			
			}
			preds[preds<0] = 0
			col = rgb(255,0,0,100,maxColorValue=255)
			points(dataspp$long[inds], dataspp$LATITUDE[inds], pch=16, xlab='', cex=ceiling(log(preds))/a+0.5, ylab='', col = col)
			points(min(xlims), weighted.mean(dataspp$lat[inds][!is.na(preds)], w = preds[!is.na(preds)]), cex=2, col=col)		
		}
	}
	
	dev.off()
	
	
## Calculate mean lat by year in observations and in models
	# folder = 'CEModels 2012-02-17 halfinter biomassmean'; trainingyrs = yrs[seq(1,length(yrs), by=2)] # subset of 12 years
	#folder = 'CEModels 2012-02-18 half biomassmean'; date = '2012-02-18'; trainingyrs = yrs[1:12] # subset of 12 years
	#folder = 'CEModels 2012-02-23 2ndhalf'; trainingyrs = yrs[13:length(yrs)]; date='2012-02-23' # subset of 12 years
	#folder = 'CEModels 2012-02-23 2ndhalf nosurf'; trainingyrs = yrs[13:length(yrs)]; date='2012-02-23' # subset of 12 years
	folder = 'CEModels 2012-02-24 half nosurf'; trainingyrs = yrs[1:12]; date='2012-02-24'; vars = c('BOT_TEMP') # subset of 12 years
	
	files=list.files(paste('Output/', folder, sep=''), pattern='.RData') # only works if only one round of CEM models are in the directory

	# save observed and predicted positions for lat and long
	meanpos = list(0)

	for(s in 1:length(files)){
		print(s)
		load(paste('Output/',folder, '/', files[s], sep=''))
		thisspp=strsplit(files[s], split=paste('CEmods_|_', min(trainingyrs), '|_', max(trainingyrs), sep=''))[[1]][2] # extract spp name from file name
	
		#thesemods = list(glm = list(mods$glm2), glmh = list(mods$glmhp2), gam = list(mods$gam2), gamh = list(mods$gamhp2, mods$gamha2))
		thesemods = list(gam = list(mods$gam2), gamh = list(mods$gamhp2, mods$gamha2))
		meanlat = list(rep(NA, length(yrs)))
		meanlong = list(rep(NA, length(yrs)))
		for(i in 1:(length(thesemods))) meanlat[[i+1]]=rep(NA,length(yrs))
		for(i in 1:(length(thesemods))) meanlong[[i+1]]=rep(NA,length(yrs))
		#names(meanlat) = c('obs', 'glm', 'glm pres', 'gam', 'gam hurdle')
		names(meanlat) = c('obs', 'gam', 'gamhurdle')
		names(meanlong) = names(meanlat)
	
		# Calculate mean lat and long by year for observed and for each model
		for(i in 1:length(yrs)){
			#inds = datatrim$YEAR == yrs[i] & complete.cases(datatrim[,c('SURF_TEMP', 'BOT_TEMP', 'mintemp', 'maxtemp', 'julian')])
			inds = datatrim$YEAR == yrs[i] & complete.cases(datatrim[,vars]) & datatrim$SCIENTIFIC == thisspp
			if(sum(inds)>0){
				meanlat[[1]][i] = weighted.mean(datatrim$LATITUDE[inds],w= datatrim$WTCPUE[inds], na.rm=T)
				meanlong[[1]][i] = weighted.mean(datatrim$long[inds],w= datatrim$WTCPUE[inds], na.rm=T)
		
				for(j in 1:length(thesemods)){
					if(length(thesemods[[j]]) == 1){
						preds = predict(thesemods[[j]][[1]], newdata = datatrim[inds,], type='response')
					}
					if(length(thesemods[[j]]) == 2){ # if a two-stage model (e.g., hurdle)
						preds1 = predict(thesemods[[j]][[1]], newdata = datatrim[inds,], type='response')
						preds2 = exp(predict(thesemods[[j]][[2]], newdata = datatrim[inds,], type='response')) # make sure to use exp if wtcpue was log-transformed!
						#plot(preds1, preds2, main=paste(spp[s], yrs[i]), xlab='Predicted presence', ylab='Predicted wtcpue')
						preds = preds1*preds2
					}
					preds[preds<0] = 0
					meanlat[[j+1]][i] = weighted.mean(datatrim$LATITUDE[inds][!is.na(preds)], w = preds[!is.na(preds)])		
					meanlong[[j+1]][i] = weighted.mean(datatrim$long[inds][!is.na(preds)], w = preds[!is.na(preds)])		
				}
			}
		}
		
		meanpos[[s]] = list(meanlat = meanlat, meanlong = meanlong, yrs = yrs)
		names(meanpos)[s] = thisspp
	}
	
	### Save meanpos and direction
	save(meanpos, file = paste('Output/meanpos_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.RData', sep=''))


# Plot predictions and reality as graphs of mean lat over time for all species
### Also calculate trends in obs and predicted
## If trainingyrs is not the full set of years, then calc trends separately for training and non-training years (second is for evaluation)
	# folder = 'CEModels 2012-02-17 half biomassmean'; load('Output/meanpos_1982to2011_2012-02-17.RData'); trainingyrs = c(1982, 1984, 1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011)
	#folder = 'CEModels 2012-02-18 half biomassmean'; load('Output/meanpos_1982to1998_2012-02-18.RData'); 	yrs = meanpos[[1]]$yrs; date = '2012-02-18'; trainingyrs = yrs[1:12] # subset of 12 years
	#folder = 'CEModels 2012-02-23 2ndhalf'; load('Output/meanpos_1999to2011_2012-02-23 wsurf.RData'); trainingyrs = yrs[13:length(yrs)]; date='2012-02-23' # subset of 13 years
	# folder = 'CEModels 2012-02-23 2ndhalf nosurf'; load('Output/meanpos_1999to2011_2012-02-23 nosurf.RData'); trainingyrs = yrs[13:length(yrs)]; date='2012-02-23' # subset of 13 years
	folder = 'CEModels 2012-02-24 half nosurf'; load('Output/meanpos_1982to1998_2012-02-24 nosurf.RData'); trainingyrs = yrs[1:12]; date='2012-02-24' # subset of 12 years

	spp=names(meanpos)
	len = length(spp)
	a = numeric(len)

	# save observed and predicted slopes for lat and long
	direction = data.frame(spp=character(len), obslat1 = a, glmlat1=a, glmhlat1=a, gamlat1=a, gamhlat1=a, obslong1=a, glmlong1=a, glmhlong1=a, gamlong1=a, gamhlong1=a, obslat2 = a, glmlat2=a, glmhlat2=a, gamlat2=a, gamhlat2=a, obslong2=a, glmlong2=a, glmhlong2=a, gamlong2=a, gamhlong2=a, stringsAsFactors=FALSE)

	ntrainingyrs = yrs[!(yrs %in% trainingyrs)]
	direction$spp = spp

	quartz(width=5, height=7)
	# pdf(paste('Figures/Meanlat&long_CEM_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.pdf', sep=''), width=5, height=7)
	par(mfrow=c(4,2), mai=c(0.4,0.5,0.2,0.1), mgp = c(2,1,0))	
	for(s in 1:length(files)){
		i = order(c(trainingyrs, ntrainingyrs))
		cols = c('black', 'blue', 'green', 'red', 'purple')
		#cols = c('black', 'red', 'purple') # without glms
		ylims = range(unlist(lapply(meanpos[[s]]$meanlat, FUN=range, na.rm=T)))*c(0.99,1.01)
		y1 = meanpos[[s]]$meanlat$obs[meanpos[[s]]$yrs %in% trainingyrs]
		y2 = meanpos[[s]]$meanlat$obs[!(meanpos[[s]]$yrs %in% trainingyrs)]
			plot(c(trainingyrs, ntrainingyrs)[i], c(y1,y2)[i], type='o', pch=16, xlab='Year', ylab = 'Mean latitude', ylim=ylims, main=spp[s], col=cols[1])
			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[1], lty=2)
			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[1], lty=2)
			direction$obslat1[s] = coef(t1)[2]
			direction$obslat2[s] = coef(t2)[2]
			abline(h = mean(c(y1,y2), na.rm=T), col='grey', lty=1)
#		y1 = meanpos[[s]]$meanlat$glm[meanpos[[s]]$yrs %in% trainingyrs]
#		y2 = meanpos[[s]]$meanlat$glm[!(meanpos[[s]]$yrs %in% trainingyrs)]
#			points(yrs, c(y1,y2), type='o', pch=16, col=cols[2])
#			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[2], lty=2)
#			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[2], lty=2)
#			direction$glmlat1[s] = coef(t1)[2]
#			direction$glmlat2[s] = coef(t2)[2]
#		y1 = meanpos[[s]]$meanlat$glmhurdle[meanpos[[s]]$yrs %in% trainingyrs]
#		y2 = meanpos[[s]]$meanlat$glmhurdle[!(meanpos[[s]]$yrs %in% trainingyrs)]
#			points(yrs, c(y1,y2), type='o', pch=16, col=cols[3])
#			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[3], lty=2)
#			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[3], lty=2)
#			direction$glmhlat1[s] = coef(t1)[2]
#			direction$glmhlat2[s] = coef(t2)[2]
		y1 = meanpos[[s]]$meanlat$gam[meanpos[[s]]$yrs %in% trainingyrs]
		y2 = meanpos[[s]]$meanlat$gam[!(meanpos[[s]]$yrs %in% trainingyrs)]
		points(c(trainingyrs, ntrainingyrs)[i], c(y1,y2)[i], type='o', pch=16, col=cols[4])
			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[4], lty=2)
			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[4], lty=2)
			direction$gamlat1[s] = coef(t1)[2]
			direction$gamlat2[s] = coef(t2)[2]
		y1 = meanpos[[s]]$meanlat$gamhurdle[meanpos[[s]]$yrs %in% trainingyrs]
		y2 = meanpos[[s]]$meanlat$gamhurdle[!(meanpos[[s]]$yrs %in% trainingyrs)]
			points(c(trainingyrs, ntrainingyrs)[i], c(y1,y2)[i], type='o', pch=16, col=cols[5])
			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[5], lty=2)
			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[5], lty=2)
			direction$gamhlat1[s] = coef(t1)[2]
			direction$gamhlat2[s] = coef(t2)[2]
		#legend('topleft', legend=c('Observed', 'Poisson GLM', 'Hurdle GLM', 'GAM', 'Hurdle GAM'), col= cols, pch=16, lty=1, bty='n')
		
		ylims = range(unlist(lapply(meanpos[[s]]$meanlong, FUN=range, na.rm=T)))*c(1.01,0.99)
		y1 = meanpos[[s]]$meanlong$obs[meanpos[[s]]$yrs %in% trainingyrs]
		y2 = meanpos[[s]]$meanlong$obs[!(meanpos[[s]]$yrs %in% trainingyrs)]
		plot(c(trainingyrs, ntrainingyrs)[i], c(y1,y2)[i], type='o', pch=16, xlab='Year', ylab = 'Mean longitude', ylim=ylims, main=spp[s], col=cols[1])
			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[1], lty=2)
			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[1], lty=2)
			direction$obslong1[s] = coef(t1)[2]
			direction$obslong2[s] = coef(t2)[2]
			abline(h = mean(c(y1,y2), na.rm=T), col='grey', lty=1)
#		y1 = meanpos[[s]]$meanlong$glm[meanpos[[s]]$yrs %in% trainingyrs]
#		y2 = meanpos[[s]]$meanlong$glm[!(meanpos[[s]]$yrs %in% trainingyrs)]
#		points(yrs, y<-meanpos[[s]]$meanlong$glm, type='o', pch=16, col=cols[2])
#			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[5], lty=2)
#			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[5], lty=2)
#			direction$glmlong1[s] = coef(t1)[2]
#			direction$glmlong2[s] = coef(t2)[2]
#		y1 = meanpos[[s]]$meanlong$glmhurdle[meanpos[[s]]$yrs %in% trainingyrs]
#		y2 = meanpos[[s]]$meanlong$glmhurdle[!(meanpos[[s]]$yrs %in% trainingyrs)]
#		points(yrs, y<-meanpos[[s]]$meanlong$glmhurdle, type='o', pch=16, col=cols[3])
#			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[5], lty=2)
#			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[5], lty=2)
#			direction$glmhlong1[s] = coef(t1)[2]
#			direction$glmhlong2[s] = coef(t2)[2]
		y1 = meanpos[[s]]$meanlong$gam[meanpos[[s]]$yrs %in% trainingyrs]
		y2 = meanpos[[s]]$meanlong$gam[!(meanpos[[s]]$yrs %in% trainingyrs)]
		points(c(trainingyrs, ntrainingyrs)[i], c(y1,y2)[i], type='o', pch=16, col=cols[4])
			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[4], lty=2)
			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[4], lty=2)
			direction$gamlong1[s] = coef(t1)[2]
			direction$gamlong2[s] = coef(t2)[2]
		y1 = meanpos[[s]]$meanlong$gamhurdle[meanpos[[s]]$yrs %in% trainingyrs]
		y2 = meanpos[[s]]$meanlong$gamhurdle[!(meanpos[[s]]$yrs %in% trainingyrs)]
		points(c(trainingyrs, ntrainingyrs)[i], c(y1,y2)[i], type='o', pch=16, col=cols[5])
			lines(trainingyrs, fitted(t1<-lm(y1 ~ trainingyrs)), col=cols[5], lty=2)
			lines(ntrainingyrs, fitted(t2<-lm(y2 ~ ntrainingyrs)), col=cols[5], lty=2)
			direction$gamhlong1[s] = coef(t1)[2]
			direction$gamhlong2[s] = coef(t2)[2]
	}
	
	plot(0,0, bty='n', col='white', xlab='', ylab='', xaxt='n', yaxt='n')
	#legend('center', legend=c('Observed', 'Poisson GLM', 'Hurdle GLM', 'GAM', 'Hurdle GAM'), col= cols, pch=16, lty=1, bty='n')
	legend('center', legend=c('Observed', 'GAM', 'Hurdle GAM'), col= cols[c(1,4,5)], pch=16, lty=1, bty='n')

	dev.off()
	
	### Save direction
	write.csv(direction, file=paste('Output/direction_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.csv', sep=''))

## Are any model trends correlated to observed trends in lat or long?
	
	# direction = read.csv('Output/direction_1982to2011_2012-02-17.csv', row.names=1); trainingyrs = c(1982, 1984, 1991, 1993, 1995, 1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011)
	#direction = read.csv('Output/direction_1982to2011_2012-02-18.csv', row.names=1); trainingyrs = c(1982, 1983, 1984, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998)
	#direction = read.csv('Output/direction_1999to2011_2012-02-23 wsurf.csv', row.names=1); trainingyrs = c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011)
	#direction = read.csv('Output/direction_1999to2011_2012-02-23 nosurf.csv', row.names=1); trainingyrs = c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011)
	direction = read.csv('Output/direction_1982to1998_2012-02-24 nosurf.csv', row.names=1); trainingyrs = c(1982, 1983, 1984, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998)
	
	yrs = meanpos[[1]]$yrs # need years from before
	ntrainingyrs = yrs[!(yrs %in% trainingyrs)]


	cor.test(direction$obslat1, direction$glmlat1)
	cor.test(direction$obslat2, direction$glmlat2)

	cor.test(direction$obslat1, direction$glmhlat1)
	cor.test(direction$obslat2, direction$glmhlat2)

	summary(lm(obslat1 ~ gamlat1, data=direction))
	summary(lm(obslat2 ~ gamlat2, data=direction))

	ca1 = summary(lm(obslat1 ~ gamhlat1, data=direction)); ca1
	ca2 = summary(lm(obslat2 ~ gamhlat2, data=direction)); ca2

	cor.test(direction$obslong1, direction$glmlong1)
	cor.test(direction$obslong2, direction$glmlong2)

	cor.test(direction$obslong1, direction$glmhlong1)
	cor.test(direction$obslong2, direction$glmhlong2)

	summary(lm(obslong1 ~ gamlong1, data=direction))
	summary(lm(obslong2 ~ gamlong2, data=direction))

	co1 = summary(lm(obslong1 ~ gamhlong1, data=direction)); co1
	co2 = summary(lm(obslong2 ~ gamhlong2, data=direction)); co2

	par(mfrow=c(1,2))
	plot(direction$gamhlat1, direction$obslat1, main=paste('AFSC Eastern Bering Shelf Training\n', paste(trainingyrs, collapse=', '), sep=''), xlab = 'GAM Hurdle Predicted (°lat/yr)', ylab = 'Observed (°lat/yr)', cex.main=0.7)
		abline(a=0,b=1)
		mod = sma(obslat1~gamhlat1, data=direction)
		abline(a = mod$coef[[1]][1,1], b = mod$coef[[1]][2,1], lty=2)
		legend('bottomright', bty='n', legend=c(paste('p=',signif(ca1$coef[2,4],2)), paste('r2=',signif(ca1$r.sq,2)), paste('b=',signif(ca1$coef[2,1],2))))
	plot(direction$gamhlat2, direction$obslat2, main=paste('AFSC Eastern Bering Shelf Test\n', paste(ntrainingyrs, collapse=', ')), xlab = 'GAM Hurdle Predicted (°lat/yr)', ylab = 'Observed (°lat/yr)', cex.main=0.7)
		abline(a=0,b=1)
		mod = sma(obslat2~gamhlat2, data=direction)
		abline(a = mod$coef[[1]][1,1], b = mod$coef[[1]][2,1], lty=2)
		legend('bottomright', bty='n', legend=c(paste('p=',signif(ca2$coef[2,4],2)), paste('r2=',signif(ca2$r.sq,2)), paste('b=',signif(ca2$coef[2,1],2))))

	lims = range(c(direction$obslat, direction$gamlat))
	par(mfrow=c(1,2))
	plot(direction$gamlat1, direction$obslat1, main='AFSC Eastern Bering Shelf', xlab = 'GAM Predicted (°lat/yr)', ylab = 'Observed (°lat/yr)')
		abline(a=0,b=1)
	plot(direction$gamlat2, direction$obslat2, main='AFSC Eastern Bering Shelf', xlab = 'GAM Predicted (°lat/yr)', ylab = 'Observed (°lat/yr)')
		abline(a=0,b=1)


	# Are any model/spp predictions correlated to observed at an annual scale?
	cor = direction
		cor = cor[,-grep('obs|glm|1', names(cor))]
	ntr = yrs %in% ntrainingyrs
	for(s in 1:length(spp)){
		cor$gamlat2[s] = cor.test(meanpos[[s]]$meanlat$obs[ntr], meanpos[[s]]$meanlat$gam[ntr])$p.value
		cor$gamhlat2[s] = cor.test(meanpos[[s]]$meanlat$obs[ntr], meanpos[[s]]$meanlat$gamhurdle[ntr])$p.value
		cor$gamlong2[s] = cor.test(meanpos[[s]]$meanlong$obs[ntr], meanpos[[s]]$meanlong$gam[ntr])$p.value
		cor$gamhlong2[s] = cor.test(meanpos[[s]]$meanlong$obs[ntr], meanpos[[s]]$meanlat$gamhurdle[ntr])$p.value
		if(any(cor[s,2:ncol(cor)]<0.05)){ 
			print(paste(spp[s], ': ', paste(names(cor)[cor[s,]<0.05], collapse='; '), sep=''))
		} else {
			print(spp[s])
		}
	}


## Plot predicted and observed mean lat during training years and during testing years (as opposed to trends during these years above)
	folder = 'CEModels 2012-02-18 half biomassmean'; load('Output/meanpos_1982to1998_2012-02-18.RData'); 	yrs = meanpos[[1]]$yrs; date = '2012-02-18'; trainingyrs = yrs[1:12] # subset of 12 years

	spp=names(meanpos)
	ntrainingyrs = yrs[!(yrs %in% trainingyrs)]
	len = length(spp)
	a = numeric(len)

	obs1 = a; obs2=a; gam1=a; gam2=a; gamh1=a; gamh2=a

	for(i in 1:length(spp)){
		obs1[i] = mean(meanpos[[i]]$meanlat$obs[meanpos[[i]]$yrs %in% trainingyrs])
		obs2[i] = mean(meanpos[[i]]$meanlat$obs[meanpos[[i]]$yrs %in% ntrainingyrs])
		gam1[i] = mean(meanpos[[i]]$meanlat$gam[meanpos[[i]]$yrs %in% trainingyrs])
		gam2[i] = mean(meanpos[[i]]$meanlat$gam[meanpos[[i]]$yrs %in% ntrainingyrs])
		gamh1[i] = mean(meanpos[[i]]$meanlat$gamhurdle[meanpos[[i]]$yrs %in% trainingyrs])
		gamh2[i] = mean(meanpos[[i]]$meanlat$gamhurdle[meanpos[[i]]$yrs %in% ntrainingyrs])
	}
	
	# Plot observed vs. predicted mean lat (not good test since obs1 and obs2 are closely correlated)
	par(mfrow=c(2,2), mai=c(0.8, 0.8, 0.4, 0.1))
	plot(obs1, gam1, xlab='Observed (°lat)', ylab='Predicted (°lat)', main=paste('GAM training\n(', paste(range(trainingyrs), collapse='-'), ')', sep=''))
		abline(a=0,b=1)
	plot(obs2, gam2, xlab='Observed (°lat)', ylab='Predicted (°lat)', main=paste('GAM testing\n(', paste(range(ntrainingyrs), collapse='-'), ')', sep=''))
		abline(a=0,b=1)
	plot(obs1, gamh1, xlab='Observed (°lat)', ylab='Predicted (°lat)', main=paste('GAM Two-stage training\n(', paste(range(trainingyrs), collapse='-'), ')', sep=''))
		abline(a=0,b=1)
	plot(obs2, gamh2, xlab='Observed (°lat)', ylab='Predicted (°lat)', main=paste('GAM Two-stage testing\n(', paste(range(ntrainingyrs), collapse='-'), ')', sep=''))
		abline(a=0,b=1)
		
	# Plot lat during testing yrs vs. lat during training yrs
	summary(lm(obs2 ~ obs1))
	plot(obs1, obs2)
		abline(0,1)

	# Plot observed vs. predicted change in mean lat
	par(mfrow=c(1,2), mai=c(0.8, 0.8, 0.4, 0.1))
	plot(obs1-obs2, gam1-gam2, xlab='Observed change (°lat)', ylab='Predicted change (°lat)', main=paste('GAM training\n(', paste(range(trainingyrs), collapse='-'), ')', sep=''))
		abline(a=0,b=1)
		summary(lm(I(obs1-obs2) ~ I(gam1-gam2)))
	plot(obs1-obs2, gamh1-gamh2, xlab='Observed change (°lat)', ylab='Predicted change (°lat)', main=paste('GAM Two-stage training\n(', paste(range(trainingyrs), collapse='-'), ')', sep=''))
		abline(a=0,b=1)
		summary(lm(I(obs1-obs2) ~ I(gamh1-gamh2)))
		



##############################################
## Correlate obs trends to predicted and LH ##
##############################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Eastern Bering Shelf')

#direction = read.csv('Output/direction_1982to2011_2012-02-17.csv', row.names=1)
direction = read.csv('Output/direction_1982to1998_2012-02-18.csv', row.names=1)
	dim(direction)
shiftlh = read.csv('Output/shiftlh_2012-02-18.csv', row.names=1)
lhnew = read.csv('../Life history/TrawlLH 1.csv')
dim(shiftlh)
	shiftlh$spptrunc = sub(" ", '', shiftlh$spp)
	
## Fix spp names and merge
direction$spptrunc = as.character(direction$spp)
direction$spptrunc[direction$spptrunc=='Clupeapallasi'] = 'Clupeapallasii'
direction$spptrunc[direction$spptrunc=='Leptagonusfrenatus'] = 'Sarritorfrenatus'
	
shiftlhpred = merge(shiftlh, direction[,-which(names(direction)=='spp')], by='spptrunc')
	dim(shiftlhpred)
	setdiff(direction$spptrunc, shiftlhpred$spptrunc)
shiftlhpred = merge(shiftlhpred, lhnew[,c('spp', 'CommFishingFB', 'F', 'Fmsy')], by.x='spp', by.y='spp', all.x=T)	

# Add some new vars
shiftlhpred$FFmsy = shiftlhpred$F/shiftlhpred$Fmsy
shiftlhpred$CommFishingFB2 = shiftlhpred$CommFishingFB
	levels(shiftlhpred$CommFishingFB2)[levels(shiftlhpred$CommFishingFB2) %in% c('Highly commercial', 'Commercial')] = 'Commercial'
	levels(shiftlhpred$CommFishingFB2)
	
# Write out
	write.csv(shiftlhpred, paste('Output/shiftlhpred_', Sys.Date(), '.csv', sep=''))
	
## Correlations
	shiftlhpred = read.csv('Output/shiftlhpred_2012-02-17.csv', row.names=1)
	shiftlhpred = read.csv('Output/shiftlhpred_2012-02-18.csv', row.names=1)
	
	i = complete.cases(shiftlhpred[,c('gamhlat2', 'MaxLengthTL', 'LongevityWild', 'K', 'Troph', 'DemersPelag')]); sum(i)
	mod = lm(obslat2 ~ gamhlat2 + MaxLengthTL + K + Troph + LongevityWild + DemersPelag, data=shiftlhpred[i,])
		summary(mod)
	mod2 = step(mod)
		summ<-summary(mod2); summ
		plot(fitted(mod2), residuals(mod))
		plot(x <- fitted(mod2), y <- shiftlhpred$obslat2[i], xlab='Predicted rate of shift (° lat/yr)', ylab = 'Observed rate of shift (° lat/yr)', main = paste('AFSC EBS, obslat ~ gamhlat + DemersPelag + MaxLength, n =', sum(i), 'spp'), cex.main=1)
			abline(0,1)
			legend('bottomright', bty='n', legend=c(paste('p=',signif(pf(summ$fstat[1], summ$fstat[2], summ$fstat[3], lower.tail=FALSE),2)), paste('r2=',signif(summ$r.sq,2)), paste('b=',signif(summ$coef[2,1],2))))


		# Simplified DemersPelag
		shiftlhpred$DP2 = shiftlhpred$DemersPelag
			levels(shiftlhpred$DP2)
			levels(shiftlhpred$DP2)[levels(shiftlhpred$DP2) %in% c('pelagic-neritic', 'pelagic-oceanic', 'pelagic')] = 'pelagic'
			levels(shiftlhpred$DP2)[levels(shiftlhpred$DP2) %in% c('bathydemersal', 'bathypelagic', 'benthopelagic', 'demersal')] = 'demersal'

		mod = lm(obslat2 ~ gamhlat2 + MaxLengthTL + K + Troph + LongevityWild+ DP2, data=shiftlhpred[i,])
			summary(mod)
		mod3 = step(mod)
			summ<-summary(mod3); summ
			anova(mod2, mod3)

		mod = lm(obslat2 ~ gamhlat2 + MaxLengthTL + K + Troph + LongevityWild + DP3, data=shiftlhpred[i,])
			summary(mod)
		mod4 = step(mod)
			summ<-summary(mod4); summ
			anova(mod4, mod3)

		plot(x <- fitted(mod4), y <- shiftlhpred$obslat2[i], xlab='Predicted rate of shift (° lat/yr)', ylab = 'Observed rate of shift (° lat/yr)', main = paste('AFSC EBS, obslat ~ gamhlat + DP3 + MaxLength, n =', sum(i), 'spp'), cex.main=1)
			abline(0,1)
			legend('bottomright', bty='n', legend=c(paste('p=',signif(pf(summ$fstat[1], summ$fstat[2], summ$fstat[3], lower.tail=FALSE),2)), paste('r2=',signif(summ$r.sq,2)), paste('b=',signif(summ$coef[2,1],2))))

	# Set which variables to look at
	vars = c('gamhlat2', 'MaxLengthTL', 'K', 'Troph')
	vars = c('gamhlat2', 'MaxLengthTL', 'K', 'Troph', 'F')
	vars = c('gamhlat2', 'K')
	vars = c('gamhlat2', 'F')
	vars = c('gamhlat2', 'FFmsy')
	vars = c('gamhlat2', 'CommFishingFB')
	vars = c('gamhlat2', 'CommFishingFB2')
	form = paste('obslat2 ~ ', paste(vars, collapse='+'))
	i = complete.cases(shiftlhpred[,vars]); sum(i)
	mod = lm(form, data=shiftlhpred[i,])
		summary(mod)
	mod2 = step(mod)
	# mod2 = lm(obslat ~ gamhlat + Troph, data=shiftlhpred[i,])
		summ<-summary(mod2); summ
		plot(fitted(mod2), residuals(mod))
		plot(x <- fitted(mod2), y <- shiftlhpred$obslat2[i], xlab='Predicted rate of shift (° lat/yr)', ylab = 'Observed rate of shift (° lat/yr)', main = paste('AFSC EBS, obslat ~ gamhlat + K + MaxLength, n =', sum(i), 'spp'))
			abline(0,1)
			legend('bottomright', bty='n', legend=c(paste('p=',signif(pf(summ$fstat[1], summ$fstat[2], summ$fstat[3], lower.tail=FALSE),2)), paste('r2=',signif(summ$r.sq,2)), paste('b=',signif(summ$coef[2,1],2))))



	library(leaps)
	library(car)
	leaps = regsubsets(obslat ~ gamhlat + MaxLengthTL + K + Troph + AnaCat, data=shiftlhpred, nbest = 1, method='exhaustive')
	
	plot(leaps, scale='bic')
	s = summary(leaps)
	subsets(leaps, statistic='rsq')
	subsets(leaps, statistic='bic')
	
	mod = lm(obslat ~ AnaCat + K + Troph, data=shiftlhpred)
		summary(mod)

