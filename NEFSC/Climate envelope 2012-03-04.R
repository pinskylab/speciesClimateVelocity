# Climate-envelope models

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')

#################
### DATA PREP ###
#################
	# Function to return all rows that are duplicates of other rows in a dataframe.
	allduplicated = function(x){
		i = which(duplicated(x))
		inds = rep(F, nrow(x))
		for(j in i){ # for each duplicated row
			temp = rep(T, nrow(x))
			for(k in 1:ncol(x)){ # find the other rows that match at every column
				temp = temp & (x[,k] == x[j,k])
			}
			inds = inds | temp
		}
		return(inds)
	}


# Choose a species and season
#allspp = c("Brosme brosme", 'Gadus morhua', 'Merluccius bilinearis', 'Conger oceanicus', 'Merluccius albidus', 'Homarus americanus', "Clupea harengus", 'Loligo pealleii')
allspp = c('Alosa pseudoharengus', 'Amblyraja radiata', 'Citharichthys arctifrons', 'Clupea harengus', 'Doryteuthis pealeii', 'Gadus morhua', 'Glyptocephalus cynoglossus', 'Hemitripterus americanus', 'Hippoglossina oblonga', 'Hippoglossoides platessoides', 'Homarus americanus', 'Leucoraja ocellata', 'Limanda ferruginea', 'Lophius americanus', 'Melanogrammus aeglefinus', 'Merluccius bilinearis', 'Myoxocephalus octodecemspinosus', 'Peprilus triacanthus', 'Placopecten magellanicus', 'Pollachius virens', 'Prionotus carolinus', 'Pseudopleuronectes americanus', 'Scomber scombrus', 'Scophthalmus aquosus', 'Sebastes fasciatus', 'Squalus acanthias', 'Urophycis chuss', 'Urophycis regius', 'Urophycis tenuis', 'Zoarces americanus')
allseason = c("Fall", 'Spring')

i = 29
spp = allspp[i]; spp
season = allseason[2]; season

## Read in data
dataspp = read.csv(paste("../Spp Data/", spp, ' (all) ', season, '.csv', sep=''))
	dim(dataspp)
	
## Figure out which names we need to use and trim the data
	sort(as.character(unique(dataspp$spp_name)))

	# Abundance by name
	tab1 = aggregate(dataspp$expcatnum, by=list(nm = dataspp$spp_name), FUN=sum, na.rm=T); tab1
	tab2 = aggregate(dataspp$cpue, by=list(nm = dataspp$spp_name), FUN=sum, na.rm=T); tab2

	sppnames = as.character(tab1$nm[tab1$x>0])
	sppnames # These are names to consider using (have expcatnum>0)
	
	# Table of when each name was used
	yr = as.numeric(substr(dataspp$startdateGMT,9,12))
	i = order(yr)
	table(yr, dataspp$spp_name[i])

	# Table of when each name was used and has abundance > 0
	yr = as.numeric(substr(dataspp$startdateGMT,9,12))
	i = order(yr)
	table(yr, dataspp$spp_name[i])

	# Maps of where each name was used
	par(mfrow=c(1,length(sppnames)))
	for(i in 1:length(sppnames)){
		plot(dataspp[dataspp$spp_name==sppnames[i], c('lon', 'lat')], main=sppnames[i])
	}
	
	# Names to use
	if(spp == "Alosa pseudoharengus") sppnames = " ALEWIFE"
	if(spp == 'Amblyraja radiata') sppnames = " AMBLYRAJA RADIATA (THORNY SKATE)"
	if(spp == "Brosme brosme") sppnames = " BROSME BROSME (CUSK)"
	if(spp == 'Citharichthys arctifrons') sppnames = c(" CITHARICHTHYS ARCTIFRONS (GULF STREAM FL)", " FLOUNDER GULF STREAM")
	if(spp == 'Clupea harengus') sppnames = " ATLANTIC HERRING"
	if(spp == "Conger oceanicus") sppnames = c(' CONGER EEL', " CONGER OCEANICUS (CONGER EEL)")
	if(spp == 'Doryteuthis pealeii') sppnames = " LOLIGO"
	if(spp == "Gadus morhua") sppnames = " ATLANTIC COD"
	if(spp == 'Glyptocephalus cynoglossus') sppnames = ' FLOUNDER WITCH'
	if(spp == 'Hemitripterus americanus') sppnames = " HEMITRIPTERUS AMERICANUS (SEA RAVEN)"
	if(spp == "Hippoglossina oblonga") sppnames = " FLOUNDER FOURSPOT"
	if(spp == "Hippoglossoides platessoides") sppnames = " AMERICAN PLAICE"
	if(spp == "Homarus americanus") sppnames = ' AMERICAM LOBSTER FEMALE'
	if(spp == "Leucoraja ocellata") sppnames = " LEUCORAJA OCELLATA (WINTER SKATE)"
	if(spp == 'Limanda ferruginea') sppnames = " FLOUNDER YELLOWTAIL"
	if(spp == 'Lophius americanus') sppnames = " GOOSEFISH"
	if(spp == 'Melanogrammus aeglefinus') sppnames = " HADDOCK"
	if(spp == "Merluccius bilinearis") sppnames = ' HAKE SILVER'
	if(spp == "Merluccius albidus") sppnames = ' HAKE OFFSHORE'
	if(spp == 'Myoxocephalus octodecemspinosus') sppnames = ' LONG HORN SCULPIN'
	if(spp == 'Peprilus triacanthus') sppnames = ' BUTTERFISH'
	if(spp == 'Placopecten magellanicus') sppnames = ' LIVE SEA SCALLOP'
	if(spp == 'Pollachius virens') sppnames = ' POLLACHIUS VIRENS (POLLOCK)'
	if(spp == 'Prionotus carolinus') sppnames = ' NORTHERN SEAROBIN'
	if(spp == 'Pseudopleuronectes americanus') sppnames = ' FLOUNDER WINTER'
	if(spp == 'Scomber scombrus') sppnames = " SCOMBER SCOMBRUS (ATLANTIC MACKEREL)"
	if(spp == "Scophthalmus aquosus") sppnames = " FLOUNDER WINDOWPANE"
	if(spp == 'Sebastes fasciatus') sppnames = " ACADIAN REDFISH"
	if(spp == 'Squalus acanthias') sppnames = " DOGFISH SPINY FEMALE"
	if(spp == 'Urophycis chuss') sppnames = " HAKE RED"
	if(spp == 'Urophycis regius') sppnames = " HAKE SPOTTED"
	if(spp == 'Urophycis tenuis') sppnames = ' HAKE WHITE'
	if(spp == 'Zoarces americanus') sppnames = ' OCEAN POUT'

	sppnames
	
	# Trim data to right spp
	datatrim = dataspp[dataspp$spp_name %in% sppnames,]
	dim(datatrim)
	
	# Trim to right strata
	i = (datatrim$stratum >=1010 & datatrim$stratum <=1300) | (datatrim$stratum >= 1360 & datatrim$stratum <= 1400) | (datatrim$stratum >=1610 & datatrim$stratum <=1760) # From Nye et al. 2009
		sum(i)
		nrow(datatrim)
	datatrim = datatrim[i,]
		nrow(datatrim)
	
	sum(datatrim$expcatnum)

	# Examine the range of sexes
	sort(unique(datatrim$catchsex))

	# Any duplicate tows?
	i = which(allduplicated(datatrim[,c('cruise6', 'stratum', 'station', 'tow')])) # indices of all duplicated entries where cpue > 0
		length(i)

		j = order(datatrim$cruise6[i], datatrim$station[i], datatrim$tow[i], datatrim$spp_name[i])
		datatrim[i,][j,]

	# Easy trim: remove duplicates where expcatnum == 0
	datatrim = datatrim[order(datatrim$cruise6, datatrim$stratum, datatrim$station, datatrim$tow, datatrim$expcatnum, decreasing = TRUE),]
	i = which(duplicated(datatrim[,c('cruise6', 'stratum', 'station', 'tow')]) & datatrim$expcatnum == 0) # indices of all duplicated entries where expcatnum==0
		length(i)	
		i
	datatrim[i,]

	datatrim = datatrim[-i,]
		nrow(datatrim)

	# Examine duplicates that cause problems: expcatnum>0
	i = which(allduplicated(datatrim[datatrim$expcatnum>0,c('cruise6', 'stratum', 'station', 'tow')])) # indices of all duplicated entries where cpue > 0
		length(i)
		i
		nrow(datatrim)
		j = order(datatrim$cruise6[datatrim$expcatnum>0][i], datatrim$station[datatrim$expcatnum>0][i], datatrim$tow[datatrim$expcatnum>0][i], datatrim$spp_name[datatrim$expcatnum>0][i])

		datatrim[datatrim$expcatnum>0,][i,][j,]
	
	# Combine sexes or alternate names at same station and year		
	i = which(duplicated(datatrim[,c('cruise6', 'stratum', 'station', 'tow')])) # indices of all duplicated entries
		length(i)
	if(length(i)>0){
		for(j in 1:length(i)){
			inds = which(datatrim$cruise6 == datatrim$cruise6[i[j]] & datatrim$stratum == datatrim$stratum[i[j]] & datatrim$station == datatrim$station[i[j]] & datatrim$tow == datatrim$tow[i[j]])
			if(length(inds)>1){
				print(datatrim[inds, c('cruise6', 'crunum', 'stratum', 'station', 'tow', 'spp_name', 'catchsex', 'expcatnum')][1:min(10,length(inds)),])
				datatrim$expcatnum[inds[1]] = sum(datatrim$expcatnum[inds], na.rm=T) # put the sum in the first entry
				datatrim$cpue[inds[1]] = sum(datatrim$cpue[inds], na.rm=T) # put the sum in the first entry
			}
		}
		datatrim = datatrim[-i,] # remove all the duplicates
	}
	nrow(datatrim)
	sum(datatrim$expcatnum) # should be the same as before

	# Add scientific name
	datatrim$spp = spp

	# Write out the cleaned table
	write.csv(datatrim, file=paste('../Spp datatrim/datatrim_', spp, '_', Sys.Date(), '.csv', sep=''))


#####################################
# Concatenate all the files together
#####################################
files = list.files('../Spp datatrim/')

out = read.csv(paste('../Spp datatrim/', files[1], sep=''), row.names=1, stringsAsFactors=FALSE)
	j = grep('endlat|endlon', names(out))
	if(length(j)>0) out = out[,-j] # remove cols that are only in some files
	dim(out)
	base = out # the base set of tows that all spp should have
		basekey = paste(base$cruise6, base$stratum, base$station, base$tow)
		length(unique(basekey))

for(i in 2:length(files)){
	temp = read.csv(paste('../Spp datatrim/', files[i], sep=''), row.names=1, stringsAsFactors=FALSE)
	
	# Drop cols
	j = grep('endlat|endlon', names(temp))
	if(length(j)>0)	temp = temp[,-j]

	# Make sure we have depth
	if(length(grep('maxdepth', names(temp)))==0){
		temp = merge(temp, base[,c('cruise6', 'station', 'maxdepth')])
	}
	print(paste(paste(dim(temp), collapse = ' '), temp$spp[1]))
	
	# If file doesn't have enough rows, add zero abundance for the missing tows
	if(nrow(temp)<nrow(base)){
		key = paste(temp$cruise6, temp$stratum, temp$station, temp$tow)
		diff = setdiff(basekey, key)
		newinds = which(basekey %in% diff)
		add = base[newinds,]
		add$svspp = unique(temp$svspp)[1]
		add$catchsex = NA
		add$expcatnum = 0
		add$cpue = 0
		add$spp_name = unique(temp$spp_name)[1]
		add$spp = unique(temp$spp)[1]
		temp = rbind(temp, add)
		print(paste('new: ', paste(dim(temp), collapse=' ')))
	}
	
	out = rbind(out, temp)
}

dim(out)
nrow(base)*30 # should match

	
## Add HadISST data (min and max temp)
load('../../HadISST 1.1/Output/hadisst_2011-12-01.RData') # load SST by month

	out$mintemp = NA
	out$mintempmnth = NA
	out$maxtemp = NA
	out$maxtempmnth = NA
	out$year = NA
	out$month = NA
	out$day = NA
	
	switchdates = function(x){
		return(switch(x, JAN=1, FEB=2, MAR=3, APR=4, MAY=5, JUN=6, JUL=7, AUG=8, SEP=9, OCT=10, NOV=11, DEC=12, NA))
	}

	# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
	out$latgrid = floor(out$lat)+0.5
	out$longgrid = floor(out$lon)+0.5
	dates = unlist(strsplit(as.character(out$startdateGMT), split='-'))
	out$month = sapply(dates[seq(2,length(dates),by=3)], switchdates)
	out$year = as.numeric(dates[seq(3,length(dates),by=3)])
	out$day = as.numeric(dates[seq(1,length(dates),by=3)])
	inds = which(!duplicated(out[,c('latgrid', 'longgrid', 'year', 'month')]))
		length(inds)
	for(i in 1:length(inds)){
		if(i %% 50 == 0) print(i)
		lat = as.character(out$latgrid[inds[i]]) # to match hadisst grid
		long = as.character(out$longgrid[inds[i]]) # to match hadisst grid
		yr = as.character(out$year[inds[i]])
		lastyr = as.character(as.numeric(yr)-1)
		thesemons = as.character(1:out$month[inds[i]]) # months in this year, converted to char
		lastmons = as.character((out$month[inds[i]]+1):12) # months we want from last year

		j = out$latgrid == out$latgrid[inds[i]] & out$longgrid == out$longgrid[inds[i]] & out$year == out$year[inds[i]] & out$month == out$month[inds[i]]

		if(as.numeric(yr)<=2004){ # only have hadisst data through 2004
			# since above are char, we can use them as indices into hadisst
			temps = c(hadisst[lat,long,lastmons,lastyr], hadisst[lat,long,thesemons,yr]) # use the previous 12 months, including months in previous year
			if(all(is.na(temps[c('7', '8', '9')]))==TRUE){
				warning(paste('WARNING: No summer temps for i=', i))
			} else {
				out$maxtemp[j] = max(temps, na.rm=T)
				out$maxtempmnth[j] = as.numeric(names(temps)[which.max(temps)])
			}
			if(all(is.na(temps[c('1', '2', '3', '4')]))==TRUE){
				warning(paste('WARNING: No winter temps for i=', i))
			} else {
				out$mintemp[j] = min(temps, na.rm=T)
				out$mintempmnth[j] = as.numeric(names(temps)[which.min(temps)])
			}
		}
	}


## Parse year and month	(if not done above with hadisst)
#	datatrim$year = NA
#	datatrim$month = NA
#	for(i in 1:nrow(datatrim)){
#		date = strsplit(as.character(datatrim$startdateGMT[i]), split='-')[[1]]
#		yr = date[3]
#		mon = switch(date[2], JAN=1, FEB=2, MAR=3, APR=4, MAY=5, JUN=6, JUL=7, AUG=8, SEP=9, OCT=10, NOV=11, DEC=12, NA)
#		if(is.na(mon)) stop(paste("WARNING: Couldn't match month on i=", i))
#		datatrim$year[i] = as.numeric(yr)
#		datatrim$month[i] = mon
#	}

# Add julian day
	require(date)
	out$julian = as.numeric(as.date(gsub('-', '', out$startdateGMT)))-as.numeric(as.date(paste('01/01/', out$year, sep=''))) # julian day since Jan 1

# Add date in AFSC format
out$date = paste(formatC(out$month, width=2, flag=0), '/', formatC(out$day,width=2, flag=0), '/', out$year, sep='')

# Transform data
	out$lsurftemp = log(out$surftemp+1)
	out$lbottemp = log(out$bottemp+1)
	out$lmintemp = log(out$mintemp+1)	

## Add mean biomass as a predictor
bm = aggregate(list(biomassmean=out$cpue), by=list(year=out$year, spp=out$spp), FUN=mean, na.rm=T)
	dim(bm)
	out = merge(out, bm)
	dim(out)

ab = aggregate(list(nummean=out$expcatnum), by=list(year=out$year, spp=out$spp), FUN=mean, na.rm=T)
	dim(bm)
	out = merge(out, ab)
	dim(out)


# Have a cpue and an abundance that never go to zero (useful for fitting log-links)
out$wtcpuena = out$cpue
out$wtcpuena[out$wtcpuena == 0] = 1e-4
out$wtcpuenal = log(out$wtcpuena)

out$numcpuena = out$expcatnum
out$numcpuena[out$numcpuena == 0] = 1e-4
out$numcpuenal = log(out$numcpuena)
	dim(out)

# Add a region
out$region = paste('NEFSC', season, sep='')

# Add a present/absent column
out$pres = out$expcatnum > 0 | out$cpue > 0 

# Sort
i = order(out$spp, out$year, out$cruise6, out$stratum, out$station, out$tow)
out = out[i,]

# Add a haulid
out$haulid = paste(out$svvessel, '-', out$cruise6, '-', formatC(out$station, width=4, flag=0), sep='')
	head(out$haulid)

# Drop cols we don't need
drp = grep('purposecode|statuscode|season|numna|numnal|catchsex|spp_name|endlat|endlon|startdateGMT|crunum|svspp', names(out))
out = out[,-drp]
names(out)
	dim(out)

# Change col names
names(out)[names(out)=='cruise6'] = 'cruise'
names(out)[names(out)=='timeGMT'] = 'time'
names(out)[names(out)=='expcatnum'] = 'numcpue'
names(out)[names(out)=='cpue'] = 'wtcpue'
names(out)[names(out)=='maxdepth'] = 'depth'
names(out)[names(out)=='longgrid'] = 'longrid'

names(out)

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'tow', 'date', 'time', 'year', 'month', 'day', 'julian', 'lat', 'lon', 'latgrid', 'longrid', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'mintemp', 'maxtemp', 'mintempmnth', 'maxtempmnth', 'lsurftemp', 'lbottemp', 'lmintemp', 'spp', 'biomassmean', 'nummean', 'wtcpue', 'numcpue', 'wtcpuena', 'numcpuena', 'wtcpuenal', 'numcpuenal', 'pres')
	dim(out)
	length(nm)
out = out[,nm]
	dim(out)

## Write out for this species
	write.csv(out, file=paste('Output/dataCEM_', season, '_', Sys.Date(), '.csv', sep=''))


##################
## Examine data ##
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')

datatrim = read.csv('Output/dataCEM_Spring_2012-03-04.csv', row.names=1); season='Spring'
    datatrim$stratumfact = as.factor(datatrim$stratum)
    datatrim$yrfact = as.factor(datatrim$year)

## When do we have data?
	table(datatrim$year, !is.na(datatrim$surftemp))
	table(datatrim$year, !is.na(datatrim$bottemp))
	table(datatrim$year, !is.na(datatrim$mintemp))
	table(datatrim$year, !is.na(datatrim$maxtemp))
	table(datatrim$year, !is.na(datatrim$botsal))

	i = !is.na(datatrim$surftemp)
	table(datatrim$stratum[i], datatrim$year[i])
	i = !is.na(datatrim$bottemp)
	table(datatrim$stratum[i], datatrim$year[i])
	i = !is.na(datatrim$mintemp)
	table(datatrim$stratum[i], datatrim$year[i])
	i = !is.na(datatrim$maxtemp)
	table(datatrim$stratum[i], datatrim$year[i])
	i = !is.na(datatrim$botsal)
	table(datatrim$stratum[i], datatrim$year[i])


# Examine data distributions
	hist(datatrim$expcatnum)
	table(datatrim$expcatnum)
	pairs(~ expcatnum + surftemp + bottemp + mintemp + maxtemp, data=datatrim)
	
	hist(datatrim$surftemp)
		hist(log(datatrim$surftemp))
	hist(datatrim$bottemp)
		hist(log(datatrim$bottemp))
	hist(datatrim$mintemp)
		hist(log(datatrim$mintemp))
	hist(datatrim$maxtemp)
		hist(log(datatrim$maxtemp))

	pairs(~ expcatnum + lsurftemp + lbottemp + lmintemp + maxtemp+lat+lon, data=datatrim)

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

# Calculate mean temperature by year
	yr = sort(unique(datatrim$year))
	btemp = numeric(length(yr))
	stemp = numeric(length(yr))

	inds = !duplicated(datatrim$haulid)
	sum(inds)
	for(i in 1:length(yr)){
		b = datatrim$bottemp[inds & datatrim$year == yr[i]]
		s = datatrim$surftemp[inds & datatrim$year == yr[i]]
		btemp[i] = mean(b, na.rm=T)
		stemp[i] = mean(s, na.rm=T)
	}
	
# Mean temp after Mueter & Litzow (also need spatially autocorrelated error)
	# only btemp in this OBIS data
	require(mgcv)
		#Spring
	inds = !duplicated(datatrim$haulid)
	btempgam = gam(bottemp ~ s(lon, lat) + s(julian) + as.factor(year), data=datatrim, subset=inds)
		summary(btempgam)
		plot(btempgam, pages=1, se=TRUE, all.terms=TRUE)
	c = coef(btempgam)
	i = grep('year', names(c))
	btemp2 = c(as.numeric(c)[1], as.numeric(c)[i] + as.numeric(c)[1]) # first is the intercept, and it's also 1968
	btemp2 = c(btemp2, rep(NA,2)) # pad final 2 years of NAs where we're missing data


	stempgam = gam(surftemp ~ s(lon,lat) + s(julian) + as.factor(year), data=datatrim, subset=inds)
		summary(stempgam)
		plot(stempgam, pages=1, se=TRUE, all.terms=TRUE)
	c = coef(stempgam)
	i = grep('year', names(c))
	stemp2 = c(as.numeric(c)[1], as.numeric(c)[i] + as.numeric(c)[1]) # add in intercept (it's also 1968)
	stemp2 = c(stemp2, rep(NA,2)) # pad final 2 years of NAs where we're missing data


	plot(yrspr, btemp2spr, col='red', ylim=c(9,11), type='l')
	lines(yrspr, btempspr, col='pink')
	lines(yrfal, btemp2fal, col='blue')
	lines(yrfal, btempfal, col='light blue')


# Plot Mean change in temperature?
	source('../../gls.ar.aic 2012-01-18.R')
	# btemp
	bmod = lm(btemp2 ~ yr)	
	bmodg = gls.ar.aic(btemp2 ~ yr, abs=0.2, quiet=TRUE, maxiter=30)

	summary(bmod)
	coef(bmod)[2]*(max(yr)-min(yr))
	summary(bmodg$mod)

	# SST
	smod = lm(stemp2 ~ yr)	
	smodg = gls.ar.aic(stemp2 ~ yr, abs=0.2, quiet=TRUE, maxiter=30)

	summary(smod)
	coef(smod)[2]*(max(yr)-min(yr))
	summary(smodg$mod)

	# Plot of mean temp, and after julian correction
	quartz(width=6, height=5)
	# pdf(paste('Figures/Temp trends IOOS_', season, '_', Sys.Date(), '.pdf', sep=''), width=6, height=5)
	plot(yr, btemp2, ylim=c(2,10), type='o', ylab='Temperature (°C)', xlab='Year', main='Northeast US', col='blue', lwd=1)
	points(yr, btemp, col='blue', type='o', lty=2)
	points(yr, stemp2, col='red', type='o', lwd=1)
	points(yr, stemp, col='red', type='o', lty=2)
	legend('bottomleft', legend=c('Bottom corrected (spring)', 'Bottom raw (spring)', 'Surface corrected (spring)', 'Surface raw (spring)'), col=c('blue', 'blue', 'red', 'red'), lty=c(1,2,1,2), pch=1, bty='n', cex=0.8, lwd=c(2,1,2,1))
	abline(bmod, lty=1, col='blue')
	abline(smod, lty=1, col='red')	
	
	dev.off()


	
#############
### MODEL ###	
#############
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')
source('../../allsubset.glm 2012-01-30.R')
source('../../revgrep 2012-02-07.R')

datatrim = read.csv('Output/dataCEM_Spring_2012-03-04.csv', row.names=1); season='Spring'
    datatrim$stratumfact = as.factor(datatrim$stratum)
    datatrim$yrfact = as.factor(datatrim$year)
     
## Models
	#require(MASS)
	#require(pscl)
	require(mgcv)

## Pick a spp
spp=sort(unique(datatrim$spp));spp # 24 spp
spp = 'Homarus americanus'
spp = 'Gadus morhua'

	# Plot cpue vs. environmental variables
	par(mfrow=c(1,4))
	i = datatrim$spp == spp[2]
	i = datatrim$spp == spp[2] & datatrim$pres
	i = datatrim$spp == spp[2] & datatrim$presfit
	plot(log(wtcpuena) ~ surftemp, data=datatrim[i,])
	plot(log(wtcpuena) ~ bottemp, data=datatrim[i,])
	plot(log(wtcpuena) ~ stratumfact, data=datatrim[i,])
	plot(log(wtcpuena) ~ biomassmean, data=datatrim[i,])
	
	plot(datatrim$mintemp[i], datatrim$wtcpue[i])
	plot(datatrim$maxtemp[i], datatrim$wtcpue[i])

## Pick the training years
yrs = sort(unique(datatrim$year)); yrs
#trainingyrs = yrs # all years
#trainingyrs = yrs[1:6] # first 6 of 44 years 
#trainingyrs = yrs[1:12] # first 12 of 44 years
#trainingyrs = yrs[seq(1,length(yrs), by=2)]; trainingyrs # subset 
trainingyrs = yrs[1:22] # first half
#trainingyrs = yrs[23:length(yrs)] # last half

# Indicator for where present, but also has at least one TRUE value per spp per stratum within the training years, for fitting stratum effects
## OR: Skip this and read it in below
	strat = sort(unique(datatrim$stratum))
	datatrim$presfit = datatrim$wtcpue > 0
	presfitspp = character(0)
	for(i in 1:length(spp)){
		for(j in 1:length(strat)){
			inds = datatrim$spp == spp[i] & datatrim$stratum==strat[j] & datatrim$year %in% trainingyrs & complete.cases(datatrim[,c('surftemp', 'bottemp')]) # also has to have complete data, otherwise useless for model fitting
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

	write.csv(datatrim[,c('haulid', 'spp', 'presfit')], file=paste('Output/CEModels/datatrimpresfit_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.csv', sep=''))

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
	inds = datatrim$year %in% trainingyrs & complete.cases(datatrim[,c('surftemp', 'bottemp')]) & datatrim$spp == spp[i]
	
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
		gam2 = gam(wtcpue ~ s(surftemp) + s(bottemp) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function#	summary(gam2)
		#gam2 = gam(WTCPUE ~ s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function#	summary(gam2)
		#	summary(gam2)
		#	plot(gam2, se=TRUE, shade=TRUE, pages=1, all.terms=TRUE)
	
		# GAM Hurdle gamma (pres/abs and abundance)
		print('GAM presence binomial')
		#gamhp1 = gam(pres ~ s(SURF_TEMP) + s(BOT_TEMP) + s(mintemp) + s(maxtemp), family=poisson, data=datatrim[inds,])
		#	summary(gamhp1)
		#	par(mfrow=c(2,2)); plot(gamhp1, se=TRUE, shade=TRUE)
		#	plot(fitted(gamhp1), resid(gamhp1))

		#gamhp2 = gam(pres ~ s(SURF_TEMP) + s(BOT_TEMP) + s(mintemp) + s(maxtemp) + s(julian), family=poisson, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function
		gamhp2 = gam(pres ~ s(surftemp) + s(bottemp) + s(julian) + stratumfact + biomassmean, family=binomial, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function#	summary(gamhp2)
		#gamhp2 = gam(pres ~ s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=binomial, data=datatrim[inds,], select=TRUE) # mgcv has a built-in model selection function#	summary(gamhp2)
		#	plot(gamhp2, se=TRUE, shade=TRUE, pages=1, all.terms=TRUE)
	
		print('GAM abundance gaussian(log)')
		#gamha1 = gam(WTCPUE ~ s(SURF_TEMP) + s(BOT_TEMP) + s(mintemp) + s(maxtemp), family=Gamma, data=datatrim[inds & datatrim$pres,])
		#	summary(gamha1)
		#	par(mfrow=c(2,2)); plot(gamha1, se=TRUE, shade=TRUE)
		#	plot(fitted(gamha1), resid(gamha1))
		gamha2 = gam(wtcpuenal ~ s(surftemp) + s(bottemp) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds & datatrim$presfit,], select=TRUE) # mgcv has a built-in model selection function
		#gamha2 = gam(wtcpuenal ~ s(BOT_TEMP) + s(julian) + stratumfact + biomassmean, family=gaussian, data=datatrim[inds & datatrim$presfit,], select=TRUE) # mgcv has a built-in model selection function
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
		save(mods, inds, file=paste('Output/CEModels/CEmods_', spp[i], '_', season, '_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.RData', sep=''))
	}
}
#################
## Plot models ##
#################

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')
#require(MASS)
#require(pscl)
require(mgcv)
source('../../mave 2012-01-24.R')

load('Output/CEModels_Homarus americanus_Spring_2012-03-03.RData'); spp='Homarus americanus'; season='Spring'
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


## Calculate mean lat by year in observations and in models
	folder = 'CEModels'; date='2012-03-04'; vars = c('bottemp', 'surftemp') 
	
	files=list.files(paste('Output/', folder, sep=''), pattern='.RData') # only works if only one round of CEM models are in the directory

	# save observed and predicted positions for lat and long
	meanpos = list(0)

	for(s in 1:length(files)){
		print(s)
		load(paste('Output/',folder, '/', files[s], sep=''))
		thisspp=strsplit(files[s], split=paste('CEmods_|_S|_F', min(trainingyrs), '|_', max(trainingyrs), sep=''))[[1]][2] # extract spp name from file name
	
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
			inds = datatrim$year == yrs[i] & complete.cases(datatrim[,vars]) & datatrim$spp == thisspp
			if(sum(inds)>0){
				meanlat[[1]][i] = weighted.mean(datatrim$lat[inds],w= datatrim$wtcpue[inds], na.rm=T)
				meanlong[[1]][i] = weighted.mean(datatrim$lon[inds],w= datatrim$wtcpue[inds], na.rm=T)
		
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
					meanlat[[j+1]][i] = weighted.mean(datatrim$lat[inds][!is.na(preds)], w = preds[!is.na(preds)])		
					meanlong[[j+1]][i] = weighted.mean(datatrim$lon[inds][!is.na(preds)], w = preds[!is.na(preds)])		
				}
			}
		}
		
		meanpos[[s]] = list(meanlat = meanlat, meanlong = meanlong, yrs = yrs)
		names(meanpos)[s] = thisspp
	}
	
	### Save meanpos and direction
	save(meanpos, file = paste('Output/meanpos_', min(trainingyrs), 'to', max(trainingyrs), '_', Sys.Date(), '.RData', sep=''))


##########################
######## Plots ###########
##########################
	
	max3 = function(x, na.rm=FALSE){
		head = sort(x, decreasing=TRUE)[1:3]
		return(mean(head, na.rm=na.rm))
	}
	
	min3 = function(x, na.rm=FALSE){
		head = sort(x, decreasing=FALSE)[1:3]
		return(mean(head, na.rm=na.rm))
	}

# Plot predictions and reality as maps
	#yrtoplot = list(one=1968:1989, two= 1990:2011)
	yrtoplot = list(one=1968:1979, two= 2000:2011)
	#yrtoplot = list(one=1968:1979, two= 1980:1989, three=1990:1999, four=2000:2009)
	#yrtoplot = 1970
	spp = 'Homarus americanus'
	spp = 'Gadus morhua'
	c = datatrim$wtcpue
	a = max(log(c), na.rm=T)/2
	cols = rev(rainbow(n=ceiling(a)+1, start=0, end=4/6))
	xlims = c(-75.5,-65.5)
	#ylims = c(35,45)
	ylims = c(37,44.7) # for cod
	col = rgb(10,255,0,150,maxColorValue=255)

	mods = list(gamhurdle = list(gamhp2, gamha2))
	quartz(width=min(12,length(yrtoplot)*4), height=4)
	# pdf(paste('Figures/Maps_CEM_', spp, '_', season, '_', Sys.Date(), '.pdf', sep=''), width=min(12,length(yrtoplot)*4), height=4)
	par(mfcol=c(length(mods),length(yrtoplot)), mai=c(0.3, 0.3, 0.2, 0.1)) # use mfcol to fill by columns
	if(length(yrtoplot)==2) par(mai=c(0.5, 0.5, 0.3, 0.1)) # use mfcol to fill by columns
	for(i in 1:length(yrtoplot)){
		for(j in 1:length(mods)){
			if(length(yrtoplot[[i]])==1) title = yrtoplot[[i]]
			if(length(yrtoplot[[i]])>1) title = paste(yrtoplot[[i]][1], 'to', max(yrtoplot[[i]]))			
			#map(database='state', regions=c('massachusetts', 'rhode island', 'maine', 'connecticut', 'new york', 'new jersey', 'delaware', 'new hampshire', 'vermont', 'pennsylvania', 'maryland'), fill=TRUE, col='grey', xlim=xlims, ylim=ylims, main=title)
			map(database='usa', xlim=xlims, ylim=ylims, fill=TRUE, col='light grey')
			map.axes()
			mtext(title, side=3)
			inds = datatrim$year %in% yrtoplot[[i]] & datatrim$spp == spp
			inds2 = inds & datatrim$wtcpue > 0
			c = datatrim$wtcpue[inds2]
			points(datatrim$lon[inds2], datatrim$lat[inds2], pch=16, cex=ceiling(log(c))/a, col = 'black')
			meanlat = weighted.mean(datatrim$lat[inds2],w= datatrim$wtcpue[inds2])
			points(min(xlims)+0.5, meanlat, cex=2, pch=16)
			if(i==1) baselat = meanlat
			abline(h=baselat, lty=2, col='black')
			#abline(h=min(datatrim$lat[inds2]), lty=1, col='black') # min and max lat
			#abline(h=max(datatrim$lat[inds2]), lty=1, col='black')

			if(length(mods[[j]]) == 1){
				preds = predict(mods[[j]][[1]], newdata = datatrim[inds,], type='response')
			}
			if(length(mods[[j]]) == 2){
				preds1 = predict(mods[[j]][[1]], newdata = datatrim[inds,], type='response')
				preds2 = exp(predict(mods[[j]][[2]], newdata = datatrim[inds,], type='response'))
				preds = preds1*preds2
			}
			preds[preds<0] = 0
			points(datatrim$lon[inds][preds>0], datatrim$lat[inds][preds>0], pch=16, xlab='', cex=ceiling(log(preds[preds>0]))/a, ylab='', col = col)
			meanpredlat = weighted.mean(datatrim$lat[inds][!is.na(preds)], w = preds[!is.na(preds)])
			points(min(xlims)+0.5, meanpredlat, cex=2, col=col, pch=16)
			#if(i==1) basepredlat = meanpredlat
			#abline(h=basepredlat, lty=2, col=col)
			#abline(h=min(datatrim$lat[inds][preds>7e-1], na.rm=TRUE), lty=1, col=col) # min and max predicted lat
			#abline(h=max(datatrim$lat[inds][preds>7e-1], na.rm=TRUE), lty=1, col=col)
		}
	}
	
	dev.off()
	

# Plot predictions and reality as graphs of mean lat
	
	# Plot mean lat and long over time
	quartz(width=5, height=7)
	# pdf(paste('Figures/Meanlat&long_CEM_', spp, '_', season, '_', Sys.Date(), '.pdf', sep=''), width=7, height=6)

	par(mfrow=c(2,1), mai=c(0.7,0.6,0.5, 0.3), mgp = c(2, 0.7, 0))
	cols = c('black', 'blue', 'green', 'red')
	#ylims = range(unlist(lapply(meanlat, FUN=range, na.rm=T)))
	ylims = range(range(meanlat[['obs']], na.rm=T))
	plot(yrs, meanlat[['obs']], type='o', pch=16, xlab='Year', ylab = 'Mean latitude', ylim=ylims, main=spp, col=cols[1])
		abline(lm(meanlat[['obs']] ~ yrs), col=cols[1], lty=2)
		#abline(h = mean(meanlat[['obs']], na.rm=T), col='grey', lty=1)
	#points(yrs, meanlat[['glm']], type='o', pch=16, col=cols[2])
	#	abline(lm(meanlat[['glm']] ~ yrs), col=cols[2], lty=2)
	#points(yrs, meanlat[['gam']], type='o', pch=16, col=cols[3])
	#	abline(lm(meanlat[['gam']] ~ yrs), col=cols[3], lty=2)
	points(yrs, meanlat[['gamhurdle']], type='o', pch=16, col=cols[4])
		abline(lm(meanlat[['gamhurdle']] ~ yrs), col=cols[4], lty=2)
	#legend('topleft', legend=c('Observed', 'Poisson GLM', 'Zero-inflated poisson GLM', 'GAM'), col= cols, pch=16, lty=1, bty='n')
	legend('topleft', legend=c('Observed', 'Predicted'), col= cols[c(1,4)], pch=16, lty=1, bty='n')
	
	#ylims = range(unlist(lapply(meanlong, FUN=range, na.rm=T)))*c(1.01,0.99)
	ylims = range(meanlong[['obs']], na.rm=T)
	plot(yrs, meanlong[['obs']], type='o', pch=16, xlab='Year', ylab = 'Mean longitude', ylim=ylims, main=spp, col=cols[1])
		abline(lm(meanlong[['obs']] ~ yrs), col=cols[1], lty=2)
		#abline(h = mean(meanlong[['obs']], na.rm=T), col='grey', lty=1)
	#points(yrs, meanlong[['glm']], type='o', pch=16, col=cols[2])
	#	abline(lm(meanlong[['glm']] ~ yrs), col=cols[2], lty=2)
	#points(yrs, meanlong[['gam']], type='o', pch=16, col=cols[3])
	#	abline(lm(meanlong[['gam']] ~ yrs), col=cols[3], lty=2)
	points(yrs, meanlong[['gamhurdle']], type='o', pch=16, col=cols[4])
		abline(lm(meanlong[['gamhurdle']] ~ yrs), col=cols[4], lty=2)

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