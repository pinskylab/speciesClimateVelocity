## How fast are species moving in AFSC Aleutians data?


#########################
## Trim to spr or fall ##
#########################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')
strata = read.csv('AFSCAleutiansStrata.csv')
data1 = read.csv('Processed/ai1983_1997.csv')
	dim(data1)
	names(data1)
data2 = read.csv('Processed/ai2000_2010.csv')
	dim(data2)
	names(data2)
	
data = rbind(data1, data2)
	dim(data) # 93461 rows

# Create a unique haulid
	data$haulid = paste(formatC(data$VESSEL, width=3, flag=0), formatC(data$CRUISE, width=3, flag=0), formatC(data$HAUL, width=3, flag=0), sep='-')
	length(unique(data$haulid)) # 3811

# Trim to high quality strata
	i = table(data$STRATUM, data$YEAR); i
	
	which(rowSums(i>0) != 10) # strata without a full set of years

	data = data[!(data$STRATUM %in% c(221, 411, 421, 521, 611)),]
	dim(data) # 88698 rows
	length(unique(data$haulid)) # 3589
	
# Reasonable depths? 
	# hist(data$BOT_DEPTH) # 
	range(data$BOT_DEPTH, na.rm=T) # 9-521

	dbins = round(data$BOT_DEPTH/25)*25
	table(dbins, data$YEAR)

# Fix column names
	names(data)[names(data)=='VESSEL'] = 'svvessel'
	names(data)[names(data)=='CRUISE'] = 'cruise'
	names(data)[names(data)=='STATION'] = 'station'
	names(data)[names(data)=='STRATUM'] = 'stratum'
	names(data)[names(data)=='HAUL'] = 'tow'
	names(data)[names(data)=='YEAR'] = 'year'
	names(data)[names(data)=='LATITUDE'] = 'lat'
	names(data)[names(data)=='LONGITUDE'] = 'lon' 
	names(data)[names(data)=='SURF_TEMP'] = 'surftemp'
	names(data)[names(data)=='BOT_TEMP'] = 'bottemp'
	names(data)[names(data)=='BOT_DEPTH'] = 'depth'
	names(data)[names(data)=='SCIENTIFIC'] = 'spp'
	names(data)[names(data)=='WTCPUE'] = 'wtcpue'
	names(data)[names(data)=='NUMCPUE'] = 'numcpue'
	names(data)[names(data)=='COMMON'] = 'common'

# Turn -9999 to NA
	data$numcpue[data$numcpue==-9999] = NA
	data$wtcpue[data$wtcpue==-9999] = NA
	data$bottemp[data$bottemp==-9999] = NA
	data$surftemp[data$surftemp==-9999] = NA
	
	range(data$numcpue)
	range(data$numcpue, na.rm=T)
	range(data$wtcpue)
	range(data$wtcpue, na.rm=T)

# Add blank columns that are missing in this region
	data$surfsal = NA
	data$botsal = NA

# What months, day, time, julian?
	# Convert from local time to UTC
	dt = strptime(data$DATETIME, tz="America/Anchorage", format="%m/%d/%Y%H:%M") # first convert to POSIXlt object
		head(dt)
	dt.pos = as.POSIXct(dt, tz='America/Anchorage') # convert to POSIXct object. Note that AK was in AHDT time zone in 1983, but AKDT starting October 1983 (http://www.alaskahistoricalsociety.org/index.cfm/discover-alaska/Glimpses-of-the-Past/98)
		head(dt.pos)
	dtu = format(dt.pos, tz='GMT', usetz=TRUE) # convert to UTC in text
		head(dtu)
	dtl = as.POSIXlt(dtu, tz='GMT') # convert back to POSIXlt so I can extract year/month/day/time
		head(dtl)
	data$month = dtl$mon+1 # month, add 1 since POSIXlt starts at 0	
	data$day = dtl$mday # day of the month
	data$time = paste(formatC(dtl$hour, width=2, flag=0), formatC(dtl$min, width=2, flag=0), sep=':')
		# check my conversions
		data[!duplicated(data[,c('year', 'month', 'day')]),c('year', 'DATETIME', 'month', 'day', 'time')][1:10,]
		data[data$year==1983 & data$month %in% c(10,11) & !duplicated(data[,c('year', 'month', 'day')]),c('year', 'DATETIME', 'month', 'day', 'time')][1:20,] # check that conversion was +9 up until 10/30/1983, and +8 after that
		data[data$year==1986 & !duplicated(data[,c('year', 'month', 'day')]),c('year', 'DATETIME', 'month', 'day', 'time')][1:10,] # 
		
	table(data$month, data$year)
		# was July-Nov in 1983
		# became May-August by 1994-2010

	require(date)
	data$julian = as.numeric(as.date(substr(data$DATETIME,1,10)))-as.numeric(as.date(paste('01/01/', data$year, sep=''))) # julian day since Jan 1

# Add strata areas
names(strata)[names(strata)=='StratumCode'] = 'stratum'
names(strata)[names(strata)=='Areakm2'] = 'stratarea'
dim(data)
data = merge(data, strata[,c('stratum', 'stratarea')])
dim(data)

# Find duplicate tows in same location
	# first sort by time so first index is always the earliest
	data = data[order(data$year, data$month, data$day, data$time, data$spp),]
	
	# turn factors to chars so we can modify them
	data$svvessel = as.character(data$svvessel)
	data$cruise = as.character(data$cruise)
	data$tow = as.character(data$tow)
	data$stratum = as.character(data$stratum)
	data$spp = as.character(data$spp)
	data$common = as.character(data$common)

	# Any completely duplicated rows?
	dups = which(duplicated(data))
		sum(dups) # 0

	# find tows that have same lat/lon but different haulid
	inds = which(duplicated(data[,c('year', 'lat', 'lon')]) & !duplicated(data$haulid))
		length(inds) # 0


# Create list of all hauls
	goodhauls = data[!duplicated(data$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'month', 'day', 'julian', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhauls) # 3589
	head(goodhauls)

# Adjust spp names for those cases where they've changed
	table(data$spp, data$year)

	data$sppold = data$spp # save it for later checking for duplicate tows
	data$spp[data$spp %in% c('Atheresthesevermanni', 'Atheresthesstomias')] = 'Atheresthessp.'
	data$common[data$spp %in% c('Atheresthesevermanni', 'Atheresthesstomias')] = '' # no common name

	data$spp[data$spp %in% c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')] = 'Lepidopsettasp.'
	data$common[data$spp %in% c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')] = 'rocksoleunident.'

	data$spp[data$spp %in% c('Myoxocephalusjaok', 'Myoxocephalusniger', 'Myoxocephaluspolyacanthocephalus', 'Myoxocephalusquadricornis', 'Myoxocephalusverrucosus')] = 'Myoxocephalussp.'
	data$common[data$spp %in% c('Myoxocephalusjaok', 'Myoxocephalusniger', 'Myoxocephaluspolyacanthocephalus', 'Myoxocephalusquadricornis', 'Myoxocephalusverrucosus')] = '' # no common name

	data$spp[data$spp %in% c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajainterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')] = 'Bathyrajasp.'
	data$common[data$spp %in% c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajainterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')] = '' # no common name

	# check changes: should be one name per search
	spp = unique(data$spp)
	spp[grep('Atheresthes', spp)]
	spp[grep('Lepidopsetta', spp)]
	spp[grep('Hippoglossoides', spp)]
	spp[grep('Myoxocephalus', spp)]	
	spp[grep('Bathyraja', spp)]

# Trim to good scientific names?
	i = data$spp == ''
	sort(unique(data$common[i]))

	data = data[!i,]
	dim(data) # 84339 rows
	length(unique(data$haulid)) # 3588
	
# Find duplicate rows (repeat rows for the same tow), and combine entries from spp that have now been consolidated. Already checked for duplicate tows in same location (see above)
	# first sort by time so first index is always the earliest
	data = data[order(data$year, data$month, data$day, data$time, data$spp),]

	inds = which(duplicated(data[, c('spp', 'year', 'haulid')]))
	# inds = which(duplicated(data[, c('sppold', 'year', 'lat', 'lon')])) # use original spp names for finding true dupcliates
	# inds = which(duplicated(data[, c('spp', 'year', 'stratum', 'station')]))
	length(inds) # 2387

	# turn factors to chars so we can modify them
	data$station = as.character(data$station)
	data$common = as.character(data$common)
	data$spp = as.character(data$spp)
	data$time = as.character(data$time)

	# new copy of data so that averaging duplicate rows won't modify the original copy
	newdata = data

	# Trim some unneeded columns from newdata (t_bottom has same bottom temps, depth has mean of depthst and depthend)
	nm = names(newdata)[!(names(newdata) %in% c('DATETIME', 'SID', 'MONTH', 'DAY', 'sppold'))]
	newdata = newdata[,nm]
	names(newdata)

	# Rows to drop
	droprows = numeric(0)
	
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	# Sum the entries: looks like the right thing based on visual inspection of the data (same haulids)
	for(i in 1:length(inds)){
		if(i %% 100 == 0) print(i)
	
		inds2 = which(data$spp == data$spp[inds[i]] & data$year == data$year[inds[i]] & data$haulid == data$haulid[inds[i]])
		# inds2 = which(data$spp == data$spp[inds[i]] & data$year == data$year[inds[i]] & data$stratum == data$stratum[inds[i]] & data$station == data$station[inds[i]])
		data[inds2,]

		# put sums in the first row
		temp = data.frame(
		numcpue = sumna(data$numcpue[inds2]), 
		wtcpue = sumna(data$wtcpue[inds2]))

		newdata$wtcpue[inds2[1]] = temp$wtcpue
		newdata$numcpue[inds2[1]] = temp$numcpue

		# mark the following row(s) for removal
		droprows = c(droprows, inds2[2:length(inds2)])
	}
	droprows = sort(unique(droprows)) # trim droprows to just the unique values
	
	# make sure no two tows were combined in the same row. they'll have a comma
	i = grep(',', newdata$haulid)
	length(i) # should be zero
	
	# drop the duplicated rows
	all(inds == droprows) # should be the same
	dim(newdata)
	newdata = newdata[-droprows,]
	dim(newdata)

	dim(data)
	dim(newdata)
	length(inds)
	length(inds) + nrow(newdata) # should match nrow(data)
		
	
# How many tows?
	length(unique(paste(newdata$year, newdata$lat, newdata$lon))) # 3588 unique locations
	length(unique(newdata$haulid)) # 3588: good that this matches # locations

# How many spp?
	length(unique(newdata$spp)) #1101
	
# Calculate a corrected longitude (all in western hemisphere coordinates)
	newdata$lon[newdata$lon>0] = newdata$lon[newdata$lon>0] - 360	

# Add a region column
newdata$region = "AFSC_Aleutians"

# How many hauls missing from data?
	setdiff(goodhauls$haulid, unique(newdata$haulid)) # lose one haul that only had inverts
	
# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdata$month, newdata$year)
	# surveys don't cross Jan 1, so yearsurv == year
	newdata$yearsurv = newdata$year
	newdata$juliansurv = newdata$julian	

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	ncol(newdata)
	length(nm)
	setdiff(nm, names(newdata))
	setdiff(names(newdata), nm)

newdataout = newdata[,nm]
	dim(newdataout) # 81,952 x 26

# Write out
	write.csv(newdataout, paste('Output/data_', Sys.Date(), '.csv', sep=''))
	write.csv(goodhauls, paste('Output/goodhauls_', Sys.Date(), '.csv', sep=''))

##############################################	
## Trim to good spp with data and add zeros ##
##############################################	
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')

data = read.csv('Output/data_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
goodhauls = read.csv('Output/goodhauls_2012-04-01.csv', row.names=1)
	
# Identify the spp to analyze
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	spplist = aggregate(list(count=data$numcpue, weight = data$wtcpue, pres = data$wtcpue>0), by=list(spp=data$spp, year=data$year), FUN=sumna)
	spplist = aggregate(list(count=spplist$count, weight=spplist$weight, pres = spplist$pres, presyr = spplist$pres>0), by=list(spp=spplist$spp), FUN=sum) # presyr col holds # years in which spp was present
	rownames(spplist) = 1:nrow(spplist)
	nrow(spplist) # 1101 spp
	
	sum(spplist$pres>1) # 895
	sum(spplist$pres>10) # 475
	max(spplist$presyr) # 10 yrs of data
	sum(spplist$presyr == max(spplist$presyr)) # 66 spp present every year

	spplist = spplist[spplist$presyr == max(spplist$presyr),] # take all spp present >= 1x per yr
		nrow(spplist) # 66 spp	

	spplist = merge(spplist, data[!duplicated(data$spp), c('common', 'spp')])
		dim(spplist)
		head(spplist)

# Remove spp not in spplist
	dim(data)
	data = data[data$spp %in% spplist$spp,]
	dim(data) # 44690

# Any spp duplicated in the same haul?
	i = duplicated(paste(data$haulid, data$spp))
	sum(i)

	j = data$haulid == data$haulid[i] & data$spp == data$spp[i]
	sum(j)

	k = duplicated(data)
	sum(k) # the whole row is not duplicated. appears to only be different at wtcpue and numcpue
	
# Add any missing hauls
	inds = which(!(goodhauls$haulid %in% data$haulid))
	length(inds) # add 2 rows (must have lost another haulid when I trimmed down the spp)
		inds
		goodhauls[inds,]
	
	add = goodhauls[inds,]
		dim(add)
		add$region = unique(data$region)
		add$spp = spplist$spp[1] # just need to add something here, so first spp works
		add$common = spplist$common[1]
		add$wtcpue = 0
		add$numcpue = 0
	add = add[,names(data)]

	dim(data)
	data = rbind(data, add)
	dim(data)
	
# Fill in zeros
	fin = length(unique(data$haulid))*length(unique(data$spp)) # expected final size
		fin
	hauls = unique(data$haulid)
		length(hauls)
		nrow(goodhauls) # should match
		
	# set up sets of unique hauls and spp
	newdata = data[!duplicated(data$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'day')]
		dim(newdata)
		newdata = newdata[order(newdata$haulid),] # sort by haulid
		rownames(newdata) = 1:nrow(newdata)
	spps = data[!duplicated(data$spp), c('spp', 'common')]
		dim(spps)
		spps = spps[order(spps$spp),]
		rownames(spps) = 1:nrow(spps)
		
	# expand so that each spp can get a haul
	newdata = newdata[rep(rownames(newdata), rep(nrow(spps), nrow(newdata))),]
		dim(newdata)
			
		# add spp info, replicated so that each haul gets a species
	newdata = cbind(newdata, spps[rep(rownames(spps), length(unique(newdata$haulid))),])
		dim(newdata)
		
		# add catch info. set NAs to -9999 so they're not confused with an absence
		datana = data[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datana$numcpue[is.na(data$numcpue)] = -9999
		datana$wtcpue[is.na(data$wtcpue)] = -9999
	newdata = merge(newdata, datana[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdata) # 236874
		summary(newdata$numcpue)
		summary(newdata$wtcpue)

		# set catch NAs to zero (absences) and -9999 to NA (true missing data)
	newdata$numcpue[is.na(newdata$numcpue)] = 0
	newdata$wtcpue[is.na(newdata$wtcpue)] = 0
	newdata$numcpue[newdata$numcpue == -9999] = NA
	newdata$wtcpue[newdata$wtcpue == -9999] = NA

	# If dim(data) and fin don't match, maybe there's a duplicate entry
	inds = which(duplicated(newdata[,c('haulid', 'spp')]))
	length(inds) # YES, if this is > 0

	#data[inds,]
	
		# How many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=newdata$haulid), by=list(spp=newdata$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
		# A blank row?
		inds = which(is.na(newdata$spp))
		length(inds) # should be 0
		#newdata[inds,]
		
		#newdata = newdata[-inds,]
		#dim(newdata)

	# sort by time so first index is always the earliest
	newdata = newdata[order(newdata$year, newdata$month, newdata$day, newdata$time, newdata$spp),]

# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdata$month, newdata$year)
	# surveys don't cross Jan 1, so yearsurv == year
	newdata$yearsurv = newdata$year
	newdata$juliansurv = newdata$julian	

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	dim(newdata)
	length(nm)
	setdiff(nm, names(newdata))
	setdiff(names(newdata), nm)

	newdata = newdata[,nm]

# Fix names
	row.names(newdata) = 1:nrow(newdata)

# Write out
	write.csv(newdata, paste('Output/datatrimwzeros_', Sys.Date(), '.csv', sep=''))

######################################
## Calc lat and long by year by spp ##
######################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/AFSC Aleutians')

data = read.csv('Output/datatrimwzeros_2012-04-01.csv', row.names=1, stringsAsFactors=TRUE)
strata = read.csv('AFSCAleutiansStrata.csv')

