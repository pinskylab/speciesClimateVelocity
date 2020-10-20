## Prep datafiles for the SEFSC Gulf of Mexico trawl survey


#########################
## Trim ##
#########################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/SEFSC Gulf')
#strata = read.csv('AFSCGulfofAKStrata.csv')
envi = read.csv('csv/ENVREC.csv')
	nrow(envi)
	names(envi)
	sum(i <- duplicated(envi[,c('CRUISEID', 'STATIONID')])) # 26 duplicates
	envi[which(envi$CRUISEID %in% envi$CRUISEID[i] & envi$STATIONID %in% envi$STATIONID[i]),] # pairs of duplicated lines, but one of the pair is NA
	i = which(!is.na(envi$SALSURF) & !is.na(envi$SALMAX))
		length(i)
	envi = envi[i,]	
		sum(i <- duplicated(envi[,c('CRUISEID', 'STATIONID')])) # 5 duplicates
		envi[which(envi$CRUISEID %in% envi$CRUISEID[i] & envi$STATIONID %in% envi$STATIONID[i]),] # pairs of duplicated lines, but one of the pair is NA
	i = !duplicated(envi[,c('CRUISEID', 'STATIONID')]) # keep the first record in each run of duplicates (ok since no missing salinity data)
		sum(i)
	envi = envi[i,]	
	
station = read.csv('csv/STAREC.csv')
	names(station)
	dim(station)
tow = read.csv('csv/INVREC.csv')
	names(tow)
	dim(tow)
	tow$COMBIO[tow$OP %in% c('M', 'U')] # what comments go with M or U?
bio = read.csv('csv/BGSREC.csv')
	names(bio)
	dim(bio)
	setdiff(bio$CRUISEID, tow$CRUISEID) # 12 cruises not in tow
	setdiff(tow$CRUISEID, bio$CRUISEID) # 0 missing from bio
	setdiff(bio$CRUISEID, station$CRUISEID) # 0 missing from station
	length(setdiff(station$CRUISEID, bio$CRUISEID)) # 167 in station but not in bio
	
	length(setdiff(station$STATIONID, bio$STATIONID)) # 18637 in station but not in bio
	length(setdiff(bio$STATIONID, station$STATIONID)) # 0 in bio but not in station
spp = read.csv('csv/NEWBIOCO.csv')
	names(spp)
	dim(spp)
	sum(duplicated(spp$CODE)) # some duplicated spp codes
	spp[spp$CODE %in% spp$CODE[which(duplicated(spp$CODE))],]
	new = data.frame(KEY1 = c(503,5770), TAXONOMIC = c('ANTHIAS TENUIS AND WOODSI', 'MOLLUSCA AND UNID.OTHER #01'), CODE=c(170026003, 300000000), TAXONSIZEC=NA, ISACTIVE=-1, COMMON_NAM=c('threadnose and swallowtail bass', 'molluscs or unknown'), TSN = NA) # make two combined records
	spp = spp[!(spp$CODE %in% spp$CODE[which(duplicated(spp$CODE))]),] # remove duplicates
	spp = rbind(spp, new)
	dim(spp)
cruise = read.csv('csv/CRUISES.csv')
	names(cruise)


data = merge(bio[,c('CRUISEID', 'STATIONID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO', 'BGSID', 'GENUS_BGS', 'SPEC_BGS', 'BIO_BGS', 'BGSCODE', 'CNTEXP', 'SELECT_BGS')], tow[,c('STATIONID', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'BOT_TYPE', 'OP')], all.x=TRUE, all.y=TRUE)
	dim(data) # 644,853
data = merge(data, station[,c('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'STAT_ZONE', 'TIME_ZN', 'TIME_MIL', 'TIME_EMIL', 'S_LATD', 'S_LATM', 'S_LATH', 'S_LOND', 'S_LONM', 'S_LONH', 'E_LATD', 'E_LATM', 'E_LATH', 'E_LOND', 'E_LONM', 'E_LONH', 'DEPTH_SSTA', 'MO_DAY_YR', 'GEARS', 'TEMP_SSURF', 'TEMP_BOT', 'VESSEL_SPD', 'COMSTAT')], all.x=TRUE, all.y=TRUE)
	dim(data) # 663,490: looks like we add some stations with no tows?
data = merge(data, spp[,c('CODE', 'TAXONOMIC', 'COMMON_NAM')], by.x='BIO_BGS', by.y='CODE', all.x=TRUE)
	dim(data) # 663,490
data = merge(data, envi[,c('CRUISEID', 'STATIONID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO', 'SALSURF', 'SALMAX')], all.x=TRUE)
	dim(data) # 663,490
data = merge(data, cruise[,c('CRUISEID', 'VESSEL', 'TITLE')], all.x=TRUE) # add cruise title
	dim(data) # 663,490

	#sort(unique(data$VESSEL))
	length(unique(data$CRUISEID)) # 793
	length(unique(data$STATIONID)) # 47,210

# Trim to high-quality tows (use OP code)
		# sort(unique(data$OP))
		# table(data$OP)
	i = data$OP %in% c('') # the subset used by Jeff Rester's GS_TRAWL_05232011.sas
		sum(i) # 631,156
	data = data[i,]
		dim(data) # 631,156

# Trim to shrimp trawl gear
	# sort(unique(data$GEAR_TYPE))
	# table(data$year, data$GEAR_TYPE)
	# hist(data$GEAR_SIZE[data$GEAR_TYPE=='ST'])
	# table(data$GEAR_SIZE[data$GEAR_TYPE=='ST']) # mostly 40
	# table(data$MESH_SIZE[data$GEAR_TYPE=='ST' & data$GEAR_SIZE==40]) # mostly 1.63
	
	i = data$GEAR_TYPE == 'ST' & data$GEAR_SIZE==40 & data$MESH_SIZE == 1.63 & !is.na(data$MESH_SIZE) # the subset used by Jeff Rester's GS_TRAWL_05232011.sas
		sum(i) # 523,790
	data = data[i,]

# Trim to SEAMAP summer trawls
	#sort(unique(data$TITLE))
		# table(data$TITLE)
	i = data$TITLE %in% c('Summer SEAMAP Groundfish Survey', 'Summer SEAMAP Groundfish Suvey')
		sum(i) # 215,427
	data = data[i,]
		dim(data)
	
# Create a unique haulid (follow Jeff Rester's GS_TRAWL_05232011.sas)
	# sum(duplicated(data$CRUISE_NO) & !duplicated(data$CRUISEID)) # 23 cruises have multiple cruise ids
	# sum(duplicated(data[,c('VESSEL', 'CRUISE_NO')]) & !duplicated(data$CRUISEID)) # 0 cruises have multiple cruiseids after accounting for vessel
	# sum(duplicated(data$CRUISEID) & !duplicated(data$CRUISE_NO)) # 0 cruiseids have multiple cruises
	# sum(duplicated(data$P_STA_NO) & !duplicated(data$STATIONID)) # 4751 stations have multiple station ids (not accounting for vessel or cruise)
	# sum(duplicated(data[,c('VESSEL', 'CRUISE_NO', 'P_STA_NO')]) & !duplicated(data$STATIONID)) # 0 haulids have multiple stationids
	
	data$haulid = paste(formatC(data$VESSEL, width=3, flag=0), formatC(data$CRUISE_NO, width=3, flag=0), formatC(data$P_STA_NO, width=5, flag=0, format='d'), sep='-')
	length(unique(data$haulid)) # 8,351

# Add strata (define by lat and depth bands)
	data$S_LATD[data$S_LATD == 0] = NA
	data$S_LOND[data$S_LOND == 0] = NA
	data$E_LATD[data$E_LATD == 0] = NA
	data$E_LOND[data$E_LOND == 0] = NA
	data$lat = rowMeans(cbind(data$S_LATD + data$S_LATM/60, data$E_LATD + data$E_LATM/60), na.rm=T) # mean of start and end positions
	data$lon = -rowMeans(cbind(data$S_LOND + data$S_LONM/60, data$E_LOND + data$E_LONM/60), na.rm=T) # need negative sign since western hemisphere
	data$depth = data$DEPTH_SSTA*1.8288 # convert fathoms to meters
	stratlatgrid = floor(data$lat)+0.5 # degree bins
	stratlongrid = floor(data$lon)+0.5 # degree bins
	stratdepthgrid = floor(data$depth/100)*100 + 50 # 100 m bins
	data$stratum = paste(stratlatgrid, stratlongrid, stratdepthgrid, sep='-')
		length(unique(data$stratum)) # 61

# What months,days,times?
	data$timemil = data$TIME_MIL
	data$timemil[nchar(data$timemil)==1] = paste('000', data$timemil[nchar(data$timemil)==1], sep='') # add 000 where needed
	data$timemil[nchar(data$timemil)==2] = paste('00', data$timemil[nchar(data$timemil)==2], sep='') # add 00 where needed
	data$timemil[nchar(data$timemil)==3] = paste('0', data$timemil[nchar(data$timemil)==3], sep='') # add 0 where needed
	data$datetime = paste(data$MO_DAY_YR, data$timemil)
		# unique(nchar(data$datetime)) # visually check ones with too few chars
		# i = which(nchar(data$datetime)==14); i[1:10]
		# data$datetime[i][1:10]
		# i = which(nchar(data$datetime)==13); i[1:10]
		# data$datetime[i][1:10]

	# Set up time zones
	data$timezone = NA
	data$timezone[data$TIME_ZN==2] = 'America/New_York' # Daylight savings
	data$timezone[data$TIME_ZN==3] = 'America/Chicago'
	data$timezone[data$TIME_ZN==4] = 'America/Chicago' # Daylight savings
	data$timezone[data$TIME_ZN==5] = 'America/Halifax'
	data$timezone[data$TIME_ZN==8] = 'GMT'

	# Convert from local time to UTC
	tzs = sort(unique(data$timezone))
	dt = vector('list', length(tzs))
	ord = vector('list', length(tzs))
	for(i in 1:length(tzs)){
		print(i)
		j = which(data$timezone == tzs[i])
		k = strptime(data$datetime[j], tz=unique(data$timezone[j]), format="%m/%d/%Y %H%M") # first convert to POSIXlt object
		dt[[i]] = k
		ord[[i]] = j
	}
	dt2 = c(dt[[1]], dt[[2]]) # concat them together
	ord2 = c(ord[[1]], ord[[2]])
	ord3 = order(ord2) # find the original order
	dt3 = dt2[ord3] # re-order
		length(dt3) # 184,071
		# nrow(data)	
		# check by eye
#		cbind(data$datetime[1:10], as.character(dt3[1:10]))
#		cbind(data$datetime[1000:1010], as.character(dt3[1000:1010]))
#		cbind(data$datetime[4571:4580], as.character(dt3[4571:4580]))
#		cbind(data$datetime[7326:7335], as.character(dt3[7326:7335]))
#			dt3[7326]; data$timezone[7326]; data$datetime[7326]
		sum(is.na(dt3)) # 0
	dt.pos = as.POSIXct(dt3) # convert to POSIXct object.
#	 		head(dt.pos)
	dtu = format(dt.pos, tz='GMT', usetz=TRUE) # convert to UTC in text
#		head(dtu)
	dtl = as.POSIXlt(dtu, tz='GMT') # convert back to POSIXlt so I can extract year/month/day/time
#		head(dtl)
#		summary(dtl$mon)
	data$year = 1900 + dtl$year
	data$month = dtl$mon+1 # month, add 1 since POSIXlt starts at 0	
	data$day = dtl$mday # day of the month
	data$time = paste(formatC(dtl$hour, width=2, flag=0), formatC(dtl$min, width=2, flag=0), sep=':')
		# check my conversions
#		data[!duplicated(data[,c('year', 'month', 'day')]),c('timezone', 'year', 'datetime', 'month', 'day', 'time')][1:50,]

#	table(data$month, data$year)
		# June/July is consistent all years

	require(date)
	data$julian = as.numeric(as.date(paste(data$month, data$day, data$year, sep='/')))-as.numeric(as.date(paste('01/01/', data$year, sep=''))) # julian day since Jan 1
#		data[!duplicated(data[,c('year', 'month', 'day')]),c('timezone', 'datetime', 'month', 'day', 'year', 'time', 'julian')][1:50,]

	# Add survey julian day and year (only relevant for surveys that cross Jan1)
	data$yearsurv = data$year
	data$juliansurv = data$julian

# Trim to high-quality years (sample all strata)	
	i = !(data$year %in% c(1982, 1983))
		sum(i)
	data = data[i,] # 1983 didn't sample many strata
		dim(data) # 205,376
	
# Trim to high quality strata (sampled every year)
	i = table(data$stratum, data$year); # i
	sum = rowSums(i>0)
	sumc = colSums(i>0)
#		as.data.frame(sum) # how many years per stratum?
#			hist(sum, breaks=60, col='grey') # most strata cover 1 years, next most cover 29
#			sum(sum==28) # 2 stratum
#			sum(sum==29) # 15 strata
#			sum(sum==30) # 0 strata
#		as.data.frame(sumc) # how many strata per year?
#		hist(sumc, col='grey', breaks=60) # most years have 28 strata, all years have at least 25, most is 50
	i2 = i[sum>25,]; # i2
#		colSums(i2>0) # all years have >= 17 strata, except 1983
#		rowSums(i2>0)
	strats = rownames(i2)[rowSums(i2>0)==28]
		length(strats) # 16 strata
		#i = !duplicated(data$haulid) & data$stratum %in% strats
		#plot(data$lon[i], data$lat[i])
		
	data = data[data$stratum %in% strats,]
		dim(data) # 168,559

# Reasonable depths? 
#	hist(data$depth) # 
	range(data$depth, na.rm=T) # 4-100

	# Plot a map of survey points colored by depth
		# set up the colors
#		d = round(data$depth/10)*10
#		d = rank(d, na.last='keep', ties.method='min')
#		du = sort(unique(d))
#		for(i in 1:length(du)) d[d==du[i]] = i
#		cols = terrain.colors(length(du))
#	plot(data$lon, data$lat, pch=16, col=cols[d])

# Trim out or fix speed and duration records
#		hist(data$MIN_FISH, breaks=100)
#		hist(data$MIN_FISH, xlim=c(0,100), breaks=100)
	i = data$MIN_FISH<=60 & data$MIN_FISH > 0 
		i[is.na(i)] = FALSE
		sum(i) # 167,800
		sum(!i) # 759 to be trimmed
		# sort(unique(data$MIN_FISH[!i]))
	data = data[i,]
		dim(data) # 167,800

#		hist(data$VESSEL_SPD, breaks=30)
#		range(data$VESSEL_SPD, na.rm=T)
	data$VESSEL_SPD[data$VESSEL_SPD==30] = 3 # fix typo according to Jeff Rester: 30 = 3	
	i = data$VESSEL_SPD < 5 & data$VESSEL_SPD > 0 
		i[is.na(i)] = FALSE
		sum(i) # 166,922
		sum(!i) # 878 to be trimmed (all are speed of 0)
		# unique(data$VESSEL_SPD[!i]) # all are NA
		# data[!i & !duplicated(data[,c('year', 'VESSEL', 'CRUISE_NO', 'P_STA_NO')]), c('year', 'VESSEL', 'CRUISE_NO', 'P_STA_NO', 'VESSEL_SPD')] # stations from 1984, 2004, 2005, 2006, 2007
	data = data[i,]
		dim(data) # 166,922
		# hist(data$VESSEL_SPD, breaks=30)
		
# Standardize count and weight
	area = data$VESSEL_SPD * 1.85200 * 1000 * data$MIN_FISH / 60 * data$GEAR_SIZE * 0.3048 # area trawled: knots * 1.8 km/hr/knot * 1000 m/km * minutes * 1 hr/60 min * feet * 0.3 m/ft
		round(range(area, na.rm=T)) # 2634 to 79,029 m2 per tow
		# hist(area)
	meanarea = mean(area)
		meanarea # 35,428 m2

	data$wtcpue = data$SELECT_BGS * meanarea/area
	data$numcpue = data$CNTEXP * meanarea/area
	
	
# Fix column names
	names(data)[names(data)=='VESSEL'] = 'svvessel'
	names(data)[names(data)=='CRUISE_NO'] = 'cruise'
	names(data)[names(data)=='P_STA_NO'] = 'station'
	names(data)[names(data)=='INVRECID'] = 'tow'
	names(data)[names(data)=='TEMP_SSURF'] = 'surftemp'
	names(data)[names(data)=='TEMP_BOT'] = 'bottemp'
	names(data)[names(data)=='TAXONOMIC'] = 'spp'
	names(data)[names(data)=='COMMON_NAM'] = 'common'
	names(data)[names(data)=='SALSURF'] = 'surfsal'
	names(data)[names(data)=='SALMAX'] = 'botsal'
	names(data)

# Check for non-standard NA codes and fix where needed
	summary(data$numcpue)
	summary(data$wtcpue)
	summary(data$surftemp)
		# hist(data$surftemp)
		data$surftemp[data$surftemp==0] = NA # 0 must mean missing data
	summary(data$bottemp)
		# hist(data$bottemp)
		data$bottemp[data$bottemp==0] = NA # 0 must mean missing data
	summary(data$surfsal)
	summary(data$botsal)

# Add blank columns that are missing in this region


# Add strata areas
	stratlatgrid = floor(data$lat)+0.5 # degree bins
	stratlongrid = floor(data$lon)+0.5 # degree bins
	R = 6371 # Earth's radius in km
	data$stratarea = (pi/180)*R^2 * abs(sin(floor(stratlatgrid)*pi/180) - sin(ceiling(stratlatgrid)*pi/180)) * abs(floor(stratlongrid)-ceiling(stratlongrid)) # from http://mathforum.org/library/drmath/view/63767.html
		range(data$stratarea)

# Remove records without lat/long?
	i = is.na(data$lon) | is.na(data$lat)
	sum(i) # should be 0

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
		length(dups) # 0
		#data = data[!duplicated(data),]
		dim(data) # 166,922

	# find tows that have same lat/lon but different haulid
	dups = which(duplicated(data[,c('year', 'lat', 'lon')]) & !duplicated(data$haulid))
		length(dups) # 13
		dupped = data[paste(data$year, data$lat, data$lon) %in% paste(data$year[dups], data$lat[dups], data$lon[dups]),]
			nrow(dupped) # 650
			sum(!duplicated(dupped$haulid)) # 26 (13 pairs of haulids)
		dupped = dupped[order(dupped$year, dupped$lat, dupped$lon),]
		# dupped[!duplicated(dupped$haulid), c('svvessel', 'year', 'lat', 'lon', 'haulid', 'month', 'day', 'time', 'COMSTAT')]
		# dupped[!duplicated(dupped$haulid), c('year', 'lat', 'lon', 'cruise', 'station', 'tow')]
		# dupped[, c('year', 'lat', 'lon', 'station', 'spp', 'wtcpue', 'numcpue')]
		keep = unique(dupped$haulid[grep('STARBOARD', dupped$COMSTAT)]) # remove the starboard haul (this is arbitrary, but seems to be right based on the notes associated with these hauls)
			keep = c(keep, '035-852-35003')
		rem = sort(unique(dupped$haulid[!(dupped$haulid %in% keep)])) # add a sequential trawl that came later
		length(rem) # 13
		sum(i<-!(data$haulid %in% rem)) # 166,583
	data = data[i,]
		dim(data) # 166,583
				
# Create list of all hauls
	goodhauls = data[!duplicated(data$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'month', 'day', 'julian', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhauls) # 6433
	# head(goodhauls)

# Adjust spp names for those cases where they've changed or where CODE match failed
	# tab = table(data$spp, data$year); write.csv(tab, 'sppbyyear.csv') # for checking by eye

	i = data$GENUS_BGS == 'PELAGIA' & data$SPEC_BGS == 'NOCTUL'
	data$spp[i] = 'PELAGIA NOCTILUCA'; data$common[i] = 'purplestriped jelly'; data$BIO_BGS[i] = 618030201
	i = data$GENUS_BGS == 'MURICAN' & data$SPEC_BGS == 'FULVEN'
	data$spp[i] = 'MURICANTHUS FULVESCENS'; data$common[i] = 'giant eastern murex'; data$BIO_BGS[i] = 308011501

	i = data$spp %in% c('APLYSIA BRASILIANA', 'APLYSIA WILLCOXI')
	data$spp[i] = 'APLYSIA'

	i = data$spp %in% c('AURELIA AURITA')
	data$spp[i] = 'AURELIA'

	i = data$spp %in% c('BOTHUS LUNATUS', 'BOTHUS OCELLATUS', 'BOTHUS ROBINSI')
	data$spp[i] = 'BOTHUS'

	i = data$spp %in% c('CLYPEASTER PROSTRATUS', 'CLYPEASTER RAVENELII')
	data$spp[i] = 'CLYPEASTER'

	i = data$spp %in% c('CONUS AUSTINI', 'CONUS STIMPSONI')
	data$spp[i] = 'CONUS'

	i = data$spp %in% c('CYNOSCION ARENARIUS', 'CYNOSCION NEBULOSUS', 'CYNOSCION NOTHUS')
	data$spp[i] = 'CYNOSCION'

	i = data$spp %in% c('ECHINASTER SENTUS', 'ECHINASTER SERPENTARIUS')
	data$spp[i] = 'ECHINASTER'

	i = data$spp %in% c('ECHINASTER SENTUS', 'ECHINASTER SERPENTARIUS')
	data$spp[i] = 'ECHINASTER'

	i = data$spp %in% c('OPISTOGNATHUS AURIFRONS', 'OPISTOGNATHUS LONCHURUS')
	data$spp[i] = 'OPISTOGNATHUS'

	i = data$spp %in% c('OPSANUS BETA', 'OPSANUS PARDUS', 'OPSANUS TAU')
	data$spp[i] = 'OPSANUS'

	i = data$spp %in% c('ROSSIA BULLISI')
	data$spp[i] = 'ROSSIA'

	i = data$spp %in% c('SOLENOCERA ATLANTIDIS', 'SOLENOCERA NECOPINA', 'SOLENOCERA VIOSCAI')
	data$spp[i] = 'SOLENOCERA'

	i = data$spp %in% c('TRACHYPENEUS CONSTRICTUS', 'TRACHYPENEUS SIMILIS')
	data$spp[i] = 'TRACHYPENEUS'

	# remove unidentified spp
	i = !(data$spp %in% c('UNID CRUSTA', 'UNID OTHER', 'UNID.FISH', 'CRUSTACEA(INFRAORDER) BRACHYURA', 'MOLLUSCA AND UNID.OTHER #01', 'ALGAE', 'MISCELLANEOUS INVERTEBR', 'OTHER INVERTEBRATES'))
		sum(i)
	data = data[i,]
		dim(data) # 166,524

	# Remove spp without a scientific name
	i = data$spp == '' | is.na(data$spp)
		data[i, c('BIO_BGS', 'GENUS_BGS', 'SPEC_BGS', 'spp', 'common', 'wtcpue')]
		sum(i) # 13
	data = data[!i,]
		dim(data) # 166,511

# Find duplicate tows (repeat rows for the same tow), and combine entries from spp that have now been consolidated
	# first sort by time so first index is always the earliest
	data = data[order(data$year, data$month, data$day, data$time, data$spp, data$haulid),]

	inds = which(duplicated(data[, c('spp', 'year', 'haulid')]))
		length(inds) # 2747
		# i = 1
		# data[data$spp %in% data$spp[inds[i]] & data$year %in% data$year[inds[i]] & data$haulid %in% data$haulid[inds[i]],]

	# inds2 = which(duplicated(data[, c('spp', 'year', 'haulid', 'BGSCODE')])) # 
	#	length(inds2) # 119: so most are duplicated because of differing BGSCODE
	#		i = 1
	#	data[data$spp %in% data$spp[inds2[i]] & data$year %in% data$year[inds2[i]] & data$haulid %in% data$haulid[inds2[i]],]

	#inds3 = which(duplicated(data[, c('spp', 'year', 'haulid', 'BGSID')])) # 
	#	length(inds3) # 0: so all are duplicated because of differing BGSID

	#	inds = which(duplicated(data[, c('spp', 'year', 'lat', 'lon')])) # use original spp names for finding true dupcliates
	# inds = which(duplicated(data[, c('spp', 'year', 'stratum', 'station')]))

	# write interim dataset
	write.csv(data, paste('Output/datainterim_', Sys.Date(), '.csv', sep=''))

	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	# Sum the entries: looks like the right thing based on visual inspection of the data (same haulids)
	data$sppyrhaulid = paste(data$spp, data$year, data$haulid, sep='-')
	nm = c("svvessel", "cruise", "tow", "stratum", "depth", "surftemp", "bottemp", "haulid", "year", "month", "day", "time", "julian", "lat", "lon", "common", "spp", "station", "surfsal", "botsal", "stratarea", "yearsurv", "juliansurv", "sppyrhaulid")

	newdata = aggregate(list(numcpue = data$numcpue, wtcpue = data$wtcpue), by=list(sppyrhaulid = data$sppyrhaulid), FUN=sumna)		
			dim(newdata) # 163,764
		newdata = merge(newdata, data[!duplicated(data[, c('spp', 'year', 'haulid')]),nm])
			dim(newdata) # 163,764

	# Check the work
	nrow(data) == nrow(newdata) + length(inds) # should be TRUE
	
	# Sort
	newdata = newdata[order(newdata$year, newdata$month, newdata$day, newdata$time, newdata$spp, newdata$haulid),]	
		
# How many tows?
	length(unique(paste(newdata$year, newdata$lat, newdata$lon))) # 6433 unique locations
	length(unique(newdata$haulid)) # 6433: good that this matches # locations	

# How many spp?
	length(unique(newdata$spp)) # 989

# Add a region column
newdata$region = "SEFSC_GOMex"

# How many hauls missing from data?
	setdiff(goodhauls$haulid, unique(newdata$haulid)) # lose no hauls

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	ncol(newdata)
	length(nm)
	setdiff(nm, names(newdata)) # missing from new data: 0 = good
	setdiff(names(newdata), nm) # to be removed from newdata

newdataout = newdata[,nm]
	dim(newdataout) # 163,764 x 26

# Write out
	write.csv(newdataout, paste('Output/data_', Sys.Date(), '.csv', sep=''))
	write.csv(goodhauls, paste('Output/goodhauls_', Sys.Date(), '.csv', sep=''))

#########################################	
## Trim to spp with data and add zeros ##
#########################################	
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/SEFSC Gulf')

data = read.csv('Output/data_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
goodhauls = read.csv('Output/goodhauls_2012-07-23.csv', row.names=1)

# Identify the spp to analyze
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	spplist = aggregate(list(count=data$numcpue, weight = data$wtcpue, pres = data$wtcpue>0), by=list(spp=data$spp, year=data$year), FUN=sumna)
	spplist = aggregate(list(count=spplist$count, weight=spplist$weight, pres = spplist$pres, presyr = spplist$pres>0), by=list(spp=spplist$spp), FUN=sum) # presyr col holds # years in which spp was present
	rownames(spplist) = 1:nrow(spplist)
	nrow(spplist) # 1989 spp
	
	sum(spplist$pres>0) # 988
	sum(spplist$pres>10) # 386
	max(spplist$presyr) # 28 yrs of data
	sum(spplist$presyr == max(spplist$presyr)) # 106 spp present every year

	spplist = spplist[spplist$presyr == max(spplist$presyr),] # take all spp present >= 1x per yr
		nrow(spplist) # 106 spp	

	spplist = merge(spplist, data[!duplicated(data$spp), c('common', 'spp')])
		dim(spplist)
		head(spplist)

# Remove spp not in spplist
	dim(data)
	data = data[data$spp %in% spplist$spp,]
	dim(data) # 136,776

# Any spp duplicated in the same haul?
	i = duplicated(paste(data$haulid, data$spp))
	sum(i) # 0 good

	j = data$haulid == data$haulid[i] & data$spp == data$spp[i]
	sum(j) # 0 good
	
	k = duplicated(data)
	sum(k) # 0 good

# Add any missing hauls
	inds = which(!(goodhauls$haulid %in% data$haulid))
	length(inds) # 36 missing

# Fill in zeros
	fin = length(unique(data$haulid))*length(unique(data$spp)) # expected final size
		fin # 678,082
	hauls = unique(data$haulid)
		length(hauls)
		nrow(goodhauls) # fewer hauls in data than in goodhauls
		
	# set up sets of unique hauls and spp
	newdata = data[!duplicated(data$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'yearsurv', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'juliansurv', 'day')]
		dim(newdata) # 6397
		newdata = newdata[order(newdata$haulid),] # sort by haulid
		rownames(newdata) = 1:nrow(newdata)
	spps = data[!duplicated(data$spp), c('spp', 'common')]
		dim(spps) # 106
		spps = spps[order(spps$spp),]
		rownames(spps) = 1:nrow(spps)
		
	# expand so that each spp can get a haul
	newdata = newdata[rep(rownames(newdata), rep(nrow(spps), nrow(newdata))),]
		dim(newdata) # 678,082
			
		# add spp info, replicated so that each haul gets a species
	newdata = cbind(newdata, spps[rep(rownames(spps), length(unique(newdata$haulid))),])
		dim(newdata) # 678,082
		
		# add catch info. set NAs to -9999 so they're not confused with an absence
		datana = data[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datana$numcpue[is.na(data$numcpue)] = -9999
		datana$wtcpue[is.na(data$wtcpue)] = -9999
	newdata = merge(newdata, datana[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdata) # 678,082
		summary(newdata$numcpue) # no -9999, so nothing missing
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


# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	dim(newdata)
	length(nm)
	setdiff(nm, names(newdata)) # should be 0
	setdiff(names(newdata), nm)

	newdata = newdata[,nm]

# Fix row names
	row.names(newdata) = 1:nrow(newdata)

# Write out
	write.csv(newdata, paste('Output/datatrimwzeros_', Sys.Date(), '.csv', sep=''))


###########################################
## Data Prep for Climate-envelope models ##
###########################################

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/SEFSC Gulf')

## Read in data
load('../HadISST 1.1/Output/hadisst_2012-07-02.RData') # load SST by month

data = read.csv('Output/datatrimwzeros_2012-07-23.csv', row.names=1)
	dim(data) # 678,082
	
## Figure out which names we need to use and trim the data
# only keep spp that are caught > 10 times/yr
	tab = aggregate(data$wtcpue>0, by=list(spp=data$spp, year = data$year), FUN=sum, na.rm=T)
	tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
	tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
	sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
	sppnames # These are spp to consider using: 47
	
	# Trim data to right spp
	datatrim = data[data$spp %in% sppnames,]
	dim(datatrim) # 300,659
			
## Add HadISST data (min and max temp)
	datatrim$mintemp = NA
	datatrim$mintempmnth = NA
	datatrim$maxtemp = NA
	datatrim$maxtempmnth = NA

	# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
	datatrim$latgrid = floor(datatrim$lat)+0.5
	datatrim$longgrid = floor(datatrim$lon)+0.5
	inds = which(!duplicated(datatrim[,c('latgrid', 'longgrid', 'year', 'month')]))
		length(inds) # 554 combinations to process
	for(i in 1:length(inds)){
		if(i %% 50 == 0) print(i)
		lat = as.character(datatrim$latgrid[inds[i]]) # to match hadisst grid
		long = as.character(datatrim$longgrid[inds[i]]) # to match hadisst grid
		yr = as.character(datatrim$year[inds[i]])
		lastyr = as.character(as.numeric(yr)-1)
		thesemons = as.character(1:datatrim$month[inds[i]]) # months in this year, converted to char
		lastmons = as.character((datatrim$month[inds[i]]+1):12) # months we want from last year

		j = datatrim$latgrid == datatrim$latgrid[inds[i]] & datatrim$longgrid == datatrim$longgrid[inds[i]] & datatrim$year == datatrim$year[inds[i]] & datatrim$month == datatrim$month[inds[i]]

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

# Add a date and a time column (rather than DATETIME)
datatrim$date = paste(formatC(datatrim$month, width=2, flag=0), '/', formatC(datatrim$day, width=2, flag=0), '/', datatrim$year, sep='')

# Transform data
	datatrim$lsurftemp = log(datatrim$surftemp+1)
	datatrim$lbottemp = log(datatrim$bottemp+1)
	datatrim$lmintemp = log(datatrim$mintemp+1)	
	
# Add pres/abs
	datatrim$pres = datatrim$numcpue>0 | (is.na(datatrim$numcpue) & !is.na(datatrim$wtcpue) & datatrim$wtcpue>0)

## Add mean biomass as a predictor
bm = aggregate(list(biomassmean=datatrim$wtcpue), by=list(year=datatrim$year, spp=datatrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm)
	dim(datatrim)

bm = aggregate(list(nummean=datatrim$numcpue), by=list(year=datatrim$year, spp=datatrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm, all.x=T)
	dim(datatrim) # 300,659

# Have a cpue that never goes to zero (useful for fitting log-links)
datatrim$wtcpuena = datatrim$wtcpue
datatrim$wtcpuena[datatrim$wtcpuena == 0] = 1e-4
datatrim$wtcpuenal = log(datatrim$wtcpuena)

datatrim$numcpuena = datatrim$numcpue
datatrim$numcpuena[datatrim$numcpuena == 0] = 1e-4
datatrim$numcpuenal = log(datatrim$numcpuena)

# Rename columns
names(datatrim)[names(datatrim)=='longgrid'] = 'longrid' # use the adjusted longitude


# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'tow', 'date', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'latgrid', 'longrid', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'mintemp', 'maxtemp', 'mintempmnth', 'maxtempmnth', 'lsurftemp', 'lbottemp', 'lmintemp', 'spp', 'biomassmean', 'nummean', 'wtcpue', 'numcpue', 'wtcpuena', 'numcpuena', 'wtcpuenal', 'numcpuenal', 'pres')
	dim(datatrim)
	length(nm)
	
	setdiff(nm, names(datatrim))
	setdiff(names(datatrim), nm) # will drop common and stratarea
	 	 
datatrim = datatrim[,nm]
	dim(datatrim) # 300,659 x 41


## Write out for this species
	write.csv(datatrim, file=paste('Output/dataCEM_', Sys.Date(), '.csv', sep=''))

