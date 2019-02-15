# Climate-envelope models

#############
## PREP #####
#############

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')

dataebs = read.csv('AFSC Eastern Bering Shelf/Output/dataCEM_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
	dim(dataebs)
	#summary(dataebs)
dataal = read.csv('AFSC Aleutians/Output/dataCEM_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
	dim(dataal)
	#summary(dataal)
datagoa = read.csv('AFSC Gulf of Alaska/Output/dataCEM_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
	dim(datagoa)
	#summary(datagoa)
datawc= read.csv('NWFSC/WCTriSurveysAllCoast77thru04/Output/dataCEM_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
	dim(datawc)
	#summary(datawc)
datagom = read.csv('SEFSC Gulf/Output/dataCEM_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
	dim(datagom)
	#summary(datagom)
dataec = read.csv('NEFSC/Output/datasprCEM_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
	dim(dataec)
	#summary(dataec)
datass = read.csv('DFO Scotian Shelf/Output/dataCEM_2012-11-15.csv', row.names=1, stringsAsFactors=FALSE)
	dim(datass)
	#summary(datass)
datasgsl = read.csv('DFO Southern Gulf/Output/dataCEM_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
	dim(datasgsl)
	#summary(datasgsl)
datanf = read.csv('DFO Newfoundland/Output/datafalCEM_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
	dim(datanf)
	#summary(datanf)

# Make list of columns to use
nm = names(dataebs)
nm = setdiff(nm, c('surfsal', 'botsal', 'date', 'lmintemp', 'lbottemp', 'lsurftemp', 'station', 'latgrid', 'longrid')) # remove some columns

# Make sure nothing missing
	setdiff(nm, names(dataal))
	setdiff(nm, names(datagoa))
	setdiff(nm, names(datawc))
	setdiff(nm, names(datagom))
	setdiff(nm, names(dataec))
	setdiff(nm, names(datass))
	setdiff(nm, names(datasgsl))
	setdiff(nm, names(datanf))

# Put all columns in the same order and merge
data = rbind(dataebs[,nm], dataal[,nm], datagoa[,nm], datawc[,nm], datagom[,nm], dataec[,nm], datass[,nm], datasgsl[,nm], datanf[,nm])
	dim(data) # 2,365,242 x 32

# Remove NA rows
	data = data[!is.na(data$wtcpue),] # removes 3 in NEFSC East Coast, 1 in DFO Scotian Shelf
	dim(data) # 2,365,238 x 32

	summary(data)
	unique(data$region)
	
	data$spp = as.factor(data$spp)
	data$region = as.factor(data$region)
	data$stratum = as.factor(data$stratum)
	data$svvessel = as.factor(data$svvessel)
	
	rm(dataebs, dataal, datagoa, datawc, datagom, dataec, datass, datasgsl, datanf)

# Add solar elevation
require(tripEstimation)
tm = as.POSIXct(paste(data$month, data$day, data$year, data$time), format='%m %d %Y %H:%M', tz='GMT')
data$solelev = elevation(data$lon, data$lat, solar(tm))

# Ensure no NAs in pres
i = which(is.na(data$pres))
	length(i) # 1
	data[i,c('region', 'spp', 'wtcpue', 'numcpue', 'pres')] # just GADUS MORHUA in DFO_So_Gulf
	
i = which(data$pres & data$wtcpue==0)	# many places where data$wtcpue == 0, but numcpue>0, so pres==TRUE
	length(i) # 8618
	summary(data$numcpue[i]) # all > 0 
	
	# reset pres
	i = which(is.na(data$pres))
	data$pres[i] = FALSE # because of this particular case, need to look carefully
		
nm = paste('Output/dataCEM_all_', Sys.Date(), '.csv', sep=''); nm
write.csv(data, nm)
	
##################
## Examine data ##
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
data = read.csv('Output/dataCEM_all_2012-11-16.csv', row.names=1)

length(unique(data$spp)) # 264 "species", but some are confounded by region because regions spell names differently (e.g., caps or not, spaces or not)
sum(!duplicated(data[,c('spp', 'region')])) # 334 spp-region combinations
sum(!duplicated(data[,c('region', 'haulid')])) # 57,819
length(unique(data$year)) # 44 years
sum(data$numcpue, na.rm=TRUE) # 75,291,142

# Examine data distributions
	hist(data$numcpue)
	table(data$expcatnum)
	pairs(~ numcpue + lsurftemp + lbottemp + mintemp + maxtemp, data=data)
	
	hist(data$wtcpue)
	hist((data$wtcpue)^(1/7))

	hist(data$surftemp)
		hist(log(data$surftemp))
	hist(data$bottemp)
		hist(log(data$bottemp))
	hist(data$mintemp)
		hist(log(data$mintemp))
	hist(data$maxtemp)
		hist(log(data$maxtemp))

# Pairs
	inds = runif(5000, 1, nrow(data))
	cols = c('red', 'green', 'blue', 'orange', 'purple', 'black')[(data$region[inds] == 'AFSC_EBS') + 2*(data$region[inds] == 'AFSC_AL') + 3*(data$region[inds] == 'AFSC_GOA') + 4*(data$region[inds] == 'WCTri') + 5*(data$region[inds] == 'NEFSCSpring') + 6*(data$region[inds] == 'DFO_SoGulf')]
	pairs(~ surftemp + bottemp + mintemp + maxtemp, data=data[inds,], col = cols) # pick 5000 random points


#############
### MODEL ###	
#############
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
source('allsubset.glm 2012-01-30.R')
source('revgrep 2012-02-07.R')

data = read.csv('Output/dataCEM_all_2012-11-16.csv', row.names=1)
# data = read.csv('NEFSC/Output/datasprCEM_Paralichthysdentatus_2014-02-10.csv', row.names=1)
    data$stratumfact = as.factor(data$stratum)
    data$yrfact = as.factor(data$year)
    data$sppregion = paste(data$spp, data$region, sep='_')

## Models
	# require(MASS)
	# require(pscl)
	require(mgcv)


## Pick the spp
spp=sort(unique(data$spp));spp # 133 spp
sppreg = sort(unique(data$sppregion))

	# Plot cpue vs. environmental variables
	par(mfrow=c(1,4))
	i = data$spp == spp[1]
	i = data$spp == spp[1] & data$pres
	plot(log(wtcpuena) ~ surftemp, data=data[i,])
	plot(log(wtcpuena) ~ bottemp, data=data[i,])
	plot(log(wtcpuena) ~ stratumfact, data=data[i,])
	plot(log(wtcpuena) ~ biomassmean, data=data[i,])
	
	plot(data$mintemp[i], data$wtcpue[i])
	plot(data$maxtemp[i], data$wtcpue[i])

## Pick the training years for each region
regs = sort(unique(data$region))
yrs = vector('list', length(regs)); names(yrs) = regs
for(i in 1:length(yrs)){
	yrs[[i]] = sort(unique(data$year[data$region == regs[i]]))
}

trainingyrs = yrs; traintype = 'allyrs' # all years
#for(i in 1:length(yrs)) trainingyrs[[i]] = yrs[[i]][1:round(length(yrs[[i]])/2)]; traintype = 'firsthalf'
#for(i in 1:length(yrs)) trainingyrs[[i]] = yrs[[i]][(round(length(yrs[[i]])/2)+1):length(yrs[[i]])]; traintype = '2ndhalf'

# Indicator for where present, but also has at least one TRUE value per spp per stratum within the training years, for fitting stratum effects
## OR: Skip this and read it in below
#	data$presfit = data$wtcpue > 0
#	presfitspp = character(0)
#	for(i in 1:length(regs)){
#		strat = sort(unique(data$stratum[data$region == regs[i]]))
#		thesespp = sort(unique(data$spp[data$region == regs[i]]))
#		for(j in 1:length(thesespp)){
#			for(k in 1:length(strat)){
#				inds = data$region == regs[i] & data$spp == thesespp[j] & data$stratum==strat[k] & data$year %in% trainingyrs[[i]] & complete.cases(data[,c('surftemp', 'bottemp')]) # also has to have complete data, otherwise useless for model fitting
#				if(sum(data$presfit[inds]) == 0){
#					print(paste(regs[i], thesespp[j],strat[k]))
#					m = sample(which(inds),1) # pick a random record in this stratum
#					data$presfit[m] = TRUE # set it to TRUE (even though wtcpue==0)
#					print(paste(data$presfit[m], data$wtcpue[m], data$wtcpuena[m], data$year[m]))
#					presfitspp = c(presfitspp, as.character(data$sppregion[m]))
#				}
#			}
#		}
#	}
#	sum(data$presfit & !data$pres)
#	presfitspp = unique(presfitspp); presfitspp
#
#	write.csv(data[,c('region', 'haulid', 'spp', 'presfit')], file=paste('Output/CEModels/datapresfit_', traintype, '_', Sys.Date(), '.csv', sep=''))

	# Read in presfit from before:
	datapresfitname = 'Output/CEModels 2012-08-05 allyrs/datapresfit_allyrs_2012-08-05.csv'
	# datapresfitname = 'Output/CEModels 2013-07-20 allyrs/datapresfit_allyrs_Paralichthysdentatus_2014-02-10.csv'
	presfit = read.csv(datapresfitname, row.names=1, stringsAsFactors=FALSE)
	dim(data)
	dim(presfit)
	all(presfit$haulid == data$haulid & presfit$spp == data$spp & presfit$region == data$region) # make sure ALL are in the same order
 data = cbind(data, presfit[, 'presfit'])
		dim(data) # should add one column
	

options(warn=0)
for(j in 1:length(regs)){
	print(j)
	spp=sort(unique(data$spp[data$region==regs[j]]))
	sppinds = 1:length(spp)
	
	for(i in sppinds){
		# Subset to training years
		if(!(regs[j] %in% c('DFO_Newfoundland_Spring', 'DFO_Newfoundland_Fall'))){
			#inds = data$yearsurv %in% trainingyrs & complete.cases(data[,c('surftemp', 'bottemp', 'mintemp', 'maxtemp')]) & data$spp == spp[i]
#			inds = data$yearsurv %in% trainingyrs[[j]] & complete.cases(data[,c('surftemp', 'bottemp', 'depth')]) & data$spp == spp[i] & data$region == regs[j]
			inds = data$yearsurv %in% trainingyrs[[j]] & complete.cases(data[,c('surftemp', 'bottemp')]) & data$spp == spp[i] & data$region == regs[j]
		} else {
#			inds = data$yearsurv %in% trainingyrs[[j]] & complete.cases(data[,c('bottemp', 'depth')]) & data$spp == spp[i] & data$region == regs[j] # no surftemp in Newfoundland
			inds = data$yearsurv %in% trainingyrs[[j]] & complete.cases(data[,c('bottemp')]) & data$spp == spp[i] & data$region == regs[j] # no surftemp in Newfoundland
		}
		
		print(paste(regs[j], ' ', spp[i], ': ', sum(data$pres[inds]), ' presences, ', sum(inds), ' indices in training set', sep=''))
		
		if(sum(inds)>300 & sum(data$pres[inds])>=40){
				
			# GAM gaussian
#			print('GAM gaussian')		
#			if(!(regs[j] %in% c('DFO_Newfoundland_Spring', 'DFO_Newfoundland_Fall'))){
##				gam2 = gam(wtcpue ~ s(surftemp) + s(bottemp) + s(depth) + stratumfact + biomassmean, family=gaussian, data=data[inds,], select=TRUE) # mgcv has a built-in model selection function
#				gam2 = gam(wtcpue ~ s(surftemp) + s(bottemp) + stratumfact + biomassmean, family=gaussian, data=data[inds,], select=TRUE) # mgcv has a built-in model selection function
#			} else {
##				gam2 = gam(wtcpue ~ s(bottemp) + s(depth) + stratumfact + biomassmean, family=gaussian, data=data[inds,], select=TRUE) # w/out surftemp for Newfoundland
#				gam2 = gam(wtcpue ~ s(bottemp) + stratumfact + biomassmean, family=gaussian, data=data[inds,], select=TRUE) # w/out surftemp for Newfoundland
#			
#			}
			#	summary(gam2)
			#	plot(gam2, se=TRUE, shade=TRUE, pages=1, all.terms=TRUE)
		
			# GAM Hurdle gamma (pres/abs and abundance)
			print('GAM presence binomial')	
			if(!(regs[j] %in% c('DFO_Newfoundland_Spring', 'DFO_Newfoundland_Fall'))){
#				gamhp2 = gam(pres ~ s(surftemp) + s(bottemp) + s(depth) + stratumfact + biomassmean, family=binomial, data=data[inds,], select=TRUE) # mgcv has a built-in model selection function
				gamhp2 = gam(pres ~ s(surftemp) + s(bottemp) + stratumfact + biomassmean, family=binomial, data=data[inds,], select=TRUE) # mgcv has a built-in model selection function
			} else {
#				gamhp2 = gam(pres ~ s(bottemp) + s(depth) + stratumfact + biomassmean, family=binomial, data=data[inds,], select=TRUE)
				gamhp2 = gam(pres ~ s(bottemp) + stratumfact + biomassmean, family=binomial, data=data[inds,], select=TRUE)
			}

			print('GAM abundance gaussian(log)')
			try3 = try(if(!(regs[j] %in% c('DFO_Newfoundland_Spring', 'DFO_Newfoundland_Fall'))){
#				gamha2 = gam(wtcpuenal ~ s(surftemp) + s(bottemp) + s(depth) + stratumfact + biomassmean, family=gaussian, data=data[inds & data$presfit,], select=TRUE, control=list(mgcv.half=30)) # mgcv has a built-in model selection function
				gamha2 = gam(wtcpuenal ~ s(surftemp) + s(bottemp) + stratumfact + biomassmean, family=gaussian, data=data[inds & data$presfit,], select=TRUE, control=list(mgcv.half=30)) # mgcv has a built-in model selection function
			} else {
#				gamha2 = gam(wtcpuenal ~ s(bottemp) + s(depth) + stratumfact + biomassmean, family=gaussian, data=data[inds & data$presfit,], select=TRUE) # mgcv has a built-in model selection function
				gamha2 = gam(wtcpuenal ~ s(bottemp) + stratumfact + biomassmean, family=gaussian, data=data[inds & data$presfit,], select=TRUE) # mgcv has a built-in model selection function
			}
			)
			
			# Save models if model fitting worked
			if(class(try3)[1] != "try-error"){
				# mods = list(gam2=gam2, gamhp2=gamhp2, gamha2 = gamha2) # without full models or glms
				mods = list(gamhp2=gamhp2, gamha2 = gamha2) # without full models or glms
				save(mods, inds, file=paste('Output/CEModels/CEmods_', regs[j], '_', spp[i], '_', traintype, '_', Sys.Date(), '.RData', sep=''))
			}
		}
	}
}

##########################################
######## Plots and basic analysis ########
##########################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
require(mgcv)
data = read.csv('Output/dataCEM_all_2012-11-16.csv', row.names=1)
# data = read.csv('NEFSC/Output/datasprCEM_Paralichthysdentatus_2014-02-10.csv', row.names=1)
    data$stratumfact = as.factor(data$stratum)
    data$yrfact = as.factor(data$year)
    data$sppregion = paste(data$spp, data$region, sep='_')
	spp=sort(unique(data$spp))
	regs = sort(unique(data$region))

	# Read in presfit from before:
	datapresfitname = 'Output/CEModels 2012-08-05 allyrs/datapresfit_allyrs_2012-08-05.csv'; traintype = 'allyrs'
	presfit = read.csv(datapresfitname, row.names=1)
		dim(data)
		dim(presfit)
		all(presfit$haulid == data$haulid & presfit$spp == data$spp & presfit$region == data$region) # make sure ALL are in the same order
	data = cbind(data, presfit[, 'presfit'])
		dim(data) # should add one column
		names(data)[grep('presfit', names(data))] = 'presfit' # fix column name if concatenated on strangely

	yrs = vector('list', length(regs)); names(yrs) = regs
	for(i in 1:length(yrs)){
		j = sort(unique(data$year[data$region == regs[i]]))
		if(regs[i] == 'DFO_SoGulf') j = j[j != 2008] # because no SST in 2008
		yrs[[i]] = j
	}

	# Write out for later use
	save(yrs, regs, spp, file = paste('Output/CEM_yrs_regs_spp', Sys.Date(), '.RData', sep=''))


## Plot GAM fits for all the abundance models (multipage pdf)
	folder = 'CEModels 2012-07-26 allyr'; trainingyrs = yrs; traintype = 'allyrs'; date='2012-07-25'; vars=c('surftemp', 'bottemp') # subset of 12 years
	# folder = 'CEModels 2012-02-17 halfinter biomassmean'; trainingyrs = yrs[seq(1,length(yrs), by=2)] # subset of 12 years
	#folder = 'CEModels 2012-02-18 half biomassmean'; trainingyrs = yrs[1:12]; vars=c('bottemp', 'surftemp') # subset of 12 years
	# folder = 'CEModels 2012-02-23 2ndhalf'; trainingyrs = yrs[13:length(yrs)]; date='2012-02-23'; vars=c('surftemp', 'bottemp') # subset of 12 years

	files = list.files(path=paste('Output/', folder, sep=''), pattern='.RData')
	pdf(width=6, height=6, file=paste('Figures/GAMabund_fits_', traintype, '_', Sys.Date(), '.pdf', sep='')) # only works if only one round of CEM models are in the directory
	for(i in 1:length(files)){
		print(files[i])
		load(file=paste('Output/',folder, '/', files[i], sep=''))
		thisspp = strsplit(files[i], split=paste('AFSC_Aleutians_|AFSC_EBS_|AFSC_GOA_|DFO_ScotianShelf_|DFO_SoGulf_|NEFSCSpring_|WestCoast_Tri_|DFO_Newfoundland_Fall_|SEFSC_GOMex_|_',traintype, sep=''))[[1]][2] # extract spp name from file name
		thisreg = strsplit(files[i], split=paste('CEmods_|_', thisspp, sep=''))[[1]][2]
		theseyrs = yrs[[thisreg]]
		inds = data$year %in% trainingyrs[[thisreg]] & complete.cases(data[,vars]) & data$spp == spp[i]
	
		plot(mods$gam2, shade=TRUE, se=TRUE, pages=1, main=paste(thisspp, '\n', thisreg), all.terms=FALSE)
		sum = summary(mods$gam2)#; print(sum)
		mtext(side=3, paste(signif(sum$dev.expl,2)*100, '% dev', sep=''))	
	}
		
	dev.off()

## Plot GAM Hurdle fits for all the abundance models (multipage pdf)
	folder = 'CEModels 2012-07-26 allyr'; trainingyrs = yrs; date='2012-07-25'; vars=c('surftemp', 'bottemp') # subset of 12 years
	#folder = 'CEModels 2012-03-14 firsthalf'; trainingyrs = yrs; date='2012-03-14'; vars = c('bottemp', 'surftemp');	for(i in 1:length(yrs)) trainingyrs[[i]] = yrs[[i]][1:round(length(yrs[[i]])/2)]; traintype = 'firsthalf' # first half of years

	files=list.files(paste('Output/', folder, sep=''), pattern='.RData') # only works if only one round of CEM models are in the directory
	regs = sort(unique(data$region))

	pdf(width=6, height=6, file=paste('Figures/GAMhurdle_fits_', traintype, '_', Sys.Date(), '.pdf', sep=''))
	for(i in 1:length(files)){
		print(files[i])
		load(file=paste('Output/',folder, '/', files[i], sep=''))
		thisspp = strsplit(files[i], split=paste('AFSC_Aleutians_|AFSC_EBS_|AFSC_GOA_|DFO_ScotianShelf_|DFO_SoGulf_|NEFSCSpring_|WestCoast_Tri_|DFO_Newfoundland_Fall_|SEFSC_GOMex_|_',traintype, sep=''))[[1]][2] # extract spp name from file name
		thisreg = strsplit(files[i], split=paste('CEmods_|_', thisspp, sep=''))[[1]][2]
		theseyrs = yrs[[thisreg]]

		inds = data$year %in% trainingyrs[[thisreg]] & complete.cases(data[,vars]) & data$spp == spp[i]
		
		plot(mods$gamhp2, shade=TRUE, se=TRUE, pages=1, main=paste(thisspp, '\n', thisreg, '\npresence'), all.terms=FALSE)
		sum = summary(mods$gamhp2)#; print(sum)
		mtext(side=3, paste(signif(sum$dev.expl,2)*100, '% dev', sep=''))

		# quartz()
		plot(mods$gamha2, shade=TRUE, se=TRUE, pages=1, main=paste(thisspp, '\n', thisreg, '\nabundance'), all.terms=FALSE)
		sum = summary(mods$gamha2)#; print(sum)	
		mtext(side=3, paste(signif(sum$dev.expl,2)*100, '% dev', sep=''))
		
		rm(mods, inds)
	
	}
	
	dev.off()


# Plot predictions and reality as maps
	require(maps)
	#yrtoplot = list(one=1968:1989, two= 1990:2011)
	spp = 'Hippoglossusstenolepis'; region = 'AFSC_EBS'; yrtoplot = list(one=c(1982, 1983), two= 2005); load('Output/CEModels 2012-03-14 firsthalf/CEmods_AFSC_EBS_Hippoglossusstenolepis_firsthalf_2012-03-14.RData'); xlims = c(-180,-155); ylims = c(52,63); db='world'
	
	spp = 'Homarus americanus'; region = 'NEFSCSpring'; yrtoplot = list(one=c(1968:1979), two=2001:2009); load('Output/CEModels 2012-03-14 firsthalf/CEmods_NEFSCSpring_Homarus americanus_firsthalf_2012-03-14.RData'); xlims = c(-77,-65); ylims = c(35,46); db='usa'; wid=3.5

	spp = 'Homarus americanus'; region = 'NEFSC_Spring'; yrtoplot = list(one=c(1970), two=2005); load('Output/CEModels 2012-09-05 allyrs onlytemp/CEmods_NEFSC_Spring_Homarus americanus_allyrs_2012-09-06.RData'); xlims = c(-77,-65); ylims = c(35,46); db='usa'; wid=3.5

	spp = 'Doryteuthis pealeii'; region = 'NEFSCSpring'; yrtoplot = list(one=c(1968:1979), two=2001:2009); load('Output/CEModels 2012-03-14 firsthalf/CEmods_NEFSCSpring_Doryteuthis pealeii_firsthalf_2012-03-14.RData'); xlims = c(-77,-65); ylims = c(35,46); db='usa'; wid=3.5

	spp = 'Merluccius bilinearis'; region = 'NEFSCSpring'; yrtoplot = list(one=c(1968:1979), two=2001:2009); load('Output/CEModels 2012-03-12 allyr/CEmods_NEFSCSpring_Merluccius bilinearis_allyrs_2012-03-12.RData'); xlims = c(-77,-65); ylims = c(35,46); db='usa'; wid=3.5

	spp = 'Merluccius bilinearis'; region = 'NEFSC_Spring'; yrtoplot = list(one=c(1968:2011), two=2001:2009); load('Output/CEModels 2012-09-05 allyrs onlytemp/CEmods_NEFSC_Spring_Merluccius bilinearis_allyrs_2012-09-06.RData'); xlims = c(-77,-65); ylims = c(35,46); db='usa'; wid=3.5

	spp = 'Clupeapallasi'; region = 'AFSC_GOA'; yrtoplot = list(one=c(1984, 1987, 1990, 1993,1996,1999), two= c(2003,2005, 2007, 2009, 2011)); load('Output/CEModels 2012-03-14 firsthalf/CEmods_AFSC_GOA_Clupeapallasi_firsthalf_2012-03-14.RData'); xlims = c(-170,-130); ylims = c(53,62); db='world'; wid=6
	

	inds = data$spp == spp & data$region == region & data$year %in% as.numeric(unlist(yrtoplot))
		sum(inds)
	c = data$wtcpue[inds] # to scale pt size by biomass
	a = max(log(c), na.rm=T)/2
	cols = rev(rainbow(n=ceiling(a)+1, start=0, end=4/6))
	col = rgb(10,255,0,150,maxColorValue=255)
	prescol='black'; abscol='grey'; prespch=16; abspch=4; abscex=0.4 # colors and pchs for presences and absences
	#prescol='black'; abscol='black'; prespch=16; abspch=16; abscex=0.1 # for all points the same
	#mapcol = 'light grey'
	mapcol = 'tan'

	mods2 = list(gamhurdle = list(mods$gamhp2, mods$gamha2))
	# plot(mods$gamhp2, shade=TRUE, residuals=TRUE, pages=1)
	# plot(mods$gamha2, shade=TRUE, residuals=TRUE, pages=1)


	quartz(width=min(12,length(yrtoplot)*wid), height=4)
	# pdf(paste('Figures/Maps_CEM_', spp, '_', region, '_', Sys.Date(), '.pdf', sep=''), width=min(12,length(yrtoplot)*wid), height=4)
	par(mfcol=c(length(mods2),length(yrtoplot)), mai=c(0.3, 0.3, 0.2, 0.1)) # use mfcol to fill by columns
	if(length(yrtoplot)==2) par(mai=c(0.5, 0.5, 0.3, 0.1)) # use mfcol to fill by columns
	for(i in 1:length(yrtoplot)){
		for(j in 1:length(mods2)){
			if(length(yrtoplot[[i]])==1) title = yrtoplot[[i]]
			if(length(yrtoplot[[i]])>1) title = paste(yrtoplot[[i]][1], 'to', max(yrtoplot[[i]]))			
			#map(database='state', regions=c('massachusetts', 'rhode island', 'maine', 'connecticut', 'new york', 'new jersey', 'delaware', 'new hampshire', 'vermont', 'pennsylvania', 'maryland'), fill=TRUE, col='grey', xlim=xlims, ylim=ylims, main=title)
			map(database=db, xlim=xlims, ylim=ylims, fill=TRUE, col=mapcol)
			map.axes()
			mtext(title, side=3)
			inds = data$year %in% yrtoplot[[i]] & data$spp == spp & complete.cases(data[,c('surftemp', 'bottemp')]) & data$region == region
			inds2 = inds & data$wtcpue > 0
			inds3 = inds & data$wtcpue == 0
			c = data$wtcpue[inds2]
			#c = rep(1.1, sum(inds)); a = 10 # for all pts small
			points(data$lon[inds3], data$lat[inds3], pch=abspch, cex=abscex, col = abscol) # plot absences
			points(data$lon[inds2], data$lat[inds2], pch=prespch, cex=ceiling(log(c))/a, col = prescol) # plot observed points
			meanlat = weighted.mean(data$lat[inds2],w= data$wtcpue[inds2])
			#points(min(xlims)+0.5, meanlat, cex=2, pch=1) # plot mean observed lat
			if(i==1) baselat = meanlat
			#abline(h=meanlat, lty=2, col='black')
			#abline(h=min(data$lat[inds2]), lty=1, col='black') # min and max lat
			#abline(h=max(data$lat[inds2]), lty=1, col='black')

			if(length(mods2[[j]]) == 1){
				preds = predict(mods2[[j]][[1]], newdata = data[inds,], type='response')
			}
			if(length(mods2[[j]]) == 2){
				preds1 = predict(mods2[[j]][[1]], newdata = data[inds,], type='response')
				preds2 = exp(predict(mods2[[j]][[2]], newdata = data[inds,], type='response'))
				preds = preds1*preds2
			}
			preds[preds<0] = 0
			#points(data$lon[inds][preds>0], data$lat[inds][preds>0], pch=16, xlab='', cex=ceiling(log(preds[preds>0]))/a, ylab='', col = col) # plot predictions
			meanpredlat = weighted.mean(data$lat[inds][!is.na(preds)], w = preds[!is.na(preds)])
			#points(min(xlims)+0.5, meanpredlat, cex=2, col=col, pch=1) # plot mean predicted lat
			#if(i==1) basepredlat = meanpredlat
			#abline(h=basepredlat, lty=2, col=col)
			#abline(h=min(data$lat[inds][preds>7e-1], na.rm=TRUE), lty=1, col=col) # min and max predicted lat
			#abline(h=max(data$lat[inds][preds>7e-1], na.rm=TRUE), lty=1, col=col)
		}
	}
	
	dev.off()
	
## Model diagnostics: calculate ROC and r2 for each model
	require(ROCR)
#	folder = 'CEModels 2012-07-26 allyr'; trainingyrs = yrs; date='2012-07-25'; vars=c('surftemp', 'bottemp'); traintype = 'allyr' # all years
	#folder = 'CEModels 2012-03-14 firsthalf'; trainingyrs = yrs; date='2012-03-14'; vars = c('bottemp', 'surftemp'); traintype = 'firsthalf' # first half of years
	#for(i in 1:length(yrs)) trainingyrs[[i]] = yrs[[i]][1:round(length(yrs[[i]])/2)]; traintype = 'firsthalf'
	folder = 'CEModels 2012-09-05 allyrs onlytemp'; trainingyrs = yrs; vars=c('surftemp', 'bottemp'); traintype='allyrs'; suff='onlytemp' # all years w/ only temp (surf, bot, strat, biomass)

	files=list.files(paste('Output/', folder, sep=''), pattern='.RData') # only works if only one round of CEM models are in the directory
	regs = sort(unique(data$region))
	sppregs = sort(unique(data$sppregion))
	n = rep(NA, length(sppregs))
	
	# save ROC and r2 for each taxon in each region
	modeldiaggamh = data.frame(sppregion = sppregs, region = n, spp = n, auc = n, r2.biomass = n, r2.all = n, dev.pres = n, dev.biomass = n, stringsAsFactors=FALSE)
		modeldiaggamh = modeldiaggamh[!grepl('PORIFERA', modeldiaggamh$sppregion),] # for some reason PORIFERA gets added, but I don't have a CEM
		
	options(warn=1) # print warnings as they occur
	fileinds = 1:length(files)
	#fileinds = 221:length(files)
	#fileinds = grep('Newfoundland', files)
	for(s in fileinds){
		print(s)
		load(paste('Output/',folder, '/', files[s], sep=''))
		thisspp = strsplit(files[s], split=paste('AFSC_Aleutians_|AFSC_EBS_|AFSC_GOA_|DFO_ScotianShelf_|DFO_SoGulf_|NEFSC_Spring_|WestCoast_Tri_|DFO_Newfoundland_Fall_|SEFSC_GOMex_|_',traintype, sep=''))[[1]][2] # extract spp name from file name
		#thisreg = strsplit(files[s], split=paste('CEmods_|_', thisspp, sep=''))[[1]][2]
		thisreg = regs[sapply(regs, FUN=grepl, x=files[s])]
		theseyrs = yrs[[thisreg]]
		j = intersect(grep(thisspp, modeldiaggamh$sppregion, fixed=TRUE), grep(thisreg, modeldiaggamh$sppregion))
		if(length(j)>1 | length(j)<1) stop('Found too few or too many indices into modeldiag dataframe')
		modeldiaggamh$spp[j] = thisspp
		modeldiaggamh$region[j] = as.character(thisreg)
	
		# Find observations and predictions
		inds = complete.cases(data[,vars]) & data$spp == thisspp & data$region == thisreg
		if(thisreg == 'DFO_Newfoundland_Fall') inds = complete.cases(data[,vars[vars!='surftemp']]) & data$spp == thisspp & data$region == thisreg

		if(sum(inds)>0){
			preds1 = predict(mods$gamhp2, newdata = data[inds,], type='response')
			preds2 = exp(predict(mods$gamha2, newdata = data[inds,], type='response')) # make sure to use exp if wtcpue was log-transformed!
			#plot(preds1, preds2, main=paste(spp[s], yrs[i]), xlab='Predicted presence', ylab='Predicted wtcpue')
			preds = preds1*preds2
			preds[preds<0] = 0

			# calculate performance (in part using ROCR)
			preds1.rocr = prediction(predictions=as.numeric(preds1), labels=data$pres[inds])
				# plot the ROC curve
#				perf = performance(preds1.rocr, 'tpr', 'fpr')
#				plot(perf)
			modeldiaggamh$auc[j] = performance(preds1.rocr, 'auc')@y.values[[1]]
			modeldiaggamh$r2.biomass[j] = cor(preds2[data$presfit[inds]], data$wtcpue[inds & data$presfit])^2 # correlation of biomass where present
			modeldiaggamh$r2.all[j] = cor(preds, data$wtcpue[inds])^2 # overall biomass correlation
			modeldiaggamh$dev.pres[j] = summary(mods$gamhp)$dev.expl
			modeldiaggamh$dev.biomass[j] = summary(mods$gamha)$dev.expl
		}
	}
	
	### Save model diagnostics
	write.csv(modeldiaggamh, file = paste('Output/modeldiaggamh_', traintype, suff, '_', Sys.Date(), '.csv', sep=''))

	## Examine
	modeldiaggamh = read.csv('Output/modeldiaggamh_allyrsonlytemp_2012-09-26.csv', row.names=1)
	dropspp = c('PARALEPIDIDAE', 'MYCTOPHIDAE', 'NOTACANTHIDAE', 'LIPARIDAE', 'Actiniaria', 'Nudibranchia', 'CEPHALOPODA', 'Porifera', 'PORIFERA') # spp that aren't even at the genus level
	modeldiaggamh2 = modeldiaggamh[!(modeldiaggamh$spp %in% dropspp),]
		dim(modeldiaggamh2) # 326


	summary(modeldiaggamh2$auc)
		hist(modeldiaggamh2$auc)
		modeldiaggamh2[modeldiaggamh2$auc<0.7, c('region', 'spp', 'auc')]
	summary(modeldiaggamh2$r2.biomass)
		hist(modeldiaggamh2$r2.biomass)
	summary(modeldiaggamh2$r2.all)
		hist(modeldiaggamh2$r2.all)
	summary(modeldiaggamh2$dev.pres)
		hist(modeldiaggamh2$dev.pres)
		modeldiaggamh2[modeldiaggamh2$dev.pres<0.1, c('region', 'spp', 'dev.pres')]
	summary(modeldiaggamh2$dev.biomass)
		hist(modeldiaggamh2$dev.biomass)
		modeldiaggamh2[modeldiaggamh2$dev.biomass<0.1, c('region', 'spp', 'dev.biomass')]


## Calculate mean lat/lon/depth by year in observations and in models
	#folder = 'CEModels 2012-07-26 allyr'; trainingyrs = yrs; date='2012-07-25'; vars=c('surftemp', 'bottemp'); traintype = 'allyr' # all years
	#folder = 'CEModels 2012-03-14 firsthalf'; trainingyrs = yrs; date='2012-03-14'; vars = c('bottemp', 'surftemp'); traintype = 'firsthalf' # first half of years
	#for(i in 1:length(yrs)) trainingyrs[[i]] = yrs[[i]][1:round(length(yrs[[i]])/2)]; traintype = 'firsthalf'
	folder = 'CEModels 2012-09-05 allyrs onlytemp'; trainingyrs = yrs; vars=c('surftemp', 'bottemp'); traintype = 'allyrs'; suff='onlytemp' # all years, stripped to only temp, stratum, biomass
	folder = 'CEModels 2013-07-20 allyrs onlytemp'; trainingyrs = yrs; vars=c('surftemp', 'bottemp'); traintype = 'allyrs'; suff='onlytemp' # all years, stripped to only temp, stratum, biomass
	
	files=list.files(paste('Output/', folder, sep=''), pattern='.RData') # only works if only one round of CEM models are in the directory
	regs = sort(unique(data$region))
	
	# save observed and predicted positions for lat and long
	meanpos = list(0)

	# save smearing estimators for re-transformation bias
	smear = numeric(length(files))

	options(warn=1) # print warnins as they occur
	for(s in 1:length(files)){
		print(s)
		load(paste('Output/',folder, '/', files[s], sep=''))
		thisspp = strsplit(files[s], split=paste('AFSC_Aleutians_|AFSC_EBS_|AFSC_GOA_|DFO_ScotianShelf_|DFO_SoGulf_|NEFSC_Spring_|WestCoast_Tri_|DFO_Newfoundland_Fall_|SEFSC_GOMex_|_',traintype, sep=''))[[1]][2] # extract spp name from file name
		#thisreg = strsplit(files[s], split=paste('CEmods_|_', thisspp, sep=''))[[1]][2]
		thisreg = regs[sapply(regs, FUN=grepl, x=files[s])]
		theseyrs = yrs[[thisreg]]

		#thesemods = list(glm = list(mods$glm2), glmh = list(mods$glmhp2), gam = list(mods$gam2), gamh = list(mods$gamhp2, mods$gamha2))
		#thesemods = list(gam = list(mods$gam2), gamh = list(mods$gamhp2, mods$gamha2))
		thesemods = list(gamh = list(mods$gamhp2, mods$gamha2))
		meanlat = list(rep(NA, length(yrs)))
		meanlong = list(rep(NA, length(yrs)))
		meandepth = list(rep(NA, length(yrs)))
		for(i in 1:(length(thesemods))) meanlat[[i+2]]=rep(NA,length(yrs))
		for(i in 1:(length(thesemods))) meanlong[[i+2]]=rep(NA,length(yrs))
		for(i in 1:(length(thesemods))) meandepth[[i+2]]=rep(NA,length(yrs))
		#names(meanlat) = c('obs', 'glm', 'glm pres', 'gam', 'gam hurdle')
		#names(meanlat) = c('obs', 'gam', 'gamhurdle')
		names(meanlat) = c('obs', 'null', 'gamhurdle')
		names(meanlong) = names(meanlat)
		names(meandepth) = names(meanlat)

		# Calc a number to weight conversion if needed, forced through 0,0
		inds = data$spp == thisspp & data$region == thisreg
		n2w = lm(wtcpue ~ numcpue - 1, data[inds,])
	
		# smearing estimator for re-transformation bias (see Duan 1983, http://www.herc.research.va.gov/resources/faq_e02.asp) in the gamh abundance model
		#smear[s] = mean(exp(thesemods[[2]][[2]]$residuals))
		smear[s] = mean(exp(thesemods[[1]][[2]]$residuals))

		# Calculate mean lat and long by year for observed and for each model
		for(i in 1:length(theseyrs)){
			inds = data$year == theseyrs[i] & complete.cases(data[,vars]) & data$spp == thisspp & data$region == thisreg
			indsallyrs = complete.cases(data[,vars]) & data$spp == thisspp & data$region == thisreg
			if(thisreg == 'DFO_Newfoundland_Fall') inds = data$year == theseyrs[i] & complete.cases(data[,setdiff(vars, 'surftemp')]) & data$spp == thisspp & data$region == thisreg

			if(sum(inds)>0){
				meanlat[[1]][i] = weighted.mean(data$lat[inds],w= data$wtcpue[inds], na.rm=T)
				meanlong[[1]][i] = weighted.mean(data$lon[inds],w= data$wtcpue[inds], na.rm=T)
				meandepth[[1]][i] = weighted.mean(data$depth[inds],w= data$wtcpue[inds], na.rm=T)
				
				if(sum(data$wtcpue[inds]) == 0){ # if wtcpue is all 0, predict from numcpue
					w = predict(n2w, newdata = data[inds,])
					meanlat[[1]][i] = weighted.mean(data$lat[inds],w= w, na.rm=T)
					meanlong[[1]][i] = weighted.mean(data$lon[inds],w= w, na.rm=T)	
					meandepth[[1]][i] = weighted.mean(data$depth[inds],w= w, na.rm=T)	
				}
		
				# null model: use ave temperature in all years
				preds1 = predict(thesemods[[j]][[1]], newdata = data.frame(stratumfact = data$stratumfact[inds], biomassmean = data$biomassmean[inds], surftemp = mean(data$surftemp[indsallyrs]), bottemp = mean(data$bottemp[indsallyrs])), type='response')
				preds2 = exp(predict(thesemods[[j]][[2]], newdata = data.frame(stratumfact = data$stratumfact[inds], biomassmean = data$biomassmean[inds], surftemp = mean(data$surftemp[indsallyrs]), bottemp = mean(data$bottemp[indsallyrs])), type='response')) # make sure to use exp if wtcpue was log-transformed!
				preds2 = preds2*smear[s]
				#plot(preds1, preds2, main=paste(spp[s], yrs[i]), xlab='Predicted presence', ylab='Predicted wtcpue')
				preds = preds1*preds2
				preds[preds<0] = 0
				meanlat[[2]][i] = weighted.mean(data$lat[inds][!is.na(preds)], w = preds[!is.na(preds)])		
				meanlong[[2]][i] = weighted.mean(data$lon[inds][!is.na(preds)], w = preds[!is.na(preds)])		
				meandepth[[2]][i] = weighted.mean(data$depth[inds][!is.na(preds)], w = preds[!is.na(preds)])		
				

				for(j in 1:length(thesemods)){
					if(length(thesemods[[j]]) == 1){
						preds = predict(thesemods[[j]][[1]], newdata = data[inds,], type='response')
					}
					if(length(thesemods[[j]]) == 2){ # if a two-stage model (e.g., hurdle)
						preds1 = predict(thesemods[[j]][[1]], newdata = data[inds,], type='response')
						preds2 = exp(predict(thesemods[[j]][[2]], newdata = data[inds,], type='response')) # make sure to use exp if wtcpue was log-transformed!
						preds2 = preds2*smear[s]
						#plot(preds1, preds2, main=paste(spp[s], yrs[i]), xlab='Predicted presence', ylab='Predicted wtcpue')
						preds = preds1*preds2
					}
					preds[preds<0] = 0
					meanlat[[j+2]][i] = weighted.mean(data$lat[inds][!is.na(preds)], w = preds[!is.na(preds)])		
					meanlong[[j+2]][i] = weighted.mean(data$lon[inds][!is.na(preds)], w = preds[!is.na(preds)])		
					meandepth[[j+2]][i] = weighted.mean(data$depth[inds][!is.na(preds)], w = preds[!is.na(preds)])		
				}
			}
		}
		
		meanpos[[s]] = list(meanlat = meanlat, meanlong = meanlong, meandepth = meandepth, yrs = theseyrs)
		names(meanpos)[s] = paste(thisreg, thisspp, sep='.')
	}
	
	# Examine smearing estimators
	summary(smear)
	hist(smear)

	### Save meanpos
	save(meanpos, smear, file = paste('Output/meanpos_', traintype, suff, '_', Sys.Date(), '.RData', sep=''))


# Plot predictions and reality as graphs of mean lat over time for all species
### Also calculate trends in obs and predicted
## If trainingyrs is not the full set of years, then calc trends separately for training and non-training years (second is for evaluation)
	setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
	load('Output/CEM_yrs_regs_spp2012-08-13.RData')
	folder = 'CEModels 2012-09-05 allyrs onlytemp'; load('Output/meanpos_allyrsonlytemp_2012-11-17.RData'); trainingyrs = yrs; ntrainingyrs = yrs; traintype = 'allyrsonlytemp' # all years with only temperature

	sppreg=names(meanpos)
	len = length(sppreg)
	a = numeric(len)

	# save observed and predicted slopes for lat and long
	#direction = data.frame(region = character(len), spp=character(len), obslat1 = a, gamlat1=a, gamhlat1=a, obslong1=a, gamlong1=a, gamhlong1=a, obsdepth1=a, gamdepth1=a, gamhdepth1=a, obslat2 = a, gamlat2=a, gamhlat2=a, obslong2=a, gamlong2=a, gamhlong2=a, obsdepth2=a, gamdepth2=a, gamhdepth2=a, stringsAsFactors=FALSE) # no glms
	direction = data.frame(region = character(len), spp=character(len), obslat1 = a, gamhlat1=a, obslong1=a, gamhlong1=a, obsdepth1=a, gamhdepth1=a, obslat2 = a, gamhlat2=a, obslong2=a, gamhlong2=a, obsdepth2=a, gamhdepth2=a, stringsAsFactors=FALSE) # no glms

	quartz(width=7.5, height=7)
	# pdf(paste('Figures/Meanlat&long_CEM_', traintype, '_', Sys.Date(), '.pdf', sep=''), width=7.5, height=7)
	par(mfrow=c(4,3), mai=c(0.4,0.5,0.2,0.1), mgp = c(2,1,0))	
	# quartz(width=7.5, height=2)
	# par(mfrow=c(1,3), mai=c(0.4,0.5,0.2,0.1), mgp = c(2,1,0))	
	cexpt = 0.6; cexmain = 0.6
	lwdobs = 0.7; lwdtrend = 3; ltytrend=1; typeobs = 'l'
	cols = c('black', 'blue', 'green', 'red', rgb(10,255,0,150,maxColorValue=255))
	for(s in 1:length(meanpos)){
		nm = strsplit(names(meanpos)[s], split='.', fixed=TRUE)[[1]]
		thisspp=nm[2] # extract spp name from file name
		thisreg = nm[1]

		direction$spp[s] = thisspp; thisspp
		direction$region[s] = thisreg; thisreg
		
		thesetrainingyrs = trainingyrs[[thisreg]]
		thesentrainingyrs = ntrainingyrs[[thisreg]]

		theseyrs = c(thesetrainingyrs, thesentrainingyrs)
		i = order(theseyrs)
		ylims = range(unlist(lapply(meanpos[[s]]$meanlat, FUN=range, na.rm=T)))*c(0.99,1.01)
		y1a = meanpos[[s]]$meanlat$obs[meanpos[[s]]$yrs %in% thesetrainingyrs]
			plot(theseyrs[i], y1a[i], type=typeobs, pch=16, xlab='Year', ylab = 'Mean latitude', ylim=ylims, main=sppreg[s], col=cols[1], cex=cexpt, cex.main=cexmain, lwd=lwdobs)
			lines(thesetrainingyrs, fitted(t1<-lm(y1 ~ thesetrainingyrs, na.action=na.exclude)), col=cols[1], lty=ltytrend, lwd=lwdtrend)
			direction$obslat1[s] = coef(t1)[2]
			#abline(h = mean(c(y1,y2), na.rm=T), col='grey', lty=1)
		y1b = meanpos[[s]]$meanlat$gamhurdle[meanpos[[s]]$yrs %in% thesetrainingyrs]
			points(theseyrs[i], y1b[i], type=typeobs, pch=16, col=cols[5], cex=cexpt, lwd=lwdobs)
			lines(thesetrainingyrs, fitted(t1<-lm(y1 ~ thesetrainingyrs, na.action=na.exclude)), col=cols[5], lty=ltytrend, lwd=lwdtrend)
			direction$gamhlat1[s] = coef(t1)[2]
#		y1c = meanpos[[s]]$meanlat$null[meanpos[[s]]$yrs %in% thesetrainingyrs]
#			points(theseyrs[i], y1c[i], type=typeobs, pch=16, col='grey', cex=cexpt, lwd=lwdobs)


		#legend('topleft', legend=c('Observed', 'Poisson GLM', 'Hurdle GLM', 'GAM', 'Hurdle GAM'), col= cols, pch=16, lty=1, bty='n')


		
		ylims = range(unlist(lapply(meanpos[[s]]$meanlong, FUN=range, na.rm=T)))*c(1.01,0.99)
		y1 = meanpos[[s]]$meanlong$obs[meanpos[[s]]$yrs %in% thesetrainingyrs]
		y2 = meanpos[[s]]$meanlong$obs[meanpos[[s]]$yrs %in% thesentrainingyrs]
		plot(theseyrs[i], c(y1,y2)[i], type=typeobs, pch=16, xlab='Year', ylab = 'Mean longitude', ylim=ylims, main=sppreg[s], col=cols[1], cex=cexpt, cex.main=cexmain, lwd=lwdobs)
			lines(thesetrainingyrs, fitted(t1<-lm(y1 ~ thesetrainingyrs, na.action=na.exclude)), col=cols[1], lty=ltytrend, lwd=lwdtrend)
			lines(thesentrainingyrs, fitted(t2<-lm(y2 ~ thesentrainingyrs, na.action=na.exclude)), col=cols[1], lty=ltytrend, lwd=lwdtrend)
			direction$obslong1[s] = coef(t1)[2]
			direction$obslong2[s] = coef(t2)[2]
			abline(h = mean(c(y1,y2), na.rm=T), col='grey', lty=1)
#		y1 = meanpos[[s]]$meanlong$gam[meanpos[[s]]$yrs %in% thesetrainingyrs]
#		y2 = meanpos[[s]]$meanlong$gam[meanpos[[s]]$yrs %in% thesentrainingyrs]
#		points(theseyrs[i], c(y1,y2)[i], type=typeobs, pch=16, col=cols[4], cex=cexpt, lwd=lwdobs)
#			lines(thesetrainingyrs, fitted(t1<-lm(y1 ~ thesetrainingyrs, na.action=na.exclude)), col=cols[4], lty=ltytrend, lwd=lwdtrend)
#			lines(thesentrainingyrs, fitted(t2<-lm(y2 ~ thesentrainingyrs, na.action=na.exclude)), col=cols[4], lty=ltytrend, lwd=lwdtrend)
#			direction$gamlong1[s] = coef(t1)[2]
#			direction$gamlong2[s] = coef(t2)[2]
		y1 = meanpos[[s]]$meanlong$gamhurdle[meanpos[[s]]$yrs %in% thesetrainingyrs]
		y2 = meanpos[[s]]$meanlong$gamhurdle[meanpos[[s]]$yrs %in% thesentrainingyrs]
		points(theseyrs[i], c(y1,y2)[i], type=typeobs, pch=16, col=cols[5], cex=cexpt, lwd=lwdobs)
			lines(thesetrainingyrs, fitted(t1<-lm(y1 ~ thesetrainingyrs, na.action=na.exclude)), col=cols[5], lty=ltytrend, lwd=lwdtrend)
			lines(thesentrainingyrs, fitted(t2<-lm(y2 ~ thesentrainingyrs, na.action=na.exclude)), col=cols[5], lty=ltytrend, lwd=lwdtrend)
			direction$gamhlong1[s] = coef(t1)[2]
			direction$gamhlong2[s] = coef(t2)[2]

		ylims = range(unlist(lapply(meanpos[[s]]$meandepth, FUN=range, na.rm=T)))*c(1.01,0.99)
		y1 = meanpos[[s]]$meandepth$obs[meanpos[[s]]$yrs %in% thesetrainingyrs]
		y2 = meanpos[[s]]$meandepth$obs[meanpos[[s]]$yrs %in% thesentrainingyrs]
		plot(theseyrs[i], c(y1,y2)[i], type=typeobs, pch=16, xlab='Year', ylab = 'Mean Depth', ylim=ylims, main=sppreg[s], col=cols[1], cex=cexpt, cex.main=cexmain, lwd=lwdobs)
			lines(thesetrainingyrs, fitted(t1<-lm(y1 ~ thesetrainingyrs, na.action=na.exclude)), col=cols[1], lty=ltytrend, lwd=lwdtrend)
			lines(thesentrainingyrs, fitted(t2<-lm(y2 ~ thesentrainingyrs, na.action=na.exclude)), col=cols[1], lty=ltytrend, lwd=lwdtrend)
			direction$obsdepth1[s] = coef(t1)[2]
			direction$obsdepth2[s] = coef(t2)[2]
			abline(h = mean(c(y1,y2), na.rm=T), col='grey', lty=1)
#		y1 = meanpos[[s]]$meandepth$gam[meanpos[[s]]$yrs %in% thesetrainingyrs]
#		y2 = meanpos[[s]]$meandepth$gam[meanpos[[s]]$yrs %in% thesentrainingyrs]
#		points(theseyrs[i], c(y1,y2)[i], type=typeobs, pch=16, col=cols[4], cex=cexpt, lwd=lwdobs)
#			lines(thesetrainingyrs, fitted(t1<-lm(y1 ~ thesetrainingyrs, na.action=na.exclude)), col=cols[4], lty=ltytrend, lwd=lwdtrend)
#			lines(thesentrainingyrs, fitted(t2<-lm(y2 ~ thesentrainingyrs, na.action=na.exclude)), col=cols[4], lty=ltytrend, lwd=lwdtrend)
#			direction$gamdepth1[s] = coef(t1)[2]
#			direction$gamdepth[s] = coef(t2)[2]
		y1 = meanpos[[s]]$meandepth$gamhurdle[meanpos[[s]]$yrs %in% thesetrainingyrs]
		y2 = meanpos[[s]]$meandepth$gamhurdle[meanpos[[s]]$yrs %in% thesentrainingyrs]
		points(theseyrs[i], c(y1,y2)[i], type=typeobs, pch=16, col=cols[5], cex=cexpt, lwd=lwdobs)
			lines(thesetrainingyrs, fitted(t1<-lm(y1 ~ thesetrainingyrs, na.action=na.exclude)), col=cols[5], lty=ltytrend, lwd=lwdtrend)
			lines(thesentrainingyrs, fitted(t2<-lm(y2 ~ thesentrainingyrs, na.action=na.exclude)), col=cols[5], lty=ltytrend, lwd=lwdtrend)
			direction$gamhdepth1[s] = coef(t1)[2]
			direction$gamhdepth2[s] = coef(t2)[2]

	}
	
	plot(0,0, bty='n', col='white', xlab='', ylab='', xaxt='n', yaxt='n')
	legend('center', legend=c('Observed', 'GAM', 'Hurdle GAM'), col= cols[c(1,4,5)], pch=16, lty=1, bty='n')

	dev.off()
	
	# Drop species not resolved to genus (would be better to do this before meanpos is calculated)
	spppres1 = read.csv('Output/sppres1_2012-11-21.csv', row.names=1) # which taxa to use
	dropspp = sort(c('PARALEPIDIDAE', 'MYCTOPHIDAE', 'NOTACANTHIDAE', 'LIPARIDAE', 'Actiniaria', 'Nudibranchia', 'CEPHALOPODA', 'Porifera')) # spp that aren't even at the genus level
	dropspp2 = sort(unique((direction$spp[!(direction$name %in% spppres1$name[spppres1$use])]))) # another way to ID species to drop
		all(dropspp == dropspp2) # the same, good
	direction = direction[!(direction$spp %in% dropspp),]
		dim(direction) # 325 x 21
	
	# Add a consistent scientific name
	spptax = read.csv('Life history/data/spptaxonomy_2012-08-12 manual.csv', row.names=1, stringsAsFactors=FALSE) # matches taxon name to taxonomy
	dim(direction)
	direction = merge(direction, spptax[,c('taxon', 'name')], by.x='spp', by.y='taxon') # add name column so that we have a correct species name
		dim(direction) 325 x 22
	direction = direction[,c(ncol(direction), 1:(ncol(direction)-1))] # put name column in the first position
	direction = direction[order(direction$region, direction$name),] # sort

	### Save direction
	write.csv(direction, file=paste('Output/direction_', traintype, '_', Sys.Date(), '.csv', sep=''))


## Bootstrapped calculations of trends through time in mean lat	and depth
	nboot = 1000
	regs = sort(unique(data$region))
	sppregs = sort(unique(data$sppregion))
		length(sppregs) # 336
	vars=c('surftemp', 'bottemp'); traintype = 'allyrsonlytemp' # to mimic behavior of allyears onlytemp models
	len= length(sppregs)
	directionlat_boot = data.frame(sppreg = sppregs, region = character(len), spp=character(len), btrue = NA, stringsAsFactors=FALSE) # to hold bootstrapped trend values in lat
		for(i in 1:nboot) directionlat_boot[[paste('b', i, sep='')]] = NA # add a column for each bootstrapped trend value
	directiondepth_boot = data.frame(sppreg = sppregs, region = character(len), spp=character(len), btrue = NA, stringsAsFactors=FALSE) # to hold bootstrapped trend values in lat
		for(i in 1:nboot) directiondepth_boot[[paste('b', i, sep='')]] = NA # add a column for each bootstrapped trend value

	options(warn=1) # print warnings as they occur
	for(s in 1:length(sppregs)){
		print(s)
		thisspp = as.character(unique(data$spp[data$sppregion == sppregs[s]]))
		thisreg = as.character(unique(data$region[data$sppregion == sppregs[s]]))
		theseyrs = yrs[[thisreg]]
		directionlat_boot$spp[s] = thisspp
		directionlat_boot$region[s] = thisreg
		directiondepth_boot$spp[s] = thisspp
		directiondepth_boot$region[s] = thisreg

		# Calc a number to weight conversion if needed, forced through 0,0
		inds = data$spp == thisspp & data$region == thisreg
		n2w = lm(wtcpue ~ numcpue - 1, data[inds,])

		inds = complete.cases(data[,vars]) & data$spp == thisspp & data$region == thisreg  & data$year %in% theseyrs
		if(thisreg == 'DFO_Newfoundland_Fall') inds = complete.cases(data[,setdiff(vars, 'surftemp')]) & data$spp == thisspp & data$region == thisreg  & data$year %in% theseyrs

		# Calculate mean lat by year and trend for observed and for each bootstrap
		if(sum(inds)>0){
			thesedata = data[inds, c('year', 'lat', 'depth', 'wtcpue')] # set up data.frame for analysis of mean lat

			if(sum(thesedata$wtcpue) == 0) thesedata$wtcpue = predict(n2w, newdata = data[inds,]) # if wtcpue are all 0, predict from numcpue
			cpues = thesedata$wtcpue # all biomass measurements for this taxon
			
			meanlats = summarize(thesedata[, c('lat', 'wtcpue')], by = list(year = thesedata$year), FUN = wgtmean, na.rm=TRUE) # Center of biomass lat from individual tows, for each year
				names(meanlats)[2] = 'meanlat'
			meandepths = summarize(thesedata[, c('depth', 'wtcpue')], by = list(year = thesedata$year), FUN = wgtmean, na.rm=TRUE) # Center of biomass lat from individual tows, for each year
				names(meandepths)[2] = 'meandepth'

			t1<-lm(meanlat ~ year, data=meanlats, na.action=na.exclude) # Calculate observed trend
			t2<-lm(meandepth ~ year, data=meandepths, na.action=na.exclude) # Calculate observed trend
			directionlat_boot$btrue[s] = coef(t1)[2] # store
			directiondepth_boot$btrue[s] = coef(t2)[2] # store
	
			# Bootstrapped meanlats and trends
			for(j in 1:nboot){
				thesedata$wtcpue = sample(x=cpues, size=nrow(thesedata), replace=TRUE) # sample with replacement to generate a bootstrapped version of wtcpue
				meanlatboot = summarize(thesedata[, c('lat', 'wtcpue')], by = list(year = thesedata$year), FUN = wgtmean, na.rm=TRUE) # Center of biomass lat, by individual tows
					names(meanlatboot)[2] = 'meanlat'
				meandepthboot = summarize(thesedata[, c('depth', 'wtcpue')], by = list(year = thesedata$year), FUN = wgtmean, na.rm=TRUE) # Center of biomass lat, by individual tows
					names(meandepthboot)[2] = 'meandepth'

				# Calculate bootstrapped trend
				tboot1 = lm(meanlat ~ year, data=meanlatboot, na.action=na.exclude)
				tboot2 = lm(meandepth ~ year, data=meandepthboot, na.action=na.exclude)
				directionlat_boot[[paste('b', j, sep='')]][s] = coef(tboot1)[2]
				directiondepth_boot[[paste('b', j, sep='')]][s] = coef(tboot2)[2]
			}
		}
	}
	
	# Add a consistent scientific name
	spptax = read.csv('Life history/data/spptaxonomy_2012-08-12 manual.csv', row.names=1, stringsAsFactors=FALSE) # matches taxon name to taxonomy
	dim(directionlat_boot)
	dim(directiondepth_boot)
	directionlat_boot = merge(directionlat_boot, spptax[,c('taxon', 'name')], by.x='spp', by.y='taxon') # add name column so that we have a correct species name
	directiondepth_boot = merge(directiondepth_boot, spptax[,c('taxon', 'name')], by.x='spp', by.y='taxon') # add name column so that we have a correct species name
		dim(directionlat_boot)
		dim(directiondepth_boot)
	directionlat_boot = directionlat_boot[,c(1005, 1:1004)] # put name column in the first position
	directiondepth_boot = directiondepth_boot[,c(1005, 1:1004)]

	# Drop species not in direction (would be better to do this before I calculate directionXX_boot)
	directionlat_boot = directionlat_boot[paste(directionlat_boot$name, directionlat_boot$region) %in% paste(direction$name, direction$region),]
	directiondepth_boot = directiondepth_boot[paste(directiondepth_boot$name, directiondepth_boot$region) %in% paste(direction$name, direction$region),]
		dim(directionlat_boot)
		dim(directiondepth_boot)
	directionlat_boot = directionlat_boot[order(directionlat_boot$region, directionlat_boot$name),] # sort
	directiondepth_boot = directiondepth_boot[order(directiondepth_boot$region, directiondepth_boot$name),]
		all(directionlat_boot$name == direction$name & directionlat_boot$region == direction$region) # make sure directionlat_boot and direction are in the same order
		all(directiondepth_boot$name == direction$name & directiondepth_boot$region == direction$region) # make sure directiondepth_boot and direction are in the same order
		all(directionlat_boot$btrue == direction$obslat1) # make sure observed lat shift values are identical
		all(directiondepth_boot$btrue == direction$obsdepth1) # make sure observed depth shift values are identical


	### Save directionlat_boot
	write.csv(directionlat_boot, file=paste('Output/directionlat_boot_', traintype, '_', Sys.Date(), '.csv', sep=''))
	write.csv(directiondepth_boot, file=paste('Output/directiondepth_boot_', traintype, '_', Sys.Date(), '.csv', sep=''))



##########################################################################
## Are model trends correlated to observed trends in lat or long?
##########################################################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
require(lmodel2)
	
direction = read.csv('Output/direction_allyrsonlytemp_2013-07-03.csv', row.names=1, stringsAsFactors=FALSE)
directionlat_boot = read.csv('Output/directionlat_boot_allyrsonlytemp_2013-07-03.csv', row.names=1, stringsAsFactors=FALSE)
directiondepth_boot = read.csv('Output/directiondepth_boot_allyrsonlytemp_2013-07-03.csv', row.names=1, stringsAsFactors=FALSE)
traintype='allyrsonlytemp'

# Verify dataframes are in the same order
all(directionlat_boot$name == direction$name & directionlat_boot$region == direction$region) # make sure directionlat_boot and direction are in the same order
all(directiondepth_boot$name == direction$name & directiondepth_boot$region == direction$region) # make sure directiondepth_boot and direction are in the same order
all(directionlat_boot$btrue == direction$obslat1) # make sure observed lat shift values are identical
all(directiondepth_boot$btrue == direction$obsdepth1) # make sure observed depth shift values are identical

# Averages by region
aggregate(x=list(gamhlat1 = direction$gamhlat1), by=list(region = direction$region), FUN=mean)
aggregate(x=list(gamhlat1 = direction$gamhlat1), by=list(region = direction$region), FUN=min)
aggregate(x=list(gamhlat1 = direction$gamhlat1), by=list(region = direction$region), FUN=max)


# Add bias and relative bias calculations
	direction$gamhlat1lag = sign(direction$gamhlat1)*(direction$obslat1 - direction$gamhlat1) # degree to which species is lagging behind thermal envelope: - is lag, + is shifting ahead (N or S), expressed as degrees/yr
	direction$gamhdepth1lag = sign(direction$gamhdepth1)*(direction$obsdepth1 - direction$gamhdepth1) # degree to which species is lagging behind thermal envelope: - is lag, + is shifting ahead (shallower or deeper)
	direction$gamhlat1rellag = sign(direction$gamhlat1)*(direction$obslat1 - direction$gamhlat1)/abs(direction$gamhlat1) # degree to which species is lagging behind thermal envelope: - is lag, + is shifting ahead (N or S), expressed as a fraction of the predicted shift
	direction$gamhdepth1rellag = sign(direction$gamhdepth1)*(direction$obsdepth1 - direction$gamhdepth1)/abs(direction$gamhdepth1) # degree to which species is lagging behind thermal envelope: - is lag, + is shifting ahead (N or S), expressed as a fraction of the predicted shift


## Useful functions
	# Mean Relative Bias: y are observation, yhat are predictions
	bias = function(y, yhat){
		if(length(y) != length(yhat)) stop('Both y and yhat must be same length')
		return(100/length(y) * sum((yhat-y)/y))
	}
	t.bias = function(y, yhat){
		if(length(y) != length(yhat)) stop('Both y and yhat must be same length')
		return(t.test((yhat-y)/y))
	}

## Calculations
	# Latitude
		# Summary calcs
	table(direction$obslat1>0)/nrow(direction) # fraction north or south
	table((direction$obslat1>0) == (direction$gamhlat1>0))/nrow(direction) # fraction in same direction as thermal envelope
	table(1+(direction$obslat1>0), direction$gamhlat1>0) # number shallower or deeper, and direction of thermal envelope
	
		# Statistical Correlations
	ca1 = summary(lm(gamhlat1 ~ obslat1, data=direction)); ca1 # pred vs. obs since former have more error
	summary(lm(obslat1 ~ gamhlat1, data=direction)) # obs vs. pred: same p-value and r2
	lmodel2(gamhlat1 ~ obslat1, data=direction, nperm=9999) # Type II regression
	cor.test(direction$obslat1, direction$gamhlat1, method='pearson') # Pearson
	cor.test(direction$obslat1, direction$gamhlat1, method='spearman') # Spearman
	ca2 = summary(lm(obslat2 ~ gamhlat2, data=direction)); ca2
	t = table(2*sign(direction$gamhlat1), sign(direction$obslat1)); t
		sum(t[1,1], t[2,2])/sum(t) # 73% in right direction
		sum(t[1,1], t[2,1])/sum(t) # 46% observed to shift south

		# By region
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='AFSC_EBS',]))
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='AFSC_Aleutians',]))
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='AFSC_GOA',]))
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='DFO_Newfoundland_Fall',]))
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='DFO_ScotianShelf',]))
		summary(m <- lm(obslat1 ~ gamhlat1, data=direction[direction$region=='DFO_ScotianShelf' & direction$spp != 'SQUALUS ACANTHIAS',]))
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='DFO_SoGulf',]))
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='NEFSC_Spring',]))
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='SEFSC_GOMex',]))
	summary(lm(obslat1 ~ gamhlat1, data=direction[direction$region=='WestCoast_Tri',]))

	summary(lm(obslat1 ~ gamhlat1*region, data=direction))
		
		# Mixed effects models with region (lat)
	require(nlme)
	mod = lme(obslat1 ~ gamhlat1, random= ~ 1+gamhlat1|region, data=direction)
		summary(mod)
		random.effects(mod)
	mod2 = lme(obslat1 ~ gamhlat1, random= ~ 1|region, data=direction)
	mod3 = lm(obslat1 ~ gamhlat1, data=direction)
		a = anova(mod, mod2, mod3); a # choose mod by AIC
		0.5 * (1-pchisq(a$L.Ratio[3],1)) # for mod2 vs. mod3 (include random intercept?)
		0.5 * ((1 - pchisq(a$L.Ratio[2], 1)) + (1 - pchisq(a$L.Ratio[2], 2))) # mod1 vs. mod2 (random slope?) # yes
	mod1 = lme(obslat1 ~ gamhlat1, random= ~ 1+gamhlat1|region, data=direction, method='ML')  # Test fixed effects
	mod2 = lme(obslat1 ~ 1, random= ~ 1+gamhlat1|region, data=direction, method='ML')
		anova(mod1, mod2)

		# Bootstrapped p-values
	nboot = as.numeric(gsub('b', '', names(directionlat_boot)[max(grep('b', names(directionlat_boot)))])) # get max # bootstraps from column names in shiftlat_boot
	teststats = numeric(nboot) # hold test-statistics from bootstrapped correlations
	testr2s = numeric(nboot) # hold r2s from bootstrapped correlations
	for(i in 1:nboot){ # for each bootstrapped shift statistics
		nm = paste('b', i, sep='') # column name in shiftlat_boot
		mod = lm(direction$gamhlat1 ~ directionlat_boot[[nm]])
		teststats[i] = summary(mod)$fstatistic[1]
		testr2s[i] = summary(mod)$r.squared
	}
	modobs = lm(direction$gamhlat1 ~ directionlat_boot$btrue) # the observed correlation
	obsteststat = summary(modobs)$fstatistic[1]
	obstestr2 = summary(modobs)$r.squared
	
	quartz(width=7, height=4) # for plot of null distribution
	par(mfrow=c(1,2))
	hist(teststats, xlab='F statistic', main='Bootstrapped correlation', breaks=40, col='darkgrey'); abline(v=obsteststat, col='red') # plot the bootstrap and observed values
	pval = min(sum(teststats>obsteststat), sum(teststats<obsteststat))/length(teststats) # p-value
		legend('topright', title = paste('p-value =', round(pval,3)), bty='o', legend='',, bg='white', box.col='white')
	hist(testr2s, xlab='r2 values', main='Bootstrapped correlation', breaks=40, col='darkgrey'); abline(v=obstestr2, col='red') # plot the bootstrap and observed values
	pval2 = min(sum(testr2s>obstestr2), sum(testr2s<obstestr2))/length(testr2s) # p-value
		legend('topright', title = paste('p-value =', round(pval2,3)), bty='o', legend='',, bg='white', box.col='white')
	
	
		# Bias calculations
	bias(direction$obslat1, direction$gamhlat1) # mean relative bias of the predictions
	t.bias(direction$obslat1, direction$gamhlat1) # t-test on the relative bias of the predictions
		j = direction$spp != 'ANARHICHAS DENTICULATUS'
	t.bias(direction$obslat1[j], direction$gamhlat1[j]) # t-test on the relative bias of the predictions w/out wolffish
	t.test(direction$gamhlat1lag) # t-test on lag
		hist(direction$gamhlat1lag, col='grey', breaks=20) 

	t.test(direction$gamhlat1rellag) # t-test on relative lag
		hist(direction$gamhlat1rellag, breaks=30, col='grey')
	t.test(direction$gamhlat1rellag[!(direction$spp == 'Zoarces americanus' & direction$region == 'NEFSC_Spring')]) # t-test on lag
		hist(direction$gamhlat1rellag[!(direction$spp == 'Zoarces americanus' & direction$region == 'NEFSC_Spring')], breaks=30, col='grey')


	# Depth
		# Summary calculations
	table(direction$obsdepth1>0)/nrow(direction) # fraction deeper or shallower
	table((direction$obsdepth1>0) == (direction$gamhdepth1>0))/nrow(direction) # fraction in same direction as thermal envelope
	table(1+(direction$obsdepth1>0), direction$gamhdepth1>0) # number shallower or deeper, and direction of thermal envelope

		# Statistical Correlations
	summary(lm(gamhdepth1 ~ obsdepth1, data=direction)) # pred vs. obs since former have more error
	summary(lm(obsdepth1 ~ gamhdepth1, data=direction)) # obs vs. pred (makes a nicer graph)
	lmodel2(gamhdepth1 ~ obsdepth1, data=direction, nperm=9999) # Type II regression
	cor.test(direction$obsdepth1, direction$gamhdepth1, method='spearman') # Spearman
	t = table(2*sign(direction$gamhdepth1), sign(direction$obsdepth1)); t
		sum(t[1,1], t[2,2])/sum(t) # 70% in right direction
		sum(t[1,1], t[2,1])/sum(t) # 58% observed to shift shallower

		# Mixed effects models with region (depth)
	require(nlme)
	mod = lme(obsdepth1 ~ gamhdepth1, random= ~ 1+gamhdepth1|region, data=direction, control = list(msVerbose=TRUE, maxIter=100, msMaxIter=500, opt='optim'))
		summary(mod)
		random.effects(mod)
	mod2 = lme(obsdepth1 ~ gamhdepth1, random= ~ 1|region, data=direction)
	mod3 = lm(obsdepth1 ~ gamhdepth1, data=direction)
		a = anova(mod, mod2, mod3); a # choose mod by AIC
		0.5 * (1-pchisq(a$L.Ratio[3],1)) # for mod2 vs. mod3 (include random intercept?)
		0.5 * ((1 - pchisq(a$L.Ratio[2], 1)) + (1 - pchisq(a$L.Ratio[2], 2))) # mod1 vs. mod2 (random slope?) # yes
	mod1 = lme(obsdepth1 ~ gamhdepth1, random= ~ 1+gamhdepth1|region, data=direction, method='ML', control=list(msMaxIter=500, opt='optim')) # Test fixed effects
	mod2 = lme(obsdepth1 ~ 1, random= ~ 1+gamhdepth1|region, data=direction, method='ML')
		anova(mod1, mod2)
	mod1 = lme(obsdepth1 ~ gamhdepth1, random= ~ 1|region, data=direction, method='ML')
	mod2 = lme(obsdepth1 ~ 1, random= ~ 1|region, data=direction, method='ML')
		anova(mod1, mod2)

		# Bias calculations
	t.test(direction$gamhdepth1lag) # t-test on lag
		hist(direction$gamhdepth1lag)
	t.test(direction$gamhdepth1rellag) # t-test on relative lag
		hist(direction$gamhdepth1rellag)
		direction[direction$gamhdepth1rellag>500, c('spp', 'region', 'obsdepth1', 'gamhdepth1', 'gamhdepth1rellag')]
	t.test(direction$gamhdepth1rellag[!((direction$spp == 'GYMNELIS VIRIDIS' & direction$region == 'DFO_Newfoundland_Fall'))]) # t-test on relative lag w/out outlier

###### Plots ######

	# Plot observed lat and depth predicted shifts vs. observed for training (Fig. 3 for paper)
	require(RColorBrewer)
	#wd = 11; ht = 6; ps=14 # for ppt
	wd = 4.6; ht = 2.3; mai = c(0.5, 0.5, 0.05, 0.05); mgp=c(1.7,0.5,0); cexlab = 1; cex = 1; cexmn = 0.8; cexpt = 0.8; ps=9 # for paper

	quartz(width=wd, height=ht)
	# pdf(width=wd, height=ht, file=paste('Figures/obslat&depth gamhlat&depth ', traintype, ' ', Sys.Date(), '.pdf', sep=''))
	par(mfrow=c(1,2), mai=mai, mgp=mgp, ps=ps, tcl=-0.3)
	cols = rep(rgb(0.2, 0.2, 0.2, 0.5), 9) # for transparent dark grey
	cinds = (direction$region=='AFSC_Aleutians') + 2*(direction$region=='AFSC_EBS') + 3*(direction$region=='AFSC_GOA') + 4*(direction$region=='DFO_Newfoundland_Fall') + 5*(direction$region=='DFO_ScotianShelf') + 6*(direction$region=='DFO_SoGulf') + 7*(direction$region=='NEFSC_Spring') + 8*(direction$region=='SEFSC_GOMex') + 9*(direction$region=='WestCoast_Tri')
	main1 = ''; main2 = ''
	plot(direction$gamhlat1, direction$obslat1, main=main1, xlab = '', ylab = '', col=cols[cinds], pch=16, cex=cexpt, las=1)
		abline(a=0,b=1, col='red')
		mod = lm(gamhlat1~obslat1, data=direction); sum = summary(mod); 
		mtext('Taxon shift (°N/yr)', side=2, line=1.7)
		mtext('Thermal envelope shift (°N/yr)', side=1, line=1.4)
		mtext("A", side=3, line=-1, adj=0.05, font=2)

	plot(direction$gamhdepth1, direction$obsdepth1, main=main2, xlab = '', ylab = '', col=cols[cinds], pch=16, cex=cexpt, las=1)
		abline(a=0,b=1, col='red')
		mod = lm(gamhdepth1~obsdepth1, data=direction); sum = summary(mod)
		mtext('Thermal envelope shift (m/yr)', side=1, line=1.4)
		mtext('Taxon shift (m/yr)', side=2, line=1.2)
		mtext("B", side=3, line=-1, adj=0.05, font=2)

	dev.off()
	
	## Plot obslat ~ gamhlat for each region separately (Fig. S5 for paper)
	regs = sort(unique(direction$region))
	mains = rep('', length(regs))
	#mains = regs
	labs = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')
	xlabs = c('', '', '', '', '', '', 'Shift in thermal envelope (°N/yr)', 'Shift in thermal envelope (°N/yr)', 'Shift in thermal envelope (°N/yr)')
	ylabs = c('Observed shift (°N/yr)', '', '', 'Observed shift (°N/yr)', '', '', 'Observed shift (°N/yr)', '', '')
	col = rgb(0.2, 0.2, 0.2, 0.5) # for transparent dark grey
	axiscex=1.2
	labcex=0.9
	legcex = 1 # for inset legend
	legpos = c('topleft', 'topleft', 'bottomright', 'topleft', 'topleft', 'bottomright', 'topleft', 'topleft', 'topleft')
	leginset = c(-0.05, -0.05, 0, -0.05, -0.05, 0, -0.05, -0.05, -0.05)

	quartz(width=7, height=6.5)
	# pdf(width=7, height=6.5, file=paste('Figures/obslat gamhlat ', traintype, ' byregion trainingonly ', Sys.Date(), '.pdf', sep=''))
	par(mfrow=c(3,3), mai=c(0.3, 0.4, 0.3, 0.1), mgp=c(3,0.7,0), omi=c(0.3, 0.3,0,0.15))
	for(i in 1:length(regs)){
		inds = direction$region==regs[i]
		plot(direction$gamhlat1[inds], direction$obslat1[inds], main=mains[i], xlab = '', ylab = '', cex.main=1, pch=16, col=col, las=1, cex.axis=axiscex)
		# text(direction$gamhlat1[inds], direction$obslat1[inds], labels = direction$name[inds], cex=0.5)
		abline(a=0,b=1, col='red')
		mod = lm(gamhlat1~obslat1, data=direction[inds,]); sum = summary(mod)
		legend(legpos[i], bty='n', legend=c(paste('p =',signif(sum$coef[2,4],2)), paste('r2 =',signif(sum$r.sq,2))), inset=c(leginset[i], 0), cex=legcex) # add lm regression in legend
		mtext(text=xlabs[i], side=1, line=3, cex=labcex)
		mtext(text=ylabs[i], side=2, line=3.5, cex=labcex)
		mtext(text=labs[i], side=3, line=0.5, adj=0.05, font=2)		
	}
	
	dev.off()


	
	## Plot obsdepth ~ gamhdepth for each region separately (training period only) (Fig. S6 for paper)
	regs = sort(unique(direction$region))
	mains = rep('', length(regs))
	#mains = regs
	labs = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')
	xlabs = c('', '', '', '', '', '', 'Shift in thermal envelope (m/yr)', 'Shift in thermal envelope (m/yr)', 'Shift in thermal envelope (m/yr)')
	ylabs = c('Observed shift (m/yr)', '', '', 'Observed shift (m/yr)', '', '', 'Observed shift (m/yr)', '', '')
	col = rgb(0.2, 0.2, 0.2, 0.5) # for transparent dark grey
	axiscex=1.2
	labcex=0.9
	legcex = 1 # for inset legend
	legpos = c('topleft', 'bottomright', 'topleft', 'topleft', 'topleft', 'topleft', 'topleft', 'topleft', 'topleft')
	leginset = c(-0.05, 0, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05)

	quartz(width=7, height=6.5)
	# pdf(width=7, height=6.5, file=paste('Figures/obsdepth gamhdepth ', traintype, ' byregion trainingonly ', Sys.Date(), '.pdf', sep=''))
	par(mfrow=c(3,3), mai=c(0.3, 0.4, 0.3, 0.1), mgp=c(3,0.7,0), omi=c(0.3, 0.2,0,0.15))
	for(i in 1:length(regs)){
		inds = direction$region==regs[i]
		plot(direction$gamhdepth1[inds], direction$obsdepth1[inds], main=mains[i], xlab = '', ylab = '', cex.main=1, pch=16, col=col, las=1, cex.axis=axiscex)
		abline(a=0,b=1, col='red')
		mod = lm(gamhdepth1~obsdepth1, data=direction[inds,]); sum = summary(mod)
		legend(legpos[i], bty='n', legend=c(paste('p =',signif(sum$coef[2,4],2)), paste('r2 =',signif(sum$r.sq,2))), inset=c(leginset[i], 0), cex=legcex) # add lm regression in legend
		mtext(text=xlabs[i], side=1, line=3, cex=labcex)
		mtext(text=ylabs[i], side=2, line=3.2, cex=labcex)
		mtext(text=labs[i], side=3, line=0.5, adj=0.05, font=2)		
	}
	
	dev.off()
	



#############################################################
## Data Prep to Correlate long-term lags to species traits ##
#############################################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')

direction = read.csv('Output/direction_allyrsonlytemp_2013-07-03.csv', row.names=1); traintype='allyrsonlytemp'
	dim(direction)
spptax = read.csv('Life history/data/spptaxonomy_2012-08-12 manual.csv', row.names=1) # matches taxon name to taxonomy
spplh = read.csv('Life history/Data/spplh_2012-10-22 combined.csv', row.names=1, na.strings=c('NA', '')) # matches taxonomy to life history data
shift = read.csv('Output/shift_2012-11-15.csv', row.names=1) # for biomass trend
shiftreg = read.csv('Output/shiftreg_onlygenera_2013-02-19.csv', row.names=1); suff = '_onlygenera' # shifts by region, only using spp with >=5 presences in every year
	
## Merge in taxonomy
names(direction)[names(direction)=='spp'] = 'taxon'
setdiff(direction$taxon, spptax$taxon) # should be 0

directionlh = merge(direction, spptax[,c('taxon', 'species', 'genus', 'family', 'superclass')], by='taxon', all.x=TRUE) # superclass only for IDing fish easily (Pisces)
	dim(directionlh)

	# Examine rows that didn't match
		i = is.na(directionlh$family)
		directionlh[i, c('taxon', 'family')] # 0, good

## Merge in life history data
	intersect(names(directionlh), names(spplh))
	
	# make a common column
	directionlh$taxonomy = paste(directionlh$family, directionlh$genus, directionlh$species, sep='.')
	spplh$taxonomy = paste(spplh$family, spplh$genus, spplh$species, sep='.')

	setdiff(directionlh$taxonomy, spplh$taxonomy) # 0: nothing missing from spplh
	
	directionlh = merge(directionlh, spplh[,c('taxonomy', 'MaxLength', 'Troph', 'DemersPelag', 'DemersPelag2', 'K', 'NorthSouthN', 'NorthSouthS', 'fished')])
		dim(directionlh) # 325 x 35

# Merge in biomass trend
	setdiff(shift$spp, spptax$taxon) # 0: good
	names(shift)[names(shift)=='spp'] = 'taxon'
		dim(shift)
	shift = merge(shift, spptax, all.x=TRUE, by='taxon') # add taxonomy to shift dataframe
		dim(shift)
	shift$taxonomy = paste(shift$family, shift$genus, shift$species, sep='.') # Column on which to merge

	setdiff(directionlh$taxonomy, shift$taxonomy) # 0: nothing missing from shift
	setdiff(directionlh$region, shift$region) # 0: nothing missing from shift
	
	directionlh$region = as.character(directionlh$region)
	shift$region = as.character(shift$region)

	dim(directionlh)
	directionlh = merge(directionlh, shift[,c('taxonomy', 'region', 'biomass.b.lm')])
	dim(directionlh) # 325 x 36
	
# Add some new vars
	# Commercially fished or not
directionlh$fished3 = directionlh$fished
	levels(directionlh$fished3)
	levels(directionlh$fished3)[c(1,3,4)] = 'no'
	levels(directionlh$fished3)
	directionlh = directionlh[,!(names(directionlh) %in% 'fished')]
	dim(directionlh)

	# Latitudinal range size
directionlh$range = directionlh$NorthSouthN - directionlh$NorthSouthS
	summary(directionlh$range)
	sum(!is.na(directionlh$range))
	directionlh = directionlh[,!(names(directionlh) %in% c('NorthSouthN', 'NorthSouthS'))]
	dim(directionlh) # 325 x 35

	# Simpler demersal/pelagic split. 
	# Only ambiguous one is benthopelagic, see Fishbase, "living and/or feeding on or near the bottom, as well as in midwater, between 0 and 200 m"
directionlh$DemersPelag3 = NA
	levels(directionlh$DemersPelag)
	levels(directionlh$DemersPelag2)
	directionlh$DemersPelag3[as.character(directionlh$DemersPelag) %in% c('pelagic', 'bathypelagic', 'pelagic-neritic', 'pelagic-oceanic')] = 'pelagic'
	directionlh$DemersPelag3[as.character(directionlh$DemersPelag2) == 'pelagic'] = 'pelagic'
	directionlh$DemersPelag3[as.character(directionlh$DemersPelag) %in% c('benthopelagic', 'demersal', 'reef-associated', 'bathydemersal')] = 'demersal'
	directionlh$DemersPelag3[as.character(directionlh$DemersPelag2) %in% c('demersal', 'benthopelagic', 'benthic')] = 'demersal'
	directionlh$DemersPelag3 = as.factor(directionlh$DemersPelag3)
	levels(directionlh$DemersPelag3)
	sort(unique(directionlh$DemersPelag3))
	directionlh = directionlh[,!(names(directionlh) %in% c('DemersPelag', 'DemersPelag2'))]
	dim(directionlh) # 325 x 34
	
	# Fish vs. Invert
directionlh$fishinvert = FALSE
	directionlh$fishinvert[directionlh$superclass=='Pisces' & !is.na(directionlh$superclass)] = TRUE
	directionlh = directionlh[,!(names(directionlh) %in% 'superclass')]
	dim(directionlh) # 325 x 34

# Regional characteristics
	directionlh$tsdur = (directionlh$region == 'AFSC_Aleutians')*31 + (directionlh$region == 'AFSC_EBS')*30 + (directionlh$region == 'AFSC_GOA')*28 + (directionlh$region == 'DFO_Newfoundland_Fall')*17 + (directionlh$region == 'DFO_ScotianShelf')*42 + (directionlh$region == 'DFO_SoGulf')*39 + (directionlh$region == 'NEFSC_Spring')*41 + (directionlh$region == 'SEFSC_GOMex')*25 + (directionlh$region == 'WestCoast_Tri')*28 # duration of survey

	directionlh = merge(directionlh, shiftreg[,c('region', 'rangelat')], by='region', all.x=TRUE) # add latitudinal range
		names(directionlh)[names(directionlh)=='rangelat'] = 'extentlat'
		dim(directionlh) # 325 x 36
	
# Trim some more columns I don't need
	names(directionlh)[names(directionlh) == 'name'] = 'sciname'
	directionlh = directionlh[, c('region', 'sciname', 'family', 'genus', 'species', 'obslat1', 'gamhlat1', 'obsdepth1', 'gamhdepth1', 'fishinvert', 'DemersPelag3', 'biomass.b.lm', 'fished3', 'MaxLength', 'Troph', 'K', 'range', 'tsdur', 'extentlat')]
		dim(directionlh) # 325 x 19
		names(directionlh)
		
# Sort
	directionlh = directionlh[order(directionlh$region, directionlh$sciname),]
	
# Write out
	write.csv(directionlh, paste('Output/directionlh_', traintype, '_', Sys.Date(), '.csv', sep=''))

################################
## Multiple Linear Regression ##
################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
require(visreg) # for partial residuals
require(AICcmodavg) # for AICcs
source('lm.aic.all 2012-09-07.r')
directionlh = read.csv('Output/directionlh_allyrsonlytemp_2013-07-03.csv', row.names=1)

# Add log variables and absolute rate of shift
	directionlh$logK = log(directionlh$K)
	directionlh$logMaxLength = log(directionlh$MaxLength)
	directionlh$logLongevity = log(directionlh$LongevityWild)

	directionlh$obslat1abs = abs(directionlh$obslat1)
	directionlh$gamhlat1abs = directionlh$gamhlat1*sign(directionlh$obslat1) # predicted shift rate, 

# Sample size by region
	table(directionlh$region)

# Sample size by factor for all taxa
	nrow(directionlh) # 325
	with(directionlh, sum(!is.na(obslat2) & !is.na(DemersPelag3))) # , 325
	with(directionlh, sum(!is.na(obslat2) & !is.na(fished))) # , 325

	with(directionlh, sum(!is.na(obslat2) & !is.na(Troph))) # , 255
	with(directionlh, sum(!is.na(obslat2) & !is.na(fecundity))) # , 180

# Sample size by factor for fish
	sum(directionlh$fishinvert) # , 259 (w/out family, w/out genus)
	with(directionlh, sum(!is.na(obslat2) & fishinvert & !is.na(DemersPelag3))) # , 259
	with(directionlh, sum(!is.na(obslat2) & fishinvert  & !is.na(fished))) # , 259
	with(directionlh, sum(!is.na(obslat2) & fishinvert  & !is.na(MaxLength))) # , 241
	with(directionlh, sum(!is.na(obslat2) & fishinvert  & !is.na(Troph))) # , 241
	with(directionlh, sum(!is.na(obslat2) & fishinvert  & !is.na(K))) # , 231
	with(directionlh, sum(!is.na(obslat2) & fishinvert  & !is.na(range))) # 203

# Choose which variables to include
	vars = c('gamhlat1abs', 'fishinvert', 'fished3', 'DemersPelag3', 'biomass.b.lm', 'tsdur', 'extentlat'); tax='All'; yvar = 'logabs' # for inverts + fish log(obslat1abs) 
	
	vars = c('gamhlat1abs', 'fished3', 'DemersPelag3', 'biomass.b.lm', 'logMaxLength', 'Troph', 'logK', 'tsdur', 'extentlat', 'range'); tax='Fish'; yvar = 'logabs' # for fish log(obslat1abs)

	vars = c('gamhlat1', 'fishinvert', 'fished3', 'DemersPelag3', 'biomass.b.lm', 'tsdur', 'extentlat'); tax='All'; yvar = 'raw' # for inverts + fish obslat1

	vars = c('gamhlat1', 'fished3', 'DemersPelag3', 'biomass.b.lm', 'logMaxLength', 'Troph', 'logK', 'tsdur', 'extentlat', 'range'); tax='Fish'; yvar = 'raw' # for fish obslat1 (no longevity)

# Trim to complete cases
	if(tax=='All'){ i = complete.cases(directionlh[,vars[!grepl(':', vars)]]); tax = 'All'; sum(i)} # with all data
	if(tax=='Fish'){	i = complete.cases(directionlh[,vars[!grepl(':', vars)]]) & directionlh$fishinvert; tax = 'Fish'; sum(i)} # with all data among fishes


# Colinearity?
	r2 = matrix(NA, nrow=length(vars), ncol=length(vars))
		colnames(r2) = vars; rownames(r2) = vars
	for(k in 1:(length(vars)-1)){
		for(j in (k+1):length(vars)){
			r2[k,j] = round(cor(as.numeric(directionlh[[vars[k]]][i]), as.numeric(directionlh[[vars[j]]][i]))^2,2)
		}
	}
	r2	

# All subsets lm		
	if(yvar=='logabs') mod2 = lm.aic.all(y=log(directionlh$obslat1abs[i]), x=directionlh[i,vars], models='all') # test all subsets with log(abs shift)
	if(yvar=='raw') mod2 = lm.aic.all(y=directionlh$obslat1[i], x=directionlh[i,vars], models='all') # test all subsets with raw shift value
		aic = numeric(length(mod2))
		for(i in 1:length(mod2)) aic[i] = AIC(mod2[[i]])
		aicc = numeric(length(mod2)) # small-sample version
		for(i in 1:length(mod2)) aicc[i] = AICc(mod2[[i]])
		r2 = numeric(length(mod2))
		for(i in 1:length(mod2)) r2[i] = summary(mod2[[i]])$r.squared

		cor.test(aicc, aic)

		modord = order(aic) # order from best to worst
		
	# AIC model weights
	aicd = aic - min(aic) # delta aic
	aicw = exp(-aicd/2)/sum(exp(-aicd/2), na.rm=TRUE) # AIC weights: Burnham & Anderson p. 75 section 2.9.1

	# term weights
	for(i in 1:length(vars)){
		cat(paste(format(vars[i], width=17), signif(sum(aicw[grep(vars[i], names(mod2))], na.rm=TRUE), 3), '\n'))
	}

	# full model
	summary(mod2[[1]])
	
	# best model
	summary(mod2[[modord[1]]])
	aic[modord[1]]
	aicw[modord[1]]
	#par(mfrow=c(1,3)); visreg(mod2[[modord[1]]], trans=exp, type='conditional')

	# plot survey duration and climate velocity
		#plot(directionlh$tsdur, directionlh$gamhlat1abs)
		#summary(lm(directionlh$gamhlat1abs ~ directionlh$tsdur))

	# plot survey duration and lags
		#plot(directionlh$tsdur, directionlh$obslat1abs - directionlh$gamhlat1abs)
		#summary(lm(I(directionlh$obslat1abs - directionlh$gamhlat1abs) ~ directionlh$tsdur))
		#i = = which(directionlh$obslat1abs - directionlh$gamhlat1abs < 0.2) # remove an outlier
		#summary(lm(I(directionlh$obslat1abs[i] - directionlh$gamhlat1abs[i]) ~ directionlh$tsdur[i]))

	# Multi-model inference
	options(warn=2) # turn warnings to errors
	vars2 = c('Intercept', vars)
	param = numeric(length(vars2))
	se = numeric(length(vars2))
	for(i in 1:length(vars2)){
		print(vars2[i])
		subs = grep(vars2[i], names(mod2))
		if(vars2[i] == 'Intercept') subs = 1:length(mod2)
		for(j in subs){
			if(all(!is.na(mod2[[j]]))){ # skip any models that couldn't be fit
				c = summary(mod2[[j]])$coefficients
				k = grep(vars2[i], rownames(c))
				if(vars2[i] == 'Intercept') k=1
				if(length(k)>1) stop('matched >1 row')
				param[i] = param[i] + aicw[j] * c[k,1] # Eq. 4.1 in Burnham & Anderson (p. 150)
				se[i] = se[i] + aicw[j] * sqrt(c[k,2]^2 + (c[k,1]- param[i])^2) # Eq. 4.9 in Burnham & Anderson (p. 162)
			}
		}
	}
	params = data.frame(var = vars2, param=param, se=se)
	print(params)
	
			
	# write out results for top 6 models, only climate velocity, only survey duration, and only survey extent (for paper)
	if(yvar=='logabs') outvars = c('family', 'gamhlat1abs', 'maxlatshift', 'extentlat', 'tsdur', 'fishinvert', 'logK', 'fished3', 'DemersPelag3', 'range', 'biomass.b.lm', 'Troph', 'logMaxLength',  'logLongevity') # so I can set the order in the table
	if(yvar=='raw') outvars = c('family', 'gamhlat1', 'maxlatshift', 'extentlat', 'tsdur', 'fishinvert', 'logK', 'fished3', 'DemersPelag3', 'range', 'biomass.b.lm', 'Troph', 'logMaxLength',  'logLongevity') # so I can set the order in the table
	outvars = outvars[outvars %in% vars]
	outvarslong = outvars # for matching to coefficient names
	outvarslong[outvarslong=='DemersPelag3'] = 'DemersPelag3pelagic'
	outvarslong[outvarslong=='fished3'] = 'fished3no'
	outvarslong[outvarslong=='fishinvert'] = 'fishinvertTRUE' # is it a fish?
	n = length(outvars)+3
	maxmods = 6
	out = data.frame(Taxa = tax, Factor = c(outvarslong, 'delAIC', 'r2', 'w'), Weights = character(n), M1 = character(n), M2 = character(n), M3 = character(n), M4 = character(n), M5 = character(n), M6 = character(n), Full = character(n), Ave = character(n), CV = character(n), SD = character(n), SE = character(n), SDSE = character(n), SDSECV = character(n), SppChar = character(n), stringsAsFactors=FALSE)
	if(maxmods >6){
		for(i in 7:maxmods){
			nm = paste('M', i, sep='')
			out[[nm]] = character(n)
		}
	}
	modord = order(aic)
	for(i in 1:min(maxmods,length(mod2))){
		v = names(coef(mod2[[modord[i]]])) # variable names in this model
		s2<- signif(coef(mod2[[modord[i]]]),2)
		nm = paste('M', i, sep='')
		for(j in 1:length(outvars)){
			if(any(grepl(outvarslong[j], v))){
				out[[nm]][j] = s2[grep(outvarslong[j], v)]
			}
		}
		out[[nm]][j+1] = signif(aic[modord[i]] - min(aic),3)
		out[[nm]][j+2] = signif(r2[modord[i]],2)
		out[[nm]][j+3] = signif(aicw[modord[i]],2)
	}
	# Full model
		v = names(coef(mod2[[1]])) # variable names in this model
		s2<- signif(coef(mod2[[1]]),2)
		for(j in 1:length(outvars)){
			out$Full[j] = s2[grep(outvarslong[j], v)]
		}
		out$Full[j+1] = signif(aic[1] - min(aic),3)
		out$Full[j+2] = signif(r2[1],2)	
		out$Full[j+3] = signif(aicw[1],2)
	# Averaged model
		v = names(coef(mod2[[1]])) # variable names in this model (same as full model)
		for(j in 1:length(outvars)){
			out$Ave[j] = signif(params$param[grep(outvars[j], params$var)],2)
		}
	# special models to include as well
	if(yvar=='logabs') specialmods = c('y~gamhlat1abs', 'y~tsdur', 'y~extentlat', 'y~tsdur+extentlat', 'y~gamhlat1abs+tsdur+extentlat')
	if(yvar=='raw') specialmods = c('y~gamhlat1', 'y~tsdur', 'y~extentlat', 'y~tsdur+extentlat', 'y~gamhlat1+tsdur+extentlat')
	if(tax=='All') specialmods = c(specialmods, 'y~fishinvert+fished3+DemersPelag3+biomass.b.lm')
	if(tax=='Fish') specialmods = c(specialmods, 'y~fished3+DemersPelag3+biomass.b.lm+logMaxLength+Troph+logK+range')
	specialnames = c('CV', 'SD', 'SE', 'SDSE', 'SDSECV', 'SppChar')
	for(j in 1:length(specialmods)){
		i = which(names(mod2) == specialmods[j])
		v = names(coef(mod2[[i]])) # variable names in this model
		s2<- signif(coef(mod2[[i]]),2)
		nm = specialnames[j]
		for(j in 1:length(outvars)){
			if(any(grepl(outvarslong[j], v))){
				out[[nm]][j] = s2[grep(outvarslong[j], v)]
			}
		}
		out[[nm]][j+1] = signif(aic[i] - min(aic),3)
		out[[nm]][j+2] = signif(r2[i],2)
		out[[nm]][j+3] = signif(aicw[i],2)
	}
	# Single-variable models
	for(j in 1:length(outvars)){ # add columns for single-variable models
		nm = outvars[j]
		out[[nm]] = character(n) # add the column
		i = which(names(mod2) == paste('y~', outvars[j], sep='')) # find the right model
		v = names(coef(mod2[[i]])) # variable names in this model
		s2<- signif(coef(mod2[[i]]),2)
		out[[nm]][j] = s2[2]
		out[[nm]][length(outvars)+1] = signif(aic[i] - min(aic),3)
		out[[nm]][length(outvars)+2] = signif(r2[i],2)
		out[[nm]][length(outvars)+3] = signif(aicw[i],2)
	}
	# Term weights
	for(i in 1:length(outvars)){
		out$Weights[i] = signif(sum(aicw[grep(outvars[i], names(mod2))], na.rm=TRUE), 3)
	}

	out			

# Write out table of models (Tables 1, S4, and S5)
	write.csv(out, file=paste('Tables/traitmods_', tax, '_', yvar, '_', Sys.Date(), '.csv', sep=''), row.names=FALSE)


