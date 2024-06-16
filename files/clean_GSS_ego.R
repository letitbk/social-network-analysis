#------------------------------------------------------------------------------
# process GSS_NDI data sets
#==============================================================================
library(data.table)
library(rio)
library(fst)
# please specify your root box directory here
here = '~/Dropbox/project/GSS_suicide/'

gss = as.data.table(import(file.path(here,'data','rawdata','gss1972_2018','GSS7218_R3.DTA')))
data_path = './files'

# check network variables
a_netvar = c('talkto',
	'age','educ','relig','sex','race',
	'sibling','parent','spouse','child','othfam','cowork','friend','neighbr','advisor','memgrp','other')

# first take care of missing values (e.g., .d or .i in stata data format = missing)
netvar = paste0(a_netvar,rep(1:5, each = length(a_netvar)))
indvar = c('age', 'educ', 'relig', 'sex', 'race')

# select only 2004 data + relevant measures
gss_network = gss[!is.na(numgiven) & year == 2004,]
gss_network[,n_size := numgiven]

gss_ind = gss_network[,c('id','year','wtssall','sampcode','n_size',indvar),with=FALSE]
gss_network = gss_network[,c('id','year','wtssall','sampcode',netvar),with=FALSE]

# rename ego measures (no)
#for (var in indvar) setnames(gss_ind,var, paste0('r_',var))

# from wide to long formats
gss_network = melt(gss_network, 
	measure = patterns('^age','^educ','^sex','^relig','^race','^talkto',
		'^sibling','^parent','^spouse','^child','^othfam','^cowork','^friend','^neighbr','^advisor','^memgrp','^other'),
	value.name = paste0('a_',c('age','educ','sex','relig','race','talkto',
		'sibling','parent','spouse','child','othfam','cowork','friend','neighbr','advisor','memgrp','other')),
	id.vars = c('id','year'))
setnames(gss_network,'variable','alterid')
gss_network[, n_nonmiss := rowSums(!is.na(.SD)), .SDcols = paste0('a_', a_netvar) ]

gss_network = gss_network[n_nonmiss > 0,]

for (var in indvar) {
	setnames(gss_network, paste0('a_', var), var)	
}

# add gss aa ties
gss_aaties = gss[year == 2004, c('id','year',paste0('close',c(12,13,14,15,23,24,25,34,35,45))), with=F]
gss_aaties = melt(gss_aaties, id.var = c('id','year'))

gss_aaties[, source_id := substr(gsub('close','',variable), 1, 1)]
gss_aaties[, target_id := substr(gsub('close','',variable), 2, 2)]
setnames(gss_aaties, 'value', 'close')

gss_aaties[, drop := 1]
list_id = unique(gss_aaties$id)

for (ego_id in list_id){
	alter_ids = gss_network[id %in% ego_id, alterid]
	gss_aaties[id %in% ego_id & (source_id %in% alter_ids & target_id %in% alter_ids), drop := 0]
}

# drop ties that are inapplicable 
gss_aaties = gss_aaties[drop == 0, ]

gss_aaties = gss_aaties[, c('id','year','source_id','target_id','close')]

# additional variable cleaning?
fwrite(gss_ind, file.path(data_path,'gssnet_ego.csv'))
fwrite(gss_network, file.path(data_path,'gssnet_dyad.csv'))
fwrite(gss_aaties, file.path(data_path,'gssnet_aaties.csv'))


