###########Functions for creating camtrapDP 1.0
library(R6)
library(tidyverse)
library(taxadb)
library(camtrapdp)
library(frictionless)

#' @title Create deployments
#' @description \code{create_deployments} creates a table of deployments.
#' @import magrittr
#' @importFrom lubridate as_datetime
#' @importFrom tibble tibble
#' @return A tibble of deployments.
#' @export
#' 
create_deployments<-function(deploymentID,
					latitude,
					longitude,
					deploymentStart=NULL,
					deploymentStart_date=NULL,
					deploymentStart_time=NULL,
					deploymentEnd=NULL,
					deploymentEnd_date=NULL,
					deploymentEnd_time=NULL,
					locationID=NULL,
					locationName=NULL,
					coordinateUncertainty=NULL,
					setupBy=NULL,
					cameraID=NULL,
					cameraModel=NULL,
					cameraDelay=NULL,
					cameraHeight=NULL,
					cameraDepth=NULL,
					cameraTilt=NULL,
					cameraHeading=NULL,
					detectionDistance=NULL,
					timestampIssues=NULL,
					baitUse=NULL,
					featureType=NULL,
					habitat=NULL,
					deploymentGroups=NULL,
					deploymentTags=NULL,
					deploymentComments=NULL,
					tz="Japan"
					){
					
	#Initial check
	if((!is.null(cameraHeight))&(!(is.null(cameraDepth)))){
		stop("'cameraHeight' and 'cameraDepth' can not be specfied simultaneously.")
	}

	if(is.null(latitude)|is.null(longitude)){
		stop("'latitude' and 'longitude' should be specified.")
	}else{
		if(any(is.na(latitude)|any(is.na(longitude)))){
			stop("Missing values in 'latitude' and 'longitude' are not permitted.")
		}
	}

	
	if(is.null(deploymentStart)){
		if(is.null(deploymentStart_date)|is.null(deploymentStart_time)){
			stop("'deploymentStart must be specified.'")
		}else{
			deploymentStart<-paste(deploymentStart_date,deploymentStart_time)
		}
	}else{
		deploymentStart<-as.character(deploymentStart)
	}
	
	if(is.null(deploymentEnd)){
		if(is.null(deploymentEnd_date)|is.null(deploymentEnd_time)){
			stop("'deploymentEnd must be specified.'")
		}else{
			deploymentEnd<-paste(deploymentEnd_date,deploymentEnd_time)
		}
	}else{
		deploymentEnd<-as.character(deploymentEnd)
	}
	
	deploymentStart<-deploymentStart%>%
				as_datetime(tz=tz)
	

	deploymentEnd<-deploymentEnd%>%
				as_datetime(tz=tz)
				
	if(any(is.na(deploymentStart)|is.na(deploymentEnd))){
		stop("Missing values in 'deploymentStart' and 'deploymentEnd' are not permitted.")
	}

	deploymentID<-as.character(deploymentID)
	
	latitude<-as.numeric(latitude)
	longitude<-as.numeric(longitude)
	
	ndep<-length(latitude)
	if(!is.null(locationID)){
		locationID<-as.character(locationID)
	}else{
		locationID<-as.character(rep(NA,ndep))
	}
	
	if(!is.null(locationName)){
		locationName<-as.character(locationName)
	}else{
		locationName<-as.character(rep(NA,ndep))
	}
	
	if(!is.null(coordinateUncertainty)){
		coordinateUncertainty<-as.numeric(round(coordinateUncertainty))
	}else{
		coordinateUncertainty<-as.numeric(rep(NA,ndep))
	}
	
	if(!is.null(setupBy)){
		setupBy<-as.character(setupBy)
	}else{
		setupBy<-as.character(rep(NA,ndep))
	}

	if(!is.null(cameraID)){
		cameraID<-as.character(cameraID)
	}else{
		cameraID<-as.character(rep(NA,ndep))
	}

	if(!is.null(cameraModel)){
		cameraModel<-as.character(cameraModel)
	}else{
		cameraID<-as.character(rep(NA,ndep))
	}

	if(!is.null(cameraDelay)){
		cameraDelay<-as.numeric(round(cameraDelay))
	}else{
		cameraDelay<-as.numeric(rep(NA,ndep))
	}

	if(!is.null(cameraHeight)){
		cameraHeight<-as.numeric(cameraHeight)
	}else{
		cameraHeight<-as.numeric(rep(NA,ndep))
	}

	if(!is.null(cameraDepth)){
		cameraDepth<-as.numeric(cameraDepth)
	}else{
		cameraDepth<-as.numeric(rep(NA,ndep))
	}

	if(!is.null(cameraTilt)){
		cameraTilt<-as.numeric(round(cameraTilt))
	}else{
		cameraTilt<-as.numeric(rep(NA,ndep))
	}

	if(!is.null(cameraHeading)){
		cameraHeading<-as.numeric(round(cameraHeading))
	}else{
		cameraHeading<-as.numeric(rep(NA,ndep))
	}

	if(!is.null(detectionDistance)){
		detectionDistance<-as.numeric(detectionDistance)
	}else{
		detectionDistance<-as.numeric(rep(NA,ndep))
	}

	if(!is.null(timestampIssues)){
		if(is.character(timestampIssues)){
			if(any(!grepl("^(true|false)$",timestampIssues),na.rm=TRUE)){
				stop("'timestampIssues' has invalid elements.")
			}
		}
		timestampIssues<-as.logical(timestampIssues)
	}else{
		timestampIssues<-as.logical(rep(NA,ndep))
	}

	if(!is.null(baitUse)){
		if(is.character(baitUse)){
			if(any(!grepl("^(true|false)$",baitUse),na.rm=TRUE)){
				stop("'baitUse' has invalid elements.")
			}
		}
		baitUse<-as.logical(baitUse)
	}else{
		baitUse<-as.logical(rep(NA,ndep))
	}
	
	if(!is.null(featureType)){
		if(any(!(grepl("^(roadPaved|roadDirt|trailHiking|trailGame|roadUnderpass|roadOverpass|roadBridge|culvert|burrow|nestSite|carcass|waterSource|fruitingTree)$",featureType)|is.na(featureType)),na.rm=TRUE)){
			stop("'featureType' has invalid elements.")
		}
	}else{
		featureType<-rep(NA,ndep)
	}
	featureType<-factor(featureType,c("roadPaved", "roadDirt", "trailHiking", "trailGame", "roadUnderpass", "roadOverpass", "roadBridge", "culvert", "burrow", "nestSite", "carcass", "waterSource", "fruitingTree"))

	if(!is.null(habitat)){
		habitat<-as.character(habitat)
	}else{
		habitat<-as.character(rep(NA,ndep))
	}

	if(!is.null(deploymentGroups)){
		deploymentGroups<-as.character(deploymentGroups)
	}else{
		deploymentGroups<-as.character(rep(NA,ndep))
	}

	if(!is.null(deploymentTags)){
		deploymentTags<-as.character(deploymentTags)
	}else{
		deploymentTags<-as.character(rep(NA,ndep))
	}

	if(!is.null(deploymentComments)){
		deploymentComments<-as.character(deploymentComments)
	}else{
		deploymentComments<-as.character(rep(NA,ndep))
	}

	deployments<-tibble(deploymentID,
					locationID,
					locationName,
					latitude,
					longitude,
					coordinateUncertainty,
					deploymentStart,
					deploymentEnd,
					setupBy,
					cameraID,
					cameraModel,
					cameraDelay,
					cameraHeight,
					cameraDepth,
					cameraTilt,
					cameraHeading,
					detectionDistance,
					timestampIssues,
					baitUse,
					featureType,
					habitat,
					deploymentGroups,
					deploymentTags,
					deploymentComments,
					)
	if(nrow(deployments)!=length(unique(deployments$deploymentID))){
		stop("'deploymentID' is required to be unique.\n")
	}
	
	return(deployments)
}

#' @title Create media
#' @description \code{create_media} creates a table of media.
#' @import magrittr
#' @importFrom lubridate as_datetime
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
#' @return A tibble of media.
#' @export
#' 
create_media<-function(	mediaID,
							deploymentID,
							timestamp=NULL,
							timestamp_date=NULL,
							timestamp_time=NULL,
							filePath,
							filePublic,
							fileMediatype,
							fileName=NULL,
							captureMethod=NULL,
							exifData=NULL,
							favorite=NULL,
							mediaComments=NULL,
							tz="Japan",
							omitduplicate=TRUE
							){
	if(any(is.na(mediaID))|any(is.na(deploymentID))|any(is.na(filePath))|any(is.na(filePublic))|any(is.na(fileMediatype))){
		stop("NAs in 'mediaID','deploymentID','timestamp','filePath','filePublic','fileMediatype' are not permitted.")
	}
	
	
	
	if(is.null(timestamp)){
		if(is.null(timestamp_date)|is.null(timestamp_time)){
			stop("'timestamp' must be specified.")
		}else{
			timestamp<-paste(timestamp_date,timestamp_time)
		}
	}else{
		timestamp<-as.character(timestamp)
	}
	
	timestamp<-timestamp%>%
				as_datetime(tz=tz)
	
	mediaID<-as.character(mediaID)
	nmed<-length(mediaID)

	deploymentID<-as.character(deploymentID)
	
	filePath<-as.character(filePath)
	
	if(is.character(filePublic)){
		if(any(!grepl("^(true|false)$",filePublic))){
			stop("'filePublic' has invalid elements.")
		}
	}else{
		filePublic<-as.logical(filePublic)
	}
	
	if(any(!grepl("^(image|video|audio)/.*$",fileMediatype))){
		stop("'fileMediatype' has invalid elements.")
	}
	fileMediatype<-as.character(fileMediatype)
	
	if(!is.null(captureMethod)){
		if(any(!grepl("^(activityDetection|timeLapse)$",captureMethod),na.rm=TRUE)){
			stop("'captureMethod' has invalid elements.")
		}
	}else{
		captureMethod<-rep(NA,nmed)
	}
	captureMethod<-factor(captureMethod,c("activityDetection","timeLapse"))
	
	if(!is.null(fileName)){
		fileName<-as.character(fileName)
	}else{
		fileName<-as.character(rep(NA,nmed))
	}
	
	if(!is.null(exifData)){
		exifData<-as.character(exifData)
	}else{
		exifData<-as.character(rep(NA,nmed))
	}
	
	if(!is.null(favorite)){	
		if(is.character(favorite)){
			if(any(!grepl("^(true|false)$",favorite),na.rm=TRUE)){
				stop("'favorite' has invalid elements.")
			}
		}
	}else{
		favorite<-rep(NA,nmed)
	}
	favorite<-as.logical(favorite)

	if(!is.null(mediaComments)){
		mediaComments<-as.character(mediaComments)
	}else{
		mediaComments<-as.character(rep(NA,nmed))
	}
	
	media<-tibble(
					mediaID,
					deploymentID,
					captureMethod,
					timestamp,
					filePath,
					filePublic,
					fileName,
					fileMediatype,
					exifData,
					favorite,
					mediaComments
					)
	
	if(omitduplicate){
		media<-distinct(media,mediaID,.keep_all=T)
	}
	
	if(nrow(media)!=length(unique(media$mediaID))){
		stop("'mediaID' is required to be unique.\n")
	}

	
	return(media)
}

#' @title Create observations
#' @description \code{create_media} creates a table of observations.
#' @import magrittr
#' @importFrom lubridate as_datetime
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
#' @return A tibble of observations.
#' @export
#' 
create_observations<-function(	observationID,
									deploymentID,
									mediaID=NULL,
									eventID=NULL,
									eventStart,
									eventEnd,
									observationLevel,
									observationType,
									cameraSetupType=NULL,
									scientificName=NULL,
									count=NULL,
									lifeStage=NULL,
									sex=NULL,
									behavior=NULL,
									individualID=NULL,
									individualPositionRadius=NULL,
									individualPositionAngle=NULL,
									individualSpeed=NULL,
									bboxX=NULL,
									bboxY=NULL,
									bboxWidth=NULL,
									bboxHeight=NULL,
									classificationMethod=NULL,
									classifiedBy=NULL,
									classificationTimestamp=NULL,
									classificationProbability=NULL,
									observationTags=NULL,
									observationComments=NULL,
									tz="Japan",
									omitduplicate=TRUE
								){
	eventStart<-eventStart%>%
		as_datetime(tz=tz)
							
	eventEnd<-eventEnd%>%
		as_datetime(tz=tz)

	if(any(is.na(observationID))|any(is.na(deploymentID))|any(is.na(eventStart))|any(is.na(eventEnd))|any(is.na(observationLevel))|any(is.na(observationType))){
		stop("NAs in 'observationID','deploymentID','eventStart','eventEnd','observationLevel','observationType' are not permitted.")
	}
	
	if(any(is.na(mediaID)&(is.na(eventID)))){
		stop("Either 'mediaID' or 'eventID' should be non-NA values.")
	}
	
	nrows<-length(observationID)
	
	observationID<-as.character(observationID)
	
	deploymentID<-as.character(deploymentID)
	
	if(!is.null(mediaID)){
		mediaID<-as.character(mediaID)
	}else{
		mediaID<-as.character(rep(NA,nrows))
	}
	
	if(!is.null(eventID)){
		eventID<-as.character(eventID)
	}else{
		eventID<-as.character(rep(NA,nrows))
	}
	
	if(any(!grepl("^(media|event)$",observationLevel))){
		stop("'observationLevel' has invalid elements.")
	}
	observationLevel<-factor(observationLevel,c("media","event"))

	if(any(!grepl("^(animal|human|vehicle|blank|unknown|unclassified)$",observationType))){
		stop("'observationType' has invalid elements.")
	}
	observationType<-factor(observationType,c("animal", "human", "vehicle", "blank", "unknown", "unclassified"))

	if(!is.null(cameraSetupType)){
		if(any(!grepl("^(setup|calibration)$",observationType),na.rm=TRUE)){
			stop("'cameraSetupType' has invalid elements.")
		}
	}else{
		cameraSetupType<-rep(NA,nrows)
	}
	cameraSetupType<-factor(cameraSetupType,c("setup","calibration"))

	if(!is.null(scientificName)){
		scientificName<-as.character(scientificName)
	}else{
		scientificName<-as.character(rep(NA,nrows))
	}

	if(!is.null(count)){
		count<-as.numeric(count)
		if(any((count<1)|(count%%1!=0),na.rm=TRUE)){
			stop("'count' should be positive integer.")
		}
	}else{
		count<-as.numeric(rep(NA,nrows))
	}
	
	if(!is.null(lifeStage)){
		if(any(!(grepl("^(adult|subadult|juvenile)$",lifeStage)|is.na(lifeStage)))){
			stop("'lifeStage' has invalid elements.")
		}
	}else{
		lifeStage<-rep(NA,nrows)
	}
	lifeStage<-factor(lifeStage,c("adult","subadult","juvenile"))
	
	if(!is.null(sex)){
		if(any(!(grepl("^(female|male)$",sex)|is.na(sex)),na.rm=TRUE)){
			stop("'sex' has invalid elements.")
		}
	}else{
		sex<-rep(NA,nrows)
	}
	sex<-factor(sex,c("female","male"))

	if(!is.null(behavior)){
		behavior<-as.character(behavior)
	}else{
		behavior<-as.character(rep(NA,nrows))
	}
	
	if(!is.null(individualID)){
		individualID<-as.character(individualID)
	}else{
		individualID<-as.character(rep(NA,nrows))
	}

	if(!is.null(individualPositionRadius)){
		individualPositionRadius<-as.numeric(individualPositionRadius)
		if(any(individualPositionRadius<0,na.rm=TRUE)){
			stop("'individualPositionRadius' should be larger than 0.")
		}
	}else{
		individualPositionRadius<-as.numeric(rep(NA,nrows))
	}
	
	if(!is.null(individualPositionAngle)){
		individualPositionAngle<-as.numeric(individualPositionAngle)
		if(any(abs(individualPositionAngle)>=90,na.rm=TRUE)){
			stop("'individualPositionAngle' should be between -90 and 90.")
		}
	}else{
		individualPositionAngle<-as.numeric(rep(NA,nrows))
	}
	
	if(!is.null(individualSpeed)){
		individualSpeed<-as.numeric(individualSpeed)
		if(any(individualSpeed<0,na.rm=TRUE)){
			stop("'individualSpeed' should be larger than 0.")
		}
	}else{
		individualSpeed<-as.numeric(rep(NA,nrows))
	}

	if(!is.null(bboxX)){
		bboxX<-as.numeric(bboxX)
		if(any((bboxX<0)|(bboxX>1),na.rm=TRUE)){
			stop("'bboxX' should be between 0 and 1.")
		}
	}else{
		bboxX<-as.numeric(rep(NA,nrows))
	}
	
	if(!is.null(bboxY)){
		bboxY<-as.numeric(bboxY)
		if(any((bboxY<0)|(bboxY>1),na.rm=TRUE)){
			stop("'bboxY' should be between 0 and 1.")
		}
	}else{
		bboxY<-as.numeric(rep(NA,nrows))
	}
	
	if(!is.null(bboxWidth)){
		bboxWidth<-as.numeric(bboxWidth)
		if(any((bboxWidth<1e-15)|(bboxWidth>1),na.rm=TRUE)){
			stop("'bboxWidth' should be between 1e-15 and 1.")
		}
	}else{
		bboxWidth<-as.numeric(rep(NA,nrows))
	}
	
	if(!is.null(bboxHeight)){
		bboxHeight<-as.numeric(bboxHeight)
		if(any((bboxHeight<1e-15)|(bboxHeight>1),na.rm=TRUE)){
			stop("'bboxHeight' should be between 1e-15 and 1.")
		}
	}else{
		bboxHeight<-as.numeric(rep(NA,nrows))
	}

	if(!is.null(classificationMethod)){
		if(any(!grepl("^(human|machine)$",classificationMethod),na.rm=TRUE)){
			stop("'classificationMethod' has invalid elements.")
		}
	}else{
		classificationMethod<-rep(NA,nrows)
	}
	classificationMethod<-factor(classificationMethod,c("human","machine"))

	if(!is.null(classifiedBy)){
		classifiedBy<-as.character(classifiedBy)
	}else{
		classifiedBy<-as.character(rep(NA,nrows))
	}
	
	if(!is.null(classificationTimestamp)){
		classificationTimestamp<-classificationTimestamp%>%
				as_datetime(tz=tz)
	}else{
		classificationTimestamp<-rep(NA,nrows)
	}
		
	if(!is.null(classificationProbability)){
		classificationProbability<-as.numeric(classificationProbability)
		if(any((classificationProbability<0)|(classificationProbability>1),na.rm=TRUE)){
			stop("'classificationProbability' should be between 0 and 1.")
		}
	}else{
		classificationProbability<-as.numeric(rep(NA,nrows))
	}

	if(!is.null(observationTags)){
		observationTags<-as.character(observationTags)
	}else{
		observationTags<-as.character(rep(NA,nrows))
	}

	if(!is.null(observationComments)){
		observationComments<-as.character(observationComments)
	}else{
		observationComments<-as.character(rep(NA,nrows))
	}
	
	observations<-tibble(	observationID,
							deploymentID,
							mediaID,
							eventID,
							eventStart,
							eventEnd,
							observationLevel,
							observationType,
							cameraSetupType,
							scientificName,
							count,
							lifeStage,
							sex,
							behavior,
							individualID,
							individualPositionRadius,
							individualPositionAngle,
							individualSpeed,
							bboxX,
							bboxY,
							bboxWidth,
							bboxHeight,
							classificationMethod,
							classifiedBy,
							classificationTimestamp=as_datetime(classificationTimestamp,tz=tz),
							classificationProbability,
							observationTags,
							observationComments
						)
	
	if(omitduplicate){
		observations<-distinct(observations,observationID,.keep_all=TRUE)
	}
	
	if(nrow(observations)!=length(unique(observations$observationID))){
		stop("'observationID' is required to be unique.\n")
	}

	
	return(observations)
}

#' @title R6 class representing Camtrap DP
#'
#' @description
#' R6 class including metadata, deployments, media and observations.
#' @Imports R6
#' @export
R6_CamtrapDP<-R6::R6Class(	"CamtrapDP",
			public = list(	resources=list(),
							profile=NULL,
							name=NULL,
							id=NULL,
							created=NA,
							title=NULL,
							contributors=list(),
							description=NULL,
							version=NULL,
							keywords=NULL,
							image=NULL,
							homepage=NULL,
							sources=NULL,
							licenses=NULL,
							bibliographicCitation=NULL,
							project=list(),
							coordinatePrecision=NULL,
							spatial=list(),
							temporal=list(start=NA,end=NA),
							taxonomic=list(),
							relatedIdentifiers=NULL,
							references=list(),
							directory=NULL,
							data=list(),
							#' @description
							#' Creates new instance of R6_CamtrapDP class.
							#' 
							initialize = function(tz="Japan",...){
								self$update_created(tz=tz)
								self$set_properties(...)
							},
							#' @description
							#' Updates timestamp of R6_CamtrapDP class.
							#' 
							update_created = function(tz="Japan"){
								self$created<-Sys.time()%>%
												as.POSIXct()%>%
												strftime("%Y-%m-%dT%H:%M:%S%z",tz=tz)
							},
							#' @description
							#' Sets properties of R6_CamtrapDP class.
							#' 
							set_properties = function(directory=getwd(),name=NULL,id=NULL,description=NULL,profile="https://raw.githubusercontent.com/tdwg/camtrap-dp/<version>/camtrap-dp-profile.json",version="1.0.1",keywords=NULL,image=NULL,homepage=NULL,bibliographicCitation=NULL,coordinatePrecision=NULL){
								self$name<-name
								self$id<-id
								self$description<-description
								self$version<-version
								self$profile<-sub("<version>",version,profile)
								self$keywords<-keywords
								self$image<-image
								self$homepage<-homepage
								self$bibliographicCitation<-bibliographicCitation
								self$coordinatePrecision<-coordinatePrecision
								self$directory<-directory
							},								
							#' @description
							#' Sets deployments of R6_CamtrapDP class.
							#' 
							set_deployments = function(	data,
														path="deployments.csv",
														profile="tabular-data-resource",
														format="csv",
														mediatype="text/csv",
														encoding="utf-8",
														schema="https://raw.githubusercontent.com/tdwg/camtrap-dp/<version>/deployments-table-schema.json"
														){
                schema<-sub("<version>",self$version,schema)
							  self$data$deployments<-data
								resource<-list(name="deployments",path=path,profile=profile,format=format,mediatype=mediatype,encoding=encoding,schema=schema)
								self$resources[[1]]<-resource
							},
							#' @description
							#' Sets media of R6_CamtrapDP class.
							#' 
							set_media = function(	data,
														path="media.csv",
														profile="tabular-data-resource",
														format="csv",
														mediatype="text/csv",
														encoding="utf-8",
														schema="https://raw.githubusercontent.com/tdwg/camtrap-dp/<version>/media-table-schema.json"
														){
							  schema<-sub("<version>",self$version,schema)
							  self$data$media<-data
								resource<-list(name="media",path=path,profile=profile,format=format,mediatype=mediatype,encoding=encoding,schema=schema)
								self$resources[[2]]<-resource
							},
							#' @description
							#' Sets observations of R6_CamtrapDP class.
							#' 
							set_observations = function(	data,
														path="observations.csv",
														profile="tabular-data-resource",
														format="csv",
														mediatype="text/csv",
														encoding="utf-8",
														schema="https://raw.githubusercontent.com/tdwg/camtrap-dp/<version>/observations-table-schema.json"
														){
							  schema<-sub("<version>",self$version,schema)
							  self$data$observations<-data
								resource<-list(name="observations",path=path,profile=profile,format=format,mediatype=mediatype,encoding=encoding,schema=schema)
								self$resources[[3]]<-resource
							},
							#' @description
							#' Sets custom data of R6_CamtrapDP class.
							#' 
							set_custom = function(name,description,data){
								resource=list(name=name,description=description,data=data)
								nlist<-length(self$resources)
								if(is.element(c("deployments","media","observations"),sapply(self$resources,"[[","name"))){
									stop("Add deployments, media and obervations prior to adding custom datasets")
								}
								self$resources[[nlist+1]]<-resource
							},
							#' @description
							#' Adds contributors of R6_CamtrapDP class.
							#' 
							add_contributors = function(contrib_table){
								if(!all(is.element(colnames(contrib_table),c("title","email","path","role","organization")))){
									stop("Columns of 'contrib_table' should be either 'title','email','path','role' or 'organization'.")
								}
								ncontrib<-nrow(contrib_table)
								for(i in 1:ncontrib){
									title<-contrib_table$title[i]
									if(is.element("email",colnames(contrib_table))){
										email<-contrib_table$email[i]
									}else{
										email<-NA
									}
									
									if(is.element("path",colnames(contrib_table))){
										path<-contrib_table$path[i]
									}else{
										path<-NA
									}
									
									if(is.element("role",colnames(contrib_table))){
										role<-as.character(contrib_table$role[i])
									}else{
										role<-NA
									}
									
									if(is.element("organization",colnames(contrib_table))){
										organization<-contrib_table$organization[i]
									}else{
										organization<-NA
									}


									contrib = list(title=title)
									if(!is.na(email)){
										contrib <- c(contrib,email=email)
									}
									
									if(!is.na(path)){
										contrib <- c(contrib,path=path)
									}
									
									if(!is.na(role)){
										if(!grepl("^(contact|principalInvestigator|rightsHolder|publisher|contributor)$",role)){
											stop("'role' is invalid.")
										}
										contrib <- c(contrib,role=role)
									}
									
									if(!is.na(organization)){
										contrib <- c(contrib,organization=organization)
									}
									if(length(self$contributors)==0){
										self$contributors<-list(contrib)
									}else{
										self$contributors<-c(self$contributors,list(contrib))
									}
								}
							},
							#' @description
							#' Add sources of R6_CamtrapDP class.
							#' 
							add_sources = function(title,path=NULL,email=NULL,version=NULL){
								if(is.null(self$sources)){
									self$sources<-list()
								}
								
								source<-list(title=title)
								
								if(!is.null(path)){
									source <- c(source,path=path)
								}
								
								if(!is.null(email)){
									source <- c(source,email=email)
								}
								
								if(!is.null(version)){
									source <- c(source,version=version)
								}
								
								self$sources<-c(self$sources,list(source))
								
							},
							#' @description
							#' Add license of R6_CamtrapDP class.
							#' 
							add_license=function(name,scope,path=NULL,title=NULL){
								if(!grepl("^(data|media)$",scope)){
									stop("'scope' is invalid.")
								}
								if(is.null(self$licenses)){
									self$licenses<-list()
								}
								license<-list(name=name)
								
								if(!is.null(path)){
									license <- c(license,path=path)
								}
								
								if(!is.null(title)){
									license <- c(license,title=title)
								}
								
								license <- c(license,scope=scope)
								
								self$licenses<-c(self$licenses,list(license))
							},
							#' @description
							#' Sets project of R6_CamtrapDP class.
							#' 
							set_project=function(title,samplingDesign,captureMethod,individualAnimals,observationLevel,id=NULL,acronym=NULL,description=NULL,path=NULL){
								if(!grepl("^(simpleRandom|systematicRandom|clusteredRandom|experimental|targeted|opportunistic)$",samplingDesign)){
									stop("'samplingDesign' is invalid.")
								}
								
								if(any(!grepl("^(activityDetection|timeLapse)$",captureMethod))){
									stop("'captureMethod' is invalid.")
								}
								
								if(!is.logical(individualAnimals)){
									if(!grepl("^(true|false)$",individualAnimals)){
										stop("'individualAnimals' is invalid.")
									}
									individualAnimals<-as.logical(individualAnimals)
								}
								
								if(any(!grepl("^(media|event)$",observationLevel))){
									stop("'observationLevel' is invalid.")
								}
								
								if(!is.null(id)){
									self$project<-c(self$project,id=id)
								}
								
								self$project<-c(self$project,title=title)
								
								if(!is.null(acronym)){
									self$project<-c(self$project,acronym=acronym)
								}
								
								if(!is.null(description)){
									self$project<-c(self$project,description=description)
								}
								
								if(!is.null(path)){
									self$project<-c(self$project,path=path)
								}
								
								self$project<-c(self$project,samplingDesign=samplingDesign)
								
								self$project<-c(self$project,captureMethod=list(as.list(captureMethod)))
								
								self$project<-c(self$project,individualAnimals=individualAnimals)
								
								self$project<-c(self$project,observationLevel=list(as.list(observationLevel)))
							},								
							#' @description
							#' Sets spatial and temporal of R6_CamtrapDP class.
							#' 
							set_st=function(){
								if(is.null(self$data$deployments)){
									stop("'deployments' should be registered.")
								}
								lat.range<-range(self$data$deployments$latitude)
								long.range<-range(self$data$deployments$longitude)
								bbox<-c(lat.range[1],long.range[1],lat.range[2],long.range[2])
								coordinates<-array(NA,dim=c(1,5,2))
								coordinates[1,,1]<-lat.range[c(1,2,2,1,1)]
								coordinates[1,,2]<-long.range[c(1,1,2,2,1)]
								self$spatial<-list(type="Polygon",bbox=bbox,coordinates=coordinates)
								
								time.start<-min(self$data$deployments$deploymentStart)
								time.end<-max(self$data$deployments$deploymentEnd)
								self$temporal$start<-time.start
								self$temporal$end<-time.end
							},
							#' @description
							#' Sets taxonomic of R6_CamtrapDP class.
							#' @importFrom taxadb get_ids
							#' @importFrom tibble tibble
							#' @importFrom dplyr mutate left_join
							#' @Imports magrittr
							#' 
							set_taxon=function(taxonDB="itis",taxonDBurl="https://www.itis.gov/"){
								if(is.null(self$data$observations)){
									stop("'observations' should be registered.")
								}
								sciname<-self$data$observations$scientificName
								unique.sciname<-unique(sciname)
								taxonIDtable<-tibble::tibble(sciname=unique.sciname)%>%
									dplyr::mutate(id=taxadb::get_ids(sciname,taxonDB,format="bare"))
								taxonIDtableclean<-na.omit(taxonIDtable)
								ntaxa<-nrow(taxonIDtableclean)

								taxonIDjoin<-tibble(sciname=sciname)%>%
								  dplyr::left_join(taxonIDtable,by="sciname")

								taxonID<-taxonIDjoin$id

								self$taxonomic<-vector("list",ntaxa)
								for(i in 1:ntaxa){
									self$taxonomic[[i]]<-list()
									self$taxonomic[[i]]$taxonID<-taxonIDtableclean$id[i]
									self$taxonomic[[i]]$taxonIDReference<-taxonDBurl
									self$taxonomic[[i]]$scientificName<-taxonIDtableclean$sciname[i]
								}
							},
							#' @description
							#' Add relatedIdentifiers of R6_CamtrapDP class.
							#' 
							add_relatedIdentifiers=function(relationType,relatedIdentifier,relatedIdentifierType,resourceTypeGeneral=NULL){
								if(!grepl("^(IsCitedBy|Cites|IsSupplementTo|IsSupplementedBy|IsContinuedBy|Continues|IsNewVersionOf|IsPreviousVersionOf|IsPartOf|HasPart|IsPublishedIn|IsReferencedBy|References|IsDocumentedBy|Documents|IsCompiledBy|Compiles|IsVariantFormOf|IsOriginalFormOf|IsIdenticalTo|HasMetadata|IsMetadataFor|Reviews|IsReviewedBy|IsDerivedFrom|IsSourceOf|Describes|IsDescribedBy|HasVersion|IsVersionOf|Requires|IsRequiredBy|Obsoletes|IsObsoletedBy)$",relationType)){
									stop("'relationType' is invalid.")
								}
								
								if(!is.null(resourceTypeGeneral)){
									if(!grepl("^(Audiovisual|Book|BookChapter|Collection|ComputationalNotebook|ConferencePaper|ConferenceProceeding|DataPaper|Dataset|Dissertation|Event|Image|InteractiveResource|Journal|JournalArticle|Model|OutputManagementPlan|PeerReview|PhysicalObject|Preprint|Report|Service|Software|Sound|Standard|Text|Workflow|Other)$",resourceTypeGeneral)){
										stop("'resourceTypeGeneral' is invalid.")
									}
								}
								
								if(!grepl("^(ARK|arXiv|bibcode|DOI|EAN13|EISSN|Handle|IGSN|ISBN|ISSN|ISTC|LISSN|LSID|PMID|PURL|UPC|URL|URN|w3id)$",relatedIdentifierType)){
									stop("'relatedIdentifierType' is invalid.")
								}
								
								if(is.null(self$relatedIdentifiers)){
									self$relatedIdentifiers<-list()
								}
								
								relatedIdentifier<-list(relationType=relationType,relatedIdentifier=relatedIdentifier)
								if(!is.null(resourceTypeGeneral)){
									relatedIdentifier<-c(relatedIdentifier, resourceTypeGeneral=resourceTypeGeneral)
								}
								
								relatedIdentifier<-c(relatedIdentifier,relatedIdentifierType=relatedIdentifierType)
								
								self$relatedIdentifiers<-c(self$relatedIdentifiers,list(relatedIdentifier))
							},
							#' @description
							#' Add references of R6_CamtrapDP class.
							#' 
							add_references=function(reference){
								self$references<-c(self$references,reference)
							},
							#' @description
							#' Exports \code{camtrapdp} object and datapackage.
							#' @Import camtrapdp
							#' @importFrom jsonlite toJSON
							#' @importFrom readr write_csv
							#' @return \code{camtrapdp} object 
							#' 
							out_camtrapdp=function(write=FALSE,directory=NULL){
								outname<-c("resources","profile","name","id","created","title","contributors","description","version","keywords","image","homepage","sources","licences","bibliographicCitation","project","coordinatePrecision","spatial","temporal","taxonomic","relatedIdentifiers","references","directory","data")
								nout<-length(outname)
								dp<-vector("list",nout);names(dp)<-outname
								for(i in 1:nout){
									dp[[outname[i]]]<-self[[outname[i]]]
								}
								class(dp)<-c("camtrapdp","datapackage","list")
								attr(dp,"version")<-self$version
								if(write){
									if(is.null(directory)){
										stop("'directory' is required.\n")
									}
									dp_write<-dp
									dp_write$data<-NULL
									dp_write$directory<-NULL
									if(!dir.exists(directory)){
										dir.create(directory, recursive = TRUE)
									}
									dp_JSON<-jsonlite::toJSON(dp_write,pretty=NULL,null="null",na="null",auto_unbox=TRUE)
									write(dp_JSON,file=file.path(directory,"datapackage.json"))
									
									resources.name<-sapply(dp$resources,"[[","name")
									deployments.file<-dp$resources[[which(resources.name=="deployments")]]$path
									readr::write_csv(dp$data$deployments,file.path(directory,deployments.file),na="")
									
									media.file<-dp$resources[[which(resources.name=="media")]]$path
									readr::write_csv(dp$data$media,file.path(directory,media.file),na="")
									
									observations.file<-dp$resources[[which(resources.name=="observations")]]$path
									readr::write_csv(dp$data$observations,file.path(directory,observations.file),na="")
									
									cat("Datapackage was saved to disk.\n")
								}
								return(dp)
							},
							#' @description
							#' Import metadata from a list.
							#' 
							import_metadata=function(metadata0){
								listname<-names(metadata0)
								listname<-listname[listname!="resources"]
								listname.valid<-listname[is.element(listname,names(self))]
								nlist<-length(listname.valid)
								for(i in 1:nlist){
									self[[listname.valid[i]]]<-metadata0[[listname.valid[i]]]
								}
							}
						)
)


