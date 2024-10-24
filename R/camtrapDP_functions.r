#' @title Create deployments
#' @description \code{create_deployments} creates a table of deployments.
#' @import magrittr
#' @import lubridate
#' @import tibble
#' @return A tibble of deployments.
#' @param deploymentID Unique identifier of the deployment
#' @param latitude Latitude of the deployment location in decimal degrees, using the WGS84 datum
#' @param longitude Longitude of the deployment location in decimal degrees, using the WGS84 datum
#' @param deploymentStart Date and time at which the deployment was started
#' @param deploymentStart_date Date at which the deployment was started
#' @param deploymentStart_time Time at which the deployment was started
#' @param deploymentEnd Date and time at which the deployment was ended
#' @param deploymentEnd_date Date at which the deployment was ended
#' @param deploymentEnd_time Time at which the deployment was ended
#' @param locationID Identifier of the deployment location
#' @param locationName Name given to the deployment location
#' @param coordinateUncertainty Horizontal distance from the given latitude and longitude describing the smallest circle containing the deployment location
#' @param setupBy Name or identifier of the person or organization that deployed the camera
#' @param cameraID Identifier of the camera used for the deployment
#' @param cameraModel Manufacturer and model of the camera
#' @param cameraDelay Predefined duration after detection when further activity is ignored
#' @param cameraHeight Height at which the camera was deployed
#' @param cameraDepth Depth at which the camera was deployed
#' @param cameraTilt Angle at which the camera was deployed in the vertical plane
#' @param cameraHeading Angle at which the camera was deployed in the horizontal plane
#' @param detectionDistance Maximum distance at which the camera can reliably detect activity
#' @param timestampIssues true if timestamps in the media resource for the deployment are known to have unsolvable issues
#' @param baitUse true if bait was used for the deployment
#' @param featureType Type of the feature associated with the deployment
#' @param habitat Short characterization of the habitat at the deployment location
#' @param deploymentGroups Deployment groups associated with the deployment
#' @param deploymentTags Tags associated with the deployment
#' @param deploymentComments Comments or notes about the deployment
#' @param tz Deployment time zone
#' @export
#' 
create_deployments<-function( deploymentID,
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
			deploymentStart<-paste(deploymentStart_date,deploymentStart_time)%>%as.POSIXlt(tz=tz)
		}
	}else{
		deploymentStart<-as.character(deploymentStart)
	}
	
	if(is.null(deploymentEnd)){
		if(is.null(deploymentEnd_date)|is.null(deploymentEnd_time)){
			stop("'deploymentEnd must be specified.'")
		}else{
			deploymentEnd<-paste(deploymentEnd_date,deploymentEnd_time)%>%as.POSIXlt(tz=tz)
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
#' @import lubridate
#' @import tibble
#' @importFrom dplyr distinct
#' @return A tibble of media.
#' @param mediaID Unique identifier of the media file
#' @param deploymentID Identifier of the deployment the media file belongs to
#' @param timestamp Date and time at which the media file was recorded
#' @param timestamp_date Date at which the media file was recorded
#' @param timestamp_time Time at which the media file was recorded
#' @param filePath URL or relative path to the media file, respectively for externally hosted files or files that are part of the package
#' @param filePublic false if the media file is not publicly accessible
#' @param fileMediatype Mediatype of the media file. Expressed as an IANA Media Type
#' @param fileName Name of the media file
#' @param captureMethod Method used to capture the media file
#' @param exifData EXIF data of the media file
#' @param favorite true if the media file is deemed of interest
#' @param mediaComments Comments or notes about the media file
#' @param tz Time zone of the media file was recorded
#' @param omitduplicate true if duplicate exclusion 
#' @export
#' 
create_media<-function( mediaID,
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
			timestamp<-paste(timestamp_date,timestamp_time)%>%as.POSIXlt(tz=tz)
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
#' @import lubridate
#' @import tibble
#' @importFrom dplyr distinct
#' @return A tibble of observations.
#' @param observationID Unique identifier of the observation
#' @param deploymentID Identifier of the deployment the observation belongs to
#' @param mediaID Identifier of the media file that was classified
#' @param eventID Identifier of the event the observation belongs to
#' @param eventStart Date and time at which the event started
#' @param eventStart_date Date at which the event started
#' @param eventStart_time Time at which the event started
#' @param eventEnd Date and time at which the event ended
#' @param eventEnd_date Date at which the event ended
#' @param eventEnd_time Time at which the event ended
#' @param observationLevel Level at which the observation was classified
#' @param observationType Type of the observation
#' @param cameraSetupType Type of the camera setup action associated with the observation
#' @param scientificName Scientific name of the observed individual
#' @param count Number of observed individuals
#' @param lifeStage Age class or life stage of the observed individual
#' @param sex Sex of the observed individual
#' @param behavior Dominant behavior of the observed individual
#' @param individualID Identifier of the observed individual
#' @param individualPositionRadius Distance from the camera to the observed individual identified by individualID
#' @param individualPositionAngle Angular distance from the camera view centerline to the observed individual identified by individualID
#' @param individualSpeed Average movement speed of the observed individual identified by individualID
#' @param bboxX Horizontal position of the top-left corner of a bounding box
#' @param bboxY Vertical position of the top-left corner of a bounding box
#' @param bboxWidth Width of a bounding box
#' @param bboxHeight Height of the bounding box
#' @param classificationMethod Method used to classify the observation
#' @param classifiedBy Name or identifier of the person or AI algorithm that classified the observation
#' @param classificationTimestamp Date and time of the classification
#' @param classificationProbability Degree of certainty of the classification
#' @param observationTags Tags associated with the observation
#' @param observationComments Comments or notes about the observation
#' @param tz Time zone of observation
#' @param omitduplicate true if duplicate exclusion
#' @export
#' 
create_observations<-function(	observationID,
									deploymentID,
									mediaID=NULL,
									eventID=NULL,
									eventStart=NULL,
									eventStart_date=NULL,
									eventStart_time=NULL,
									eventEnd=NULL,
									eventEnd_date=NULL,
									eventEnd_time=NULL,
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
  
  if(is.null(eventStart)){
    if(is.null(eventStart_date)|is.null(eventStart_time)){
      stop("'eventStart must be specified.'")
    }else{
      eventStart<-paste(eventStart_date,eventStart_time)%>%as.POSIXlt(tz=tz)
    }
  }else{
    eventStart<-as.character(eventStart)
  }
  
  if(is.null(eventEnd)){
    if(is.null(eventEnd_date)|is.null(eventEnd_time)){
      stop("'eventEnd must be specified.'")
    }else{
      eventEnd<-paste(eventEnd_date,eventEnd_time)%>%as.POSIXlt(tz=tz)
    }
  }else{
    eventEnd<-as.character(eventEnd)
  }

  eventStart<-eventStart%>%
		as_datetime(tz=tz)
							
	eventEnd<-eventEnd%>%
		as_datetime(tz=tz)

	if(any(is.na(observationID))|any(is.na(deploymentID))|any(is.na(observationLevel))|any(is.na(observationType))){
		stop("NAs in 'observationID','deploymentID','observationLevel','observationType' are not permitted.")
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
#' @import R6
#' @export 
R6_CamtrapDP<-R6::R6Class(	"CamtrapDP",
			public = list(
			  #' @field resources is the package data resources
			  #' @field profile of the resource
			  #' @field name Identifier of the resource
			  #' @field id A property reserved for globally unique identifiers e.g., UUID and DOI
			  #' @field created The datetime on which this Data Package was created
			  #' @field title Title of this Data Package
			  #' @field contributors The people or organizations who contributed to this Data Package
			  #' @field description Description of this Data Package
			  #' @field version The version of this Data Package
			  #' @field keywords Keywords of this Data Package
			  #' @field image A URL or Path of an image for this Data Package
			  #' @field homepage A URL for the home on the web that is related to this Data Package
			  #' @field sources A row sources for this Data Package
			  #' @field licenses The licenses under which the Data Package is provided
			  #' @field bibliographicCitation A bibliographical reference for the resource
			  #' @field project Camera trap project or study that originated this Data package
			  #' @field coordinatePrecision Least precise coordinate precision of the deployments.latitude and deployments.longitude
			  #' @field spatial Spatial coverage of this Data Package, expressed as GeoJSON
			  #' @field temporal Temporal coverage of this Data Package
			  #' @field taxonomic Taxonomic coverage of this Data Package, based on the unique observations.scientificName
			  #' @field relatedIdentifiers Identifiers of resources related to this Data Package
			  #' @field references List of references related to this Data Package
			  #' @field directory Directory of this Data Package
			  #' @field data Observation, Media and Deployments which consist this Data Package
			        resources=list(),
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
							#' @param tz Time zone.
							#' @param ... parameters
							#' 
							initialize = function(tz="Japan",...){
								self$update_created(tz=tz)
								self$set_properties(...)
							},
							#' @description
							#' Updates timestamp of R6_CamtrapDP class.
							#' @param tz Time zone.
							#' 
							update_created = function(tz="Japan"){
								self$created<-Sys.time()%>%
												as.POSIXct()%>%
												strftime("%Y-%m-%dT%H:%M:%S%z",tz=tz)
							},
							#' @description
							#' Sets properties of R6_CamtrapDP class.
							#' @param directory Directory of datapackage.
							#' @param name Identifier of the resource
							#' @param id A property reserved for globally unique identifiers e.g., UUID and DOI
							#' @param description Description of this Data Package
							#' @param version The version of this Data Package
							#' @param profile Profile of the resource
							#' @param keywords Keywords of this Data Package
							#' @param image A URL or Path of an image for this Data Package
							#' @param homepage A URL for the home on the web that is related to this Data Package
							#' @param bibliographicCitation A bibliographical reference for the resource
							#' @param coordinatePrecision Least precise coordinate precision of the deployments.latitude and deployments.longitude
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
							#' @param data Deployments dataset.
							#' @param path Path or URL to the data file
							#' @param profile Profile of deployments
							#' @param format Format of deployments data
							#' @param mediatype Media type of deployments data
							#' @param encoding Encpding of deployments data
							#' @param schema URL of the used Camtrap DP Table Schema version
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
							#' @param data Media dataset.
							#' @param path Path or URL to the data file
							#' @param profile Profile of media data
							#' @param format Format of media data
							#' @param mediatype Mediatype of media data
							#' @param encoding Encoding of media data
							#' @param schema URL of the used Camtrap DP Table Schema version
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
							#' @param data Observations dataset.
							#' @param path Path of URL to the data file
							#' @param profile Profile of observations
							#' @param format Format of observation data
							#' @param mediatype Mediatype of observation data
							#' @param encoding Encoding of observation data
							#' @param schema URL of the used Camtrap DP Table Schema version
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
							#' @param name Name of dataset.
							#' @param description Description of dataset.
							#' @param data Custom dataset.
							#' 
							set_custom = function(name,description,data){
								resource=list(name=name,description=description,data=data)
								nlist<-length(self$resources)
								if(!all(is.element(c("deployments","media","observations"),sapply(self$resources,"[[","name")))){
									stop("Add deployments, media and obervations prior to adding custom datasets")
								}
								self$resources[[nlist+1]]<-resource
							},
							#' @description
							#' Adds contributors of R6_CamtrapDP class.
							#' @param contrib_table data frame or tibble of contributors including title, email, path, role and organizaiton.
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
							#' @param title Title of sources.
							#' @param path Path or URL to the source
							#' @param email An email address
							#' @param version The version of this Data Package
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
							#' @param name Name of license.
							#' @param scope scope of license ("data" or "media").
							#' @param path A URL or path to the details of license
							#' @param title A title of license
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
							#' @param title Title of project.
							#' @param samplingDesign Sampling design. Either "(simpleRandom|systematicRandom|clusteredRandom|experimental|targeted|opportunistic)".
							#' @param captureMethod Capture method. Either "(activityDetection|timeLapse)".
							#' @param individualAnimals Logical indicating whether the individuals are recognized.
							#' @param observationLevel Observation level. Either "(media|event)".
							#' @param id Unique identifier of the project
							#' @param acronym Project acronym
							#' @param description Description of the project
							#' @param path Project website
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
							#' @param taxonDB Name of taxon data base passed to \code{taxadb::get_ids()}.
							#' @importFrom taxadb get_ids
							#' @importFrom tibble tibble
							#' @importFrom dplyr mutate left_join
							#' @import magrittr
							#' 
							set_taxon=function(taxonDB="gbif"){
								if(is.null(self$data$observations)){
									stop("'observations' should be registered.")
								}
								sciname<-self$data$observations$scientificName
								unique.sciname<-unique(sciname)%>%na.omit()
								taxonIDtable<-tibble::tibble(sciname=unique.sciname)%>%
									dplyr::mutate(id=taxadb::get_ids(sciname,taxonDB,format="uri"))
								taxonHigher<-taxadb::filter_name(unique.sciname,taxonDB)
								colnames(taxonHigher)[2]<-"sciname"
								ntaxa<-length(unique.sciname)

								taxonIDjoin<-tibble(sciname=unique.sciname)%>%
								  dplyr::left_join(taxonIDtable,by="sciname")%>%
								  left_join(taxonHigher,by="sciname")

								taxonID<-taxonIDjoin$id

								self$taxonomic<-vector("list",ntaxa)
								for(i in 1:ntaxa){
									self$taxonomic[[i]]<-list()
									self$taxonomic[[i]]$taxonID<-taxonIDjoin$id[i]
									self$taxonomic[[i]]$scientificName<-taxonIDjoin$sciname[i]
									self$taxonomic[[i]]$taxonRank<-taxonIDjoin$taxonRank[i]
									self$taxonomic[[i]]$kingdom<-taxonIDjoin$kingdom[i]
									self$taxonomic[[i]]$phylum<-taxonIDjoin$phylum[i]
									self$taxonomic[[i]]$class<-taxonIDjoin$class[i]
									self$taxonomic[[i]]$order<-taxonIDjoin$order[i]
									self$taxonomic[[i]]$family<-taxonIDjoin$family[i]
									self$taxonomic[[i]]$family<-taxonIDjoin$genus[i]
								}
							},
							#' @description
							#' Add relatedIdentifiers of R6_CamtrapDP class.
							#' @param relationType Type of relation. Either "^(IsCitedBy|Cites|IsSupplementTo|IsSupplementedBy|IsContinuedBy|Continues|IsNewVersionOf|IsPreviousVersionOf|IsPartOf|HasPart|IsPublishedIn|IsReferencedBy|References|IsDocumentedBy|Documents|IsCompiledBy|Compiles|IsVariantFormOf|IsOriginalFormOf|IsIdenticalTo|HasMetadata|IsMetadataFor|Reviews|IsReviewedBy|IsDerivedFrom|IsSourceOf|Describes|IsDescribedBy|HasVersion|IsVersionOf|Requires|IsRequiredBy|Obsoletes|IsObsoletedBy)$".
							#' @param relatedIdentifier Related identifier.
							#' @param relatedIdentifierType Type of related identifier. Either "^(ARK|arXiv|bibcode|DOI|EAN13|EISSN|Handle|IGSN|ISBN|ISSN|ISTC|LISSN|LSID|PMID|PURL|UPC|URL|URN|w3id)$".
							#' @param resourceTypeGeneral General type of the related resource
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
							#' @param reference Reference of data
							#' 
							add_references=function(reference){
								self$references<-c(self$references,reference)
							},
							#' @description
							#' Exports \code{camtrapdp} object and datapackage.
							#' @param write If TRUE, datapackage is written to \code{directory}.
							#' @param directory Path to the directory where new data packages will save
							#' @import camtrapdp
							#' @importFrom jsonlite toJSON
							#' @importFrom readr write_csv
							#' @return \code{camtrapdp} object 
							#' 
							out_camtrapdp=function(write=FALSE,directory=NULL){
								outname<-c("resources","profile","name","id","created","title","contributors","description","version","keywords","image","homepage","sources","licenses","bibliographicCitation","project","coordinatePrecision","spatial","temporal","taxonomic","relatedIdentifiers","references","directory","data")
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
							#' @param metadata0 List of metadata.
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
