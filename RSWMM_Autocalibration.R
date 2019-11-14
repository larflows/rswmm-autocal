# RSWMM autocalibrator version 0.1
# Last updated 11/13/2019 by Daniel Philippus at Colorado School of Mines.
#
# This script automatically runs iterations to calibrate SWMM.

DEBUG <- FALSE

SWMMExe <-function(swmm,Input,Report,Output,iType,vIndex){
  swmm=paste('"',swmm,'"',sep="")
  Input=paste('"', Input,'"',sep="")
  Report=paste('"', Report,'"',sep="")
  Output=paste('"', Output,'"',sep="")
  
  # Running executable file
  
  system(paste(swmm,Input,Report,Output,sep=" "),show.output.on.console=T)
  return(Output)
}
GetObjectsSWMM<-function(Output){
  BinaryFile = file(Output,"rb")
  Status ={}
  seek(BinaryFile,1*4,"start")
  #the version number of the engine (currently 51000)
  Status$VersionNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  #a code number for the flow units that are in effect where
  
  #0 = CFS
  #1 = GPM
  #2 = MGD
  #3 = CMS
  #4 = LPS
  #5 = LPD
  Status$UnitCode = readBin(BinaryFile, integer(), n = 1, size = 4)
  #the number of subcatchments in the project reported on
  Status$SubCatchNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  #the number of nodes in the project reported on
  Status$NodesNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  #the number of links in the project reported on
  Status$LinksNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  #the number of pollutants in the project
  Status$PollutantsNum = readBin(BinaryFile, integer(), n = 1, size = 4)
  seek(BinaryFile,-2*4,"end") 
  #Check the error status (0 or 1)
  Status$Err= readBin(BinaryFile, integer(), n = 1, size = 4)
  if (Status$Err == 0){
    print ("There is no error in the file")
  }else{
    print("find the error in the file")
  }
  
  seek(BinaryFile,-6*4,"end")
  Status$ObjectID = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Getting Object ID
  seek(BinaryFile,Status$ObjectID,"start")
  #subcatchment ID names
  Status$SubCatchName = {}
  if (Status$SubCatchNum>0){
    for (i in 1:Status$SubCatchNum){
      Status$SubCatchName[i]= readChar(BinaryFile,readBin(BinaryFile, integer(), n = 1, size = 4),useBytes = FALSE) 
      
    }
    
  }
  else {
    print ("No Subcatchment")
  }
  #node ID names
  Status$NodesName = {}
  if (Status$NodesNum>0){
    for (i in 1:Status$NodesNum){
      Status$NodesName[i]= readChar(BinaryFile,readBin(BinaryFile, integer(), n = 1, size = 4),useBytes = FALSE) 
      
    }
    
  }
  else {
    print ("No Node")
  }
  
  #link ID names
  Status$LinksName = {}
  if (Status$LinksNum>0){
    for (i in 1:Status$LinksNum){
      Status$LinksName[i]= readChar(BinaryFile,readBin(BinaryFile, integer(), n = 1, size = 4),useBytes = FALSE) 
      
    }
    
  }
  else {
    print ("No Link")
  }
  #Pollutant names
  Status$PollutantsName = {}
  if (Status$PollutantsNum>0){
    for (i in 1:Status$PollutantsNum){
      Status$PollutantsName[i]= readChar(BinaryFile,readBin(BinaryFile, integer(), n = 1, size = 4),useBytes = FALSE) 
    }
    
  }
  else {
    print ("No Pollutant")
  }
  #pollutant concentration units codes
  #0 for mg/L
  #1 for ug/L
  #2 for counts/L.
  Status$PollutantsUnitCode = {}
  for (i in 1:Status$PollutantsNum){
    Status$PollutantsUnitCode[i]= readBin(BinaryFile, integer(), n = 1, size = 4) 
  }
  #Getting Object Properties
  seek(BinaryFile,-5*4,"end")
  Status$ObjectProperties = readBin(BinaryFile, integer(), n = 1, size = 4)
  seek(BinaryFile,Status$ObjectProperties,"start")
  #Number of subcatchment properties saved (Currently equal to 1)
  Status$NumofSubCatchSaved = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each subcatchment property saved (Currently equal to 1 for subcatchment area)
  Status$CodeNumSubCatchSaved = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Value of each property for each subcatchment (Subcatchment area (ac or ha) for each subcatchment)
  Status$SubCatchArea=readBin(BinaryFile,what="double",n=Status$SubCatchNum,size=4);
  #Number of node properties saved (Currently equal to 3)
  Status$NumofNodesSaved = readBin(BinaryFile, integer(), n = 1, size = 4)
  
  #Code number of each node property saved
  #0 (node type code)
  #2 (node invert elevation)
  #3 (node max. depth)
  
  
  Status$CodeNumNodesSaved = readBin(BinaryFile, integer(), n = Status$NumofNodesSaved, size = 4)
  
  Status$TypeofNodes = {}
  if(Status$NodesNum>0){
    NodeType=readBin(BinaryFile,what="double",n=Status$NumofNodesSaved*Status$NodesNum,size=4)
    
    Status$CodeNumNodesSaved=NodeType[seq(from=1,by=3,to=length(NodeType))]
    #0 = Junction
    
    #1 = Outfall
    
    #2 = Storage
    
    #3 = Divider
    for (i in 1 :length(Status$CodeNumNodesSaved)){
      if (Status$CodeNumNodesSaved[i]==0){
        Status$TypeofNodes[i]="Junction"}
      else if (Status$CodeNumNodesSaved[i]==1){
        Status$TypeofNodes[i]="Outfall"}
      else if (Status$CodeNumNodesSaved[i]==2){
        Status$TypeofNodes[i]="Storage"}
      else if (Status$CodeNumNodesSaved[i]==3){
        Status$TypeofNodes[i]="Divider"}
    }
    
    
    Status$InvertElevation=NodeType[seq(from=2,by=3,to=length(NodeType))] 
    Status$MaximumDepth=NodeType[seq(from=3,by=3,to=length(NodeType))] 
    
  }
  
  #Number of node properties saved (Currently equal to 5)
  Status$NumofLinksSaved = readBin(BinaryFile, integer(), n = 1, size = 4)
  
  #Code number of each link property saved
  #0 (link type code)
  #4 (upstream invert offset)
  #4 (downstream invert offset)
  #3 (link max. depth) 
  #5 (link length)
  Status$CodeNumLinkSaved = readBin(BinaryFile, integer(), n = Status$NumofLinksSaved, size = 4)
  Status$TypeofLinks = {}
  if(Status$LinksNum>0){
    LinkType=readBin(BinaryFile,what="double",n=Status$NumofLinksSaved*Status$LinksNum,size=4)
    Status$CodeNumLinksSaved=LinkType[seq(from=1,by=5,to=length(LinkType))]
    #0 = Conduit 
    #1 = Pump 
    #2 = Orifice 
    #3 = Weir 
    #4 = Outlet
    for (i in 1 :length(Status$CodeNumLinksSaved)){
      if (Status$CodeNumLinksSaved[i]==0){
        Status$TypeofLinks[i]="Conduit"}
      else if (Status$CodeNumLinksSaved[i]==1){
        Status$TypeofLinks[i]="Pump"}
      else if (Status$CodeNumLinksSaved[i]==2){
        Status$TypeofLinks[i]="Orifice"}
      else if (Status$CodeNumLinksSaved[i]==3){
        Status$TypeofLinks[i]="Weir"}
      else if (Status$CodeNumLinksSaved[i]==4){
        Status$TypeofLinks[i]="Outlet"}
      #4 (upstream invert offset)
      Status$UpstreamInvertOffset=LinkType[seq(from=2,by=5,to=length(LinkType))]
      #4 (downstream invert offset)
      Status$DownstreamInvertOffset=LinkType[seq(from=3,by=5,to=length(LinkType))]
      #3 (link max. depth) 
      Status$MaximumDepth=LinkType[seq(from=4,by=5,to=length(LinkType))]
      #5 (link length)
      Status$LinkLength=LinkType[seq(from=5,by=5,to=length(LinkType))]
      
      
      
    }
  }
  
  #Getting Reporting Variables
  #Number of subcatchment variables (currently 8 + number of pollutants).
  Status$NumofSubCatchVariables = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each subcatchment variable
  #0 for rainfall (in/hr or mm/hr),
  #1 for snow depth (in or mm),
  #2 for evaporation loss (in/day or mm/day),
  #3 for infiltration losses (in/hr or mm/hr),
  #4 for runoff rate (flow units),
  #5 for groundwater outflow rate (flow units),
  #6 for groundwater water table elevation (ft or m),
  #7 for unsaturated zone moisture content (fraction)
  #8 for runoff concentration of first pollutant,
  #...
  #7 + N for runoff concentration of N-th pollutant.
  Status$CodeNumSubCatch = readBin(BinaryFile, integer(), n = Status$NumofSubCatchVariables, size = 4)
  #Number of node variables (currently 6 + number of pollutants)
  Status$NumNodesVariables = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each node variable
  #0 for depth of water above invert (ft or m),
  #1 for hydraulic head (ft or m),
  #2 for volume of stored + ponded water (ft3 or m3),
  #3 for lateral inflow (flow units),
  #4 for total inflow (lateral + upstream) (flow units),
  #5 for flow lost to flooding (flow units),
  #6 for concentration of first pollutant,
  #...
  #5 + N for concentration of N-th pollutant.
  Status$CodeNumNode = readBin(BinaryFile, integer(), n = Status$NumNodesVariables, size = 4)
  #Number of link variables (currently 5 + number of pollutants)
  Status$NumLinksVariables = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each link variable:
  #0 for flow rate (flow units),
  #1 for flow depth (ft or m),
  #2 for flow velocity (ft/s or m/s),
  #3 for flow volume (ft3 or m3)
  #4 for fraction of conduit's area filled or setting for non-conduits
  #5 for concentration of first pollutant,
  #...
  #4 + N for concentration of N-th pollutant.
  Status$CodeNumLink = readBin(BinaryFile, integer(), n = Status$NumLinksVariables, size = 4)
  #Number of system-wide variables (currently 14)
  Status$NumSystemVariables = readBin(BinaryFile, integer(), n = 1, size = 4)
  #Code number of each system-wide variable:
  #0 for air temperature (deg. F or deg. C),
  #1 for rainfall (in/hr or mm/hr),
  #2 for snow depth (in or mm),
  #3 for evaporation + infiltration loss rate (in/hr or mm/hr),
  #4 for runoff flow (flow units),
  #5 for dry weather inflow (flow units),
  #6 for groundwater inflow (flow units),
  #7 for RDII inflow (flow units),
  #8 for user supplied direct inflow (flow units),
  #9 for total lateral inflow (sum of variables 4 to 8) (flow units),
  #10 for flow lost to flooding (flow units),
  #11 for flow leaving through outfalls (flow units),
  #12 for volume of stored water (ft3 or m3),
  #13 for evaporation rate (in/day or mm/day)
  Status$CodeNumSystems = readBin(BinaryFile, integer(), n = Status$NumSystemVariables, size = 4)
  #Getting Reporting Interval
  Status$BytesPerPeriod= 2*4 +(Status$SubCatchNum*(Status$NumofSubCatchVariables) + 
                                 Status$NodesNum*(Status$NumNodesVariables) +
                                 Status$LinksNum*(Status$NumLinksVariables) + Status$NumSystemVariables)*4;
  seek(BinaryFile,-3*4,"end")
  Status$ReportingPeriods= readBin(BinaryFile, integer(), n = 1, size = 4)
  Status$BinaryFile = BinaryFile
  seek(BinaryFile,-4*4,"end")
  Status$ComputedResults= readBin(BinaryFile, integer(), n = 1, size = 4) 
  #Writing function to get table of computed results
  #ObjectType is the type of the Object that you want to see the result (i.e., Subcatchment, node, conduit, or system)
  #Object ID is the name of object to see the result (i.e., S1 or J2)
  #Index is the position of one object among other objects
  #Codenum is the code number of each variable from abover list
  #Time period
  
  
  return(Status)
}
getSWMMResult<-function(headObj,iType,iIndex,vIndex,period){  
  SUBCATCH=0
  NODE     = 1;
  LINK     = 2;
  SYS      = 3;
  
  
  f=headObj$BinaryFile
  StartPos=headObj$ComputedResults
  
  off = StartPos + period*(headObj$BytesPerPeriod) + 2*4;
  if ( iType == SUBCATCH )
  {
    off = off+ 4*(iIndex*(headObj$NumofSubCatchVariables) + vIndex);
  }
  else if (iType == NODE)
  {
    off = off+ 4*(headObj$SubCatchNum*(headObj$NumofSubCatchVariables) +
                    iIndex*(headObj$NumNodesVariables) + vIndex);
  }
  else if (iType == LINK)
  {
    off = off+ 4*(headObj$SubCatchNum*(headObj$NumofSubCatchVariables) +
                    headObj$NodesNum*(headObj$NumNodesVariables) +
                    iIndex*(headObj$NumLinksVariables) + vIndex);
  }
  else if (iType == SYS)
  {
    off = off+ 4*(headObj$SubCatchNum*(headObj$NumofSubCatchVariables) +
                    headObj$NodesNum*(headObj$NumNodesVariables) +
                    headObj$LinksNum*(headObj$NumLinksVariables) + vIndex);
    
  }
  
  seek(f,off,"start")
  Status=readBin(f,what="double",size=4,n=1)
  return(Status)
} 
getSWMMTimes<-function(headObj){
  #gets the time stamps of the SWMM results in binary file
  f=headObj$BinaryFile
  seek(f,headObj$ComputedResults,"start")
  
  headObj$SWMMTimes<-array(NaN,headObj$ReportingPeriods)
  if(headObj$ReportingPeriods>0){
    for(i in 1:headObj$ReportingPeriods){
      
      headObj$SWMMTimes[i]<-readBin(f,what="double",size=8,n=1)
      #      if(i<100){
      #       print(headObj$SWMMTimes[i])
      #    }
      seek(f,headObj$BytesPerPeriod-8,"current")
    }
  }else{
    stop("No time steps listed in SWMM output file.")
    
  }
  #Convert SWMM times to R POSIXlt datetimes
  headObj$SWMMTimes<-headObj$SWMMTimes*86400.0+as.POSIXct(strptime("12/30/1899", format="%m/%d/%Y",tz="GMT"))#edit 2/10/2012 to force GMT time zone rather than locale specific
  return(headObj)
}
getSWMMTimeSeriesData<-function(headObj,iType,nameInOutputFile,vIndex){
  
  if(iType==0){
    iIndex=(0:(headObj$SubCatchNum-1))[headObj$SubCatchName==nameInOutputFile]
  }else if(iType==1){
    iIndex=(0:(headObj$NodesNum-1))[headObj$NodesName==nameInOutputFile]  
  }else if(iType==2){
    iIndex=(0:(headObj$LinksNum-1))[headObj$LinksName==nameInOutputFile]
  }else if(iType==3){
    
    iIndex=0
  }
  Status=array(NaN,headObj$ReportingPeriods)
  for(period in 0:(-1+headObj$ReportingPeriods)){
    #browser()
    # Separated for debugging
    result <- getSWMMResult(headObj=headObj,iType=iType,iIndex=iIndex,vIndex=vIndex,period=period)
    Status[period+1]=result
  }
  return(Status)
}

Aggregate<-function(Q, headObj){
  Sim <- aggregate(Q, 
                   list(hour=cut(as.POSIXct(headObj$SWMMTimes, format="%Y-%m-%d %H:%M:%S",tz="GMT"), "day")),
                   mean)
  library(readxl)
  CalibrationData = read_excel("Observed.xlsx")
  #Creat Observed Data in the same format of Simulated
  Obs <- aggregate(CalibrationData$Flow, 
                   list(hour=cut(as.POSIXct(CalibrationData$Date, format="%Y-%m-%d %H:%M:%S",tz="GMT"), "day")),
                   mean)
  Obs[is.na(Obs)] <- 0
  Obs = Obs[-1,]
  Sim[is.na(Sim)] <- 0
  Average = {}
  Average$Date = Obs$hour
  Average$Sim = Sim$x
  Average$Obs = Obs$x

  return(Average)
}
#Calculating summary statistics

PerfomrmStatistic<-function(Average){
  require(hydroGOF)
  sim = Average$Sim
  obs = Average$Obs
  MeanError = me(sim,obs)
  MeanSquaredError = mse(sim,obs)
  IndexAgreement = d(sim,obs)
  IndexAgreementTimesMinus1 = -1*IndexAgreement
  Nashsutcliffe = NSE(sim, obs)
  NashsutcliffeTimesMinus1 = -1*Nashsutcliffe
  
  PercentBias = pbias(sim,obs)
  absBias = abs(PercentBias)
  
  linearCorrelation = cor(sim,obs)
  linearCorrelationTimesMinus1 = -1*linearCorrelation
  negativeRSquared = (-1) * linearCorrelation^2
  output=data.frame(MeanError,MeanSquaredError,IndexAgreementTimesMinus1,NashsutcliffeTimesMinus1,Nashsutcliffe, PercentBias,
                    absBias, linearCorrelationTimesMinus1, negativeRSquared)
  return(output)
}

# Creating input files with uncertain parameters and name it as $1$ $2$ $3$
ReadSWMMOptFile<-function(SWMMOptFile){
  SWMMOpt=readLines(con = SWMMOptFile, n = -1L, ok = TRUE, warn = TRUE,
                    encoding = "unknown")
  return(SWMMOpt)
}

#Replace Optimization parameters to the Input File

OptimizationFile<-function(Optimization){
  Optimization = read.csv(file=Optimization, header = TRUE, sep = ",", quote="\"", dec=".",
                          fill = TRUE, comment.char="",stringsAsFactors = FALSE)
  parameters = Optimization[,2]
  replacementCodes = Optimization[,1]
  return (Optimization)
}

replaceCodesInTemplateFile<-function(SWMMOpt,parameters, replacementCodes,File){
  #Optimization = read.csv(file=Optimization, header = TRUE, sep = ",", quote="\"", dec=".",
  #fill = TRUE, comment.char="",stringsAsFactors = FALSE)
  #parameters = Optimization[,2]
  #replacementCodes = Optimization[,1]
  
  for(i in 1:length(parameters)){
    
    SWMMOpt=sub(replacementCodes[i], parameters[i], SWMMOpt,fixed=TRUE)
  }
  writeLines(SWMMOpt, con = File, sep = "\n", useBytes = FALSE)
  
  return(SWMMOpt)
}

#Defining Minim, initial, and Maximum Vallues for uncertain parameters
ParametersBound<-function(ParametersFile){
  Bounds = read.csv(file=ParametersFile, header = TRUE, sep = ",", quote="\"", dec=".",
                    fill = TRUE, comment.char="",stringsAsFactors = FALSE)
  #initial = Bounds$Initial
  #Minimum = Bounds$Minimum
  #Maximum = Bounds$Maximum
  
  return(Bounds)
}

Objectivefunction<-function(SWMMOptFile,x,OutFile,swmm,Timeseries,StatParameters){
  swmm_dir <- "swmm-files/"
  print(paste("Iteration:", iteration))
  SWMMOpt= ReadSWMMOptFile(SWMMOptFile)
  
  Input=paste(swmm_dir, OutFile,"_", iteration,'.inp',sep="")
  ReplaceCodes<<- replaceCodesInTemplateFile(SWMMOpt,x,as.matrix(Bounds["Code"]),Input)
  Report=paste(swmm_dir, OutFile, "_", iteration,'.rpt',sep="")
  Output=paste(swmm_dir, OutFile, "_", iteration,'.out',sep="")
  SWMMExe(swmm,Input,Report,Output)
  headObj = GetObjectsSWMM (Output)
  headObj = getSWMMTimes (headObj)
  Q = getSWMMTimeSeriesData(headObj,iType,nameInOutputFile,vIndex)
  Average = Aggregate(Q, headObj)
  # Average = read.zoo(data.frame(Average))
  require(hydroGOF)
  sim = Average$Sim
  obs = Average$Obs
  MeanError = me(sim,obs)
  MeanSquaredError = mse(sim,obs)
  IndexAgreement = d(sim,obs)
  IndexAgreementTimesMinus1 = -1*IndexAgreement
  Nashsutcliffe = NSE(sim, obs)
  NashsutcliffeTimesMinus1 = -1*Nashsutcliffe
  
  PercentBias = pbias(sim,obs)
  absBias <- abs(PercentBias)
  
  linearCorrelation = cor(sim,obs)
  linearCorrelationTimesMinus1 = -1*linearCorrelation
  negativeRSquared <- (-1) * linearCorrelation^2
  
  output1=data.frame(iteration = iteration, Nashsutcliffe,PercentBias,R2 = linearCorrelation ^ 2, t(x))
  
  # library(xlsx)
  # 
  # for (i in iteration){
  #   write.xlsx(output1,paste("Combinations",i,".xlsx",sep=""))}
  
  filename <- "Combinations.csv"
  # Don't use the header if the file already exists and has stuff in it
  use_header <- !(file.exists(filename) && file.size(filename) > 0)
  # write.csv ignores append and col.names, so need to use write.table directly
  write.table(output1, filename, append = T, col.names = use_header, sep = ",", row.names = F)
  
  SummaryStatistics = PerfomrmStatistic(Average)
  perfStatsToUse=as.numeric(SummaryStatistics[StatParameters])
  summaryRow=unlist(c(iteration,x,perfStatsToUse))
  #write.xlsx(summaryRow[3:5],"summaryRow.xlsx",sheetName = paste("iteration",iteration),append = TRUE)
  names(summaryRow)= c("iteration",paste("Parameter",t(Bounds["Code"]),sep=""),StatParameters)
  iteration<<-iteration+1
  # print(summaryRow)
  printrow <- data.frame(NSE = Nashsutcliffe, PBias = PercentBias, R2 = linearCorrelation^2)
  printrow[1,] <- signif(printrow[1,], 3)
  print(printrow)
  # nsga2 seems to expect just the last 3 things
  return (summaryRow[(length(summaryRow)-2):length(summaryRow)])
  # nsga2 tries to minimize the parameters, so it needs to be set up with lower is better
}

OptimizationFunction <- function(SWMMOptFile,OutFile,swmm,Timeseries,StatParameters,initial,lower,upper,
                               generations = 200, popsize = 100){
  if (file.exists("Combinations.csv")) file.remove("Combinations.csv")
  if (!dir.exists("swmm-files")) dir.create("swmm-files")
  optimOpt={}
  optimOpt$OutFile = OutFile
  optimOpt$SWMMOptFile = SWMMOptFile
  optimOpt$swmm = swmm
  optimOpt$Timeseries = 'Q = getSWMMTimeSeriesData(headObj,iType,nameInOutputFile,vIndex)'
  optimOpt$StatParameters = StatParameters
  optimOpt$lower = lower
  optimOpt$upper = upper
  library(mco)
  out= nsga2(Objectivefunction,
             idim=length(optimOpt$lower),
             odim=length(optimOpt$StatParameters),
             OutFile= optimOpt$OutFile,
             SWMMOptFile = optimOpt$SWMMOptFile,
             swmm = optimOpt$swmm,
             Timeseries = optimOpt$Timeseries,
             StatParameters = optimOpt$StatParameters,
             generations=generations,
             lower.bounds=as.double(optimOpt$lower),
             upper.bounds=as.double(optimOpt$upper),
             constraints=NULL,popsize = popsize)
}

default_sorter <- function(data, n, nsecol = "Nashsutcliffe", pbcol = "PercentBias") {
  # Return index of the top result, based on the top n nses and the lowest absolute pbias among those
  nse <- data[[nsecol]]
  pbias <- data[[pbcol]]
  ordering <- order(nse, decreasing = T)
  apb <- abs(pbias)
  n <- if (n <= length(nse)) n else length(nse)
  ordering[order(apb[ordering[1:n]])[1]]
}

find_optimal <- function(n = 30, file = "Combinations.csv", sorter = default_sorter) {
  # Sorter: takes file contents and n and returns top result
  data <- read.csv(file)
  sorter(data, n)
}

# Testing
testRun <- function() {
  iteration <<- 1
  OptimizationFunction(SWMMOptFile,
                       OutFile,
                       swmm,
                       Timeseries,
                       StatParameters,
                       initial,
                       lower,
                       upper)
}
if (DEBUG) testRun()