# SWG_functions, compiled by Niels Stehwien, 27.2.18

#####################################################################################
mymkdir <-function(dir){
  if (!file.exists(dir)){
  dir.create(file.path(dir))
  }
}


######################################################
#written by Fabian Tomaschek, edited by Lea Hofmaier 

read_praat_boundaries_SWG <- function (PARAM, checkTiernames = FALSE)  {

filename = paste(PARAM$ROOTFOLDER, PARAM$TG.FN,sep='')

DF.tg.in = read.table(filename, fill = NA, stringsAsFactors=FALSE, encoding = 'UTF-8')

  #Find the lines of the tiers 
  wo.tiers= grep("name", DF.tg.in$V1) #get line numbers # this line of code takes the colum V2 in DF.tG.in puts it into the grep function, and checks where it is located. 
  
  Tiers= DF.tg.in$V3[wo.tiers] #extract lines
  DF.tg.in = DF.tg.in[-(1:(wo.tiers[1]-1)),] #reduce original 
  wo.tiers= c(grep("name", DF.tg.in$V1),nrow(DF.tg.in)) 

  #add tier names to columns
  DF.tg.in$Tier = ''
  
  for(it in 1:(length(wo.tiers)-1)){
    DF.tg.in$Tier[wo.tiers[it]:wo.tiers[it+1]] = Tiers[it]
  }

  #extract name and time
  wo.int = which(DF.tg.in$V1 == "intervals") #all intervals

  #save cells to new DF
  WB = data.frame(
    w.start = as.numeric(DF.tg.in$V3[wo.int+1]),
    w.end = as.numeric(DF.tg.in$V3[wo.int+2]), 
    A = DF.tg.in$V3[wo.int+3],
    Tier = DF.tg.in$Tier[wo.int+1],
    stringsAsFactors = FALSE
  )
  
  #WB$A = gsub('"', '', WB$A)
  
if(checkTiernames){
return(unique(WB$Tier))
} else {


###############################################################
####### create new variables
slowa = WB[grep(PARAM$wordtier, WB$Tier),]
slowa$A[which(is.na(slowa$A))] = '<P>'

slowa$WordNr = 1:nrow(slowa)


 
slowa$Word = slowa$A
slowa$A = NULL
slowa$Tier = NULL

#######################################################################
## Segmenttier
segmenty = WB[grep(PARAM$segmenttier, WB$Tier),]
colnames(segmenty)[1:2] = c('s.start', 's.end')
  
    
segmenty$SegmentNr = 1:nrow(segmenty) # within word
segmenty$Segment = segmenty$A

segmenty$nSegments = 0
segmenty$SegmentNrWord = 0
segmenty$WordNr = NA
segmenty$Word = ''

segmenty$w.start = 0
segmenty$w.end = 0 
segmenty$Wordstart = FALSE

segmenty$A = NULL
segmenty$Tier = NULL

segmenty$AlignerSegments = ''

  

###############################
## add word-names to segments using the time interval
   for (iword in 1:nrow(slowa) ) {  
	start1 = slowa$w.start[iword]-0.00001 #beginning of the interval
	end1 = slowa$w.end[iword]#+0.00001 #end of the interval
	replaced = which(segmenty$s.start >= start1 &	segmenty$s.end <= end1)
	
	segmenty$nSegments[replaced] = length(replaced)
	segmenty$SegmentNrWord[replaced] = 1:length(replaced)
	segmenty$WordNr[replaced] = slowa$WordNr[iword]

	segmenty$Word[replaced] = slowa$Word[iword]

	
	segmenty$w.start[replaced] = slowa$w.start[iword]
	segmenty$w.end[replaced] = slowa$w.end[iword]
	segmenty$Wordstart[replaced[1]] = TRUE

	segmenty$AlignerSegments[replaced] = paste(gsub('_', '',slowa$Segment[replaced]), collapse = '_')
      }#for
 

segmenty$s.start[segmenty$s.start<0] = 0
segmenty$w.start[segmenty$w.start<0] = 0

segmenty$Segment.Duration = segmenty$s.end - segmenty$s.start

segmenty = segmenty[!is.na(segmenty$WordNr),]

return(segmenty)
}

}

 
 





####################################################################################
# newTextGridfromBoundries.R
# written by Niels Stehwien

newTextGridfromBoundries <- function(TG, signalduration, GRIDFOLDER, FILENAME){
  
  ########## open new textgrid file 
  #open file and delete everything in it
  fileConn<-file(paste(GRIDFOLDER, FILENAME, '.TextGrid', sep =""), open = 'w',encoding = 'UTF-8');
  close(fileConn) 
  
  # open file in append mode and leave it open
  fileConn<-file(paste(GRIDFOLDER, FILENAME, '.TextGrid', sep =""), open = 'a', encoding = 'UTF-8')
  
  ############ first lines
  writeLines(c('File type = "ooTextFile"','Object class = "TextGrid"', ''), fileConn) 
  writeLines(c('xmin = 0'), fileConn) 
  writeLines(c(paste('xmax = ', signalduration, sep = "")), fileConn)
  writeLines(c('tiers? <exists> '), fileConn)
  writeLines(c(paste('size = ', length(TG), sep ="")), fileConn)
  writeLines(c('item []:'), fileConn)
  
  
  ############  write tiers into the file         
  for (iTier in 1:length(TG)){ #go through Tiers
    z = 0#counter for intervalls
    
    Intervalls = TG[[iTier]]  #pass all intervalls in one tier
        colnames(Intervalls) <- c('s.start', 's.end', 'content')

    #### Insert tier information  
    writeLines(c(paste('    item [', iTier, ']:', sep = "")), fileConn)          
    writeLines(c('        class = "IntervalTier"'), fileConn)          
    writeLines(c(paste('        name = "', names(TG)[iTier] ,'"', sep ="")), fileConn)  
    writeLines(c('        xmin = 0'), fileConn) 
    writeLines(c(paste('        xmax = ', signalduration, sep = "")), fileConn)   
    writeLines(c(paste('        intervals: size = ', dim(Intervalls)[1], sep = "")), fileConn) 
    
    
    ############# insert boundaries found by the aligner     
    for (iIntervall in 1:dim(Intervalls)[1]){
      
      writeLines(c(paste('        intervals [', iIntervall, ']:', sep = "")), fileConn) 
      writeLines(c(paste('            xmin = ', Intervalls$s.start[iIntervall], sep = "")), fileConn) 
      writeLines(c(paste('            xmax = ', Intervalls$s.end[iIntervall], sep = "")), fileConn) 
      writeLines(c(paste('            text = "', Intervalls$content[iIntervall], '"', sep = "")), fileConn) 
      
    } #iIntervalls
    
    
  }#iTier
  
  close(fileConn)
  
}

# file renaming and relocating funtion
my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir))
    dir.create(todir, recursive = TRUE)
  file.rename(from = from,  to = to)
}




################################################################
## FUNCTIONS TO CREATE PHRASES FOR CORRECTION


ChangeTime = function(TT) {
SS = as.numeric(unlist(strsplit(TT, split = ':')))
H = SS[1]
M = SS[2]
S = SS[3]
out = H*60*60 + M*60 + S
return(out)
}




CreatePhrasesForAlignment  = function(DIR, DIR.out, wav.fn){

setwd(DIR)

library(tuneR)
library(foreach)
#library(doMC)
#registerDoMC(detectCores())


		    W = tuneR::readWave(wav.fn)
		    
            textgrid.fn = gsub('.wav' ,'.TextGrid', wav.fn, fixed = TRUE)
            
            PARAM = list(TG.FN = textgrid.fn, ROOTFOLDER = paste0(getwd(),'/'), wordtier = 'SWG', segmenttier = 'STY')
            DF.TG = read_praat_boundaries_SWG(PARAM)[,c('w.start', 'w.end', 'SegmentNr', 'Word', 'Segment')]
            
            #clean words for search algo       
            DF.TG$Word2 = DF.TG$Word        
            DF.TG$Word2 = gsub('#', '', DF.TG$Word2)
            DF.TG$Word2 = gsub('ê', 'e', DF.TG$Word2)
            DF.TG$Word2 = gsub('â', 'a', DF.TG$Word2)
            DF.TG$Word2 = gsub('ã', 'a', DF.TG$Word2)
            DF.TG$Word2 = gsub('ô', 'o', DF.TG$Word2)
            DF.TG$Word2 = gsub('ß', 'ss', DF.TG$Word2) 
            DF.TG$Word2 = gsub('ä', 'ae', DF.TG$Word2)
            DF.TG$Word2 = gsub('ö', 'oe', DF.TG$Word2)
            DF.TG$Word2 = gsub('ü', 'ue', DF.TG$Word2)

            
       
            DF.TG2 = DF.TG[nchar(DF.TG$Word)>0 & !grepl("-mhm-", DF.TG$Word),]
        
	Win = readWave(wav.fn)
            
       OUT = foreach (itarget = 1:nrow(DF.TG2), .combine = rbind) %dopar% {
            cat('.')
            #find target word
            info.target = DF.TG2[itarget,]



            #take audio
            anfang = info.target$w.start * Win@samp.rate
            ende = info.target$w.end * Win@samp.rate

            Wout = Win[anfang:ende]
            
            #save audio
            
            out.fn = paste0(gsub(".wav", "", wav.fn,fixed = TRUE), '_', rownames(info.target))

        
            writeWave(Wout, filename =paste0(DIR.out, out.fn, '.wav'), extensible = TRUE)
            
            #save text                      
            write.table(info.target$Word2, file =paste0(DIR.out, out.fn, '.txt'), quote = FALSE, col.names=FALSE, row.names=FALSE, fileEncoding = 'UTF-8')
            
        }#foreach 
        cat('\n')
}#function

########################################################################################################

GetTargetWords = function(DIR, speaker.dir, TARGET = 'AIS1|AIS2'){
setwd(paste0(DIR, '/SWG_Data/', speaker.dir))

files = list.files()
dat.fn =  grep('TOK.csv', files, value = TRUE, fixed = TRUE)

    if(length(dat.fn)>0){
        # TOK.tots = read.csv( grep('tots', files, value = TRUE) )
        TOK = read.csv(dat.fn, stringsAsFactors = FALSE )


        DF.targets = TOK[grep(TARGET, TOK$VAR_string),]
        DF.targets$beg_hms = as.character(DF.targets$beg_hms)
        DF.targets$SWG_string = trimws(as.character(DF.targets$SWG_string))

        #clean words for search algo
        DF.targets$SWG_string2 = gsub('#', '', DF.targets$SWG_string)
        DF.targets$SWG_string2 = gsub('ê', 'e', DF.targets$SWG_string2)
        DF.targets$SWG_string2 = gsub('â', 'a', DF.targets$SWG_string2)
        DF.targets$SWG_string2 = gsub('ã', 'a', DF.targets$SWG_string2)
        DF.targets$SWG_string2 = gsub('ô', 'o', DF.targets$SWG_string2)
        DF.targets$SWG_string2 = gsub('ä', 'ae', DF.targets$SWG_string2)
        DF.targets$SWG_string2 = gsub('ü', 'ue', DF.targets$SWG_string2)
        DF.targets$SWG_string2 = gsub('ö', 'oe', DF.targets$SWG_string2)
        

        
        DF.targets$SWG_string2 = gsub('ß', 'ss', DF.targets$SWG_string2) 
        
        DF.targets$Occurrence = 1
        
        DF.targets$FN = gsub('-TOK.csv', '',  grep('TOK.csv', files, value = TRUE, fixed = TRUE), fixed = TRUE)
        
        table.phrases = xtabs(~beg_hms+SWG_string2, data = DF.targets)
        
        take.table.phrases =  data.frame(which(table.phrases>1, arr.ind = TRUE), stringsAsFactors = FALSE)

            for(itake in 1:nrow(take.table.phrases)){
                info = take.table.phrases[itake, , drop = FALSE]
                
                wo = which(DF.targets$beg_hms == rownames(info) & DF.targets$SWG_string2 == colnames(table.phrases)[info$SWG_string2])
                
                DF.targets$Occurrence[wo] = 1:length(wo)
           }
            
        
        return(DF.targets)

    } else {
    cat('No TOK file found for: ', speaker.dir, '\n')
    return(c())
}
}


########################################################################################################
CopyTokenFiles = function (DIR){
setwd(DIR)
setwd('./TokenFiles/')
FNS = list.files(recursive = TRUE)
take.fns = grep('TOK', FNS, value = TRUE)

    for(ifn in 1:length(take.fns)){


    take = gsub('.csv', '', gsub('TokenFiles/|-TOK|-tots', '', take.fns[ifn]), fixed = TRUE)

    file.copy(take.fns[ifn], paste0(DIR, '/SWG_Data/', take, '/'), overwrite = TRUE)

    }

          
          
}
########################################################################################################



CreateTrigrams = function(DIR, SPEAKERS){
library(foreach)
#library(doMC)
#registerDoMC(detectCores())



# ispeaker = grep('S010-82-I-1-Angela', SPEAKER)
# ispeaker = 4

X =   foreach (ispeaker = 1:length(SPEAKERS)) %dopar%{
    setwd(paste0(DIR, '/SWG_Data/'))
    load(SPEAKERS[ispeaker], verbose = TRUE)

    cat('PROCESSING: #', ispeaker, ' ',  SPEAKERS[ispeaker], '\n')
    
    what.speaker = PROTOCOL_S0_S1$FN[1]

    setwd(paste0(DIR, '/SWG_Data/', what.speaker, '/Step2_allinierte_Phrasen/'))

    files1 = sort(PROTOCOL_S0_S1$FN.out)
    files1 = files1[!files1 == "NA"]
    
    files.in.dir = list.files()
    
    files =  files1[ paste0(files1, '.TextGrid') %in% files.in.dir]
    
    ifile = grep('166_231', files)
    
##########################        
#        X = foreach (ifile =  1:length(files)) %dopar% {
for(ifile in 1:length(files)){
#         cat('.')
            dir.in = paste0(DIR, '/SWG_Data/' , what.speaker, '/Step1_Phrasen/')
            
            FILENAME = files[ifile]
            
            W = tuneR::readWave(paste0(dir.in, FILENAME, '.wav'))
            
                        
            PARAM=list(
            ROOTFOLDER = paste0(DIR, '/SWG_Data/' , what.speaker, '/Step2_allinierte_Phrasen/'),
            TG.FN = paste0(FILENAME, '.TextGrid'),
            wordtier = 'words',
            segmenttier = 'segments'
            )
            
             if(file.exists(paste0(PARAM$ROOTFOLDER, PARAM$TG.FN))){
                
                TG.IN = unique(read_praat_boundaries_SWG(PARAM)[,c('w.start', 'w.end', 'Word', 'WordNr', 's.start', 's.end', 'Segment', 'SegmentNr')])
            
                DF.word = unique(TG.IN[,c('Word', 'WordNr')])
                
                ## get position of target word
                info = PROTOCOL_S0_S1[PROTOCOL_S0_S1$FN.out == FILENAME,][1,]
                
                
                string = gsub('ä', 'ae', info$SWG_string2)
                string = gsub('ö', 'oe', string)
                string = gsub('ü', 'ue', string)
                
                wo.string = DF.word$WordNr[DF.word$Word == string]#check position of string on textgrid
                
                #if exact match is not found, use grep
                if(length(wo.string) == 0){
                wo.string = DF.word$WordNr[grep(string, DF.word$Word)]#check position of string on textgrid
                }
                            
                Occ = as.numeric(unlist(strsplit(FILENAME, split = '_'))[5])#access occurrence position

                                    
                if(length(wo.string)>0  & length(wo.string) >= Occ ){#run only of a word was found
                
                which.pos = wo.string [Occ]
                            
                
                #### FIRST POSITION
                if( which.pos != min(DF.word$WordNr)){von = which.pos-1} else {von = which.pos}
                if( which.pos != max(DF.word$WordNr)){bis = which.pos+1} else { bis = which.pos}
                
                
                
                ## inspect whether <P> is part of invetory, if yes, expand
                
                if (DF.word$Word[DF.word$WordNr == von] == '<P>' & von != min(DF.word$WordNr)){von = von-1}
                if (DF.word$Word[DF.word$WordNr == bis] == '<P>' & bis != max(DF.word$WordNr)){bis = bis+1}
                
                
                tmp.tg = TG.IN[TG.IN$WordNr %in% von:bis,]
                
                rm(list = c('von', 'bis'))
                
                tmp.tg$Word[tmp.tg$WordNr == which.pos] = paste0(tmp.tg$Word[tmp.tg$WordNr == which.pos], '_corr')    
                    
            
                    word.info = unique(tmp.tg[,c('w.start', 'w.end', 'Word')])
                    
                    ## write wave
                    start = min(word.info$w.start) 
                    ende = max(word.info$w.end)
                    
                    take = (start*W@samp.rate):(ende*W@samp.rate)
                    take = take[!take == 0]
                    W.sub = W[take]
                    
                    writeWave(W.sub, filename = paste0(DIR, '/SWG_Data/' , what.speaker, '/Step3_3Gramme/TG_', FILENAME, '.wav') )
                    
                
                    ## write textgrid
                
                    # align with zero
                    MIN = min(tmp.tg$w.start)
                    tmp.tg$w.start = tmp.tg$w.start - MIN
                    tmp.tg$w.end = tmp.tg$w.end - MIN
                    tmp.tg$s.start = tmp.tg$s.start - MIN
                    tmp.tg$s.end = tmp.tg$s.end - MIN
                    
                    tmp.tg$w.end[nrow(tmp.tg)] = length(W.sub@left)/W.sub@samp.rate
                    tmp.tg$s.end[nrow(tmp.tg)] = length(W.sub@left)/W.sub@samp.rate
                    
                    word.info = unique(tmp.tg[,c('w.start', 'w.end', 'Word')])
                    segment.info = unique(tmp.tg[,c('s.start', 's.end', 'Segment')])
                    
                    newTextGridfromBoundries(
                    TG = list(words = word.info, segments = segment.info),
                    signalduration = length(W.sub@left)/W.sub@samp.rate,  
                    GRIDFOLDER = paste0(DIR, '/SWG_Data/' , what.speaker, '/Step3_3Gramme/TG_'),
                    FILENAME = FILENAME)
                    
                    }#if wo.string > 0 
                
        } #if file exist
       } #foreach(ifile)

    gc(reset = TRUE)

        cat('\n')
    } #for(ispeaker)

}

###################################################################################


CopyToBins = function(DIR, Names){

iname = 2
for (iname in 1:length(Names)){
    setwd(DIR)
    Name = Names[iname]
    speaker.dirs = list.files (path = 'SWG_Data/', pattern = Name)

    for (irepetition in 1:length(speaker.dirs)){
        speaker.dir = speaker.dirs[irepetition]
        setwd(paste0(DIR, '/SWG_Data/', speaker.dir, '/Step3_3Gramme'))
        loc.dir = getwd()
        
        files = list.files(pattern = 'wav')
        
        if(length(files)>0){
            Ordner = as.character(1:ceiling(length(files)/10))
            Ordner[1:9] = paste0('0', Ordner[1:9])

            take = c(seq(from = 1, to = length(files), by = 10), (length(files)+1))
            
            for(itake in 1:(length(take) - 1)){
                    dir.out = paste0(loc.dir, '/P', Ordner[itake], '/')
                    mymkdir(dir.out)
                    
                    von = take[itake]
                    bis = take[itake+1]-1
                
                    file.copy(files[von:bis], dir.out, overwrite = TRUE)
                    file.remove(files[von:bis])
                    
                    file.copy(gsub('.wav', '.TextGrid', files[von:bis]), dir.out, overwrite = TRUE)
                    file.remove(gsub('.wav', '.TextGrid', files[von:bis]))
                    
            
                }# for itake
            } else {
            cat('No files found for: ', files, '\n')
            }#if
        }#for irepetition
    }#for iname

}#function


######################################################################################################
### ALIGNER FUNCTIONS


# KEC
# Created by Tomaschek, Fabian 01.11.2012


##########################################################################################
#downsample for aligner

# downsample.forAl <- function (ROOTFOLDER, ALIGNSUB, FILENAME){
#   library(tuneR)  
#   ### create a new directory for mono files if there is non
#   if (!file.exists(paste(ROOTFOLDER, ALIGNSUB, sep = ""))){dir.create(paste(ROOTFOLDER, ALIGNSUB, sep = ""))}  
#   #### read the file and take the speech signal
#   wav = readWave(paste(ROOTFOLDER, "/", FILENAME, '.wav', sep = ""))
#   sig =  downsample(wav, 16000) # choose speech signal and downsample
#   #### save the monofile
#   writeWave(sig, paste(ROOTFOLDER, "TMP/", FILENAME, '.wav', sep = ""))
# }

downsample.forAligner <- function (ROOTFOLDER, ALIGNSUB, FILENAME){
  ### create a new directory for mono files if there is non
    system( #get access to the system (LINUX)    
      paste("sox ", ROOTFOLDER, FILENAME, ".wav -c 1 -e a-law -r 16000 ", ALIGNSUB, FILENAME, ".wav", sep="") ##10.06.15, FT geändert
          )#system

}

excludeHashtags <- function (TRANSFOLDER, FILENAME, ALIGNFOLDER, encoding =  encoding){
  cat('EXCLUDING HASHTAGS\n#################\n')
  #written by Florence Lopez, 22.06.15
  
  FILENAME2 = gsub ('_CLEAN', '', FILENAME)
  
  text <- scan(paste0(TRANSFOLDER, FILENAME2, ".txt"), character(0), quote=NULL, encoding = encoding)#latin1

  print(text)
  
  text2 = text[!grepl('^#', text)]
  text2 = gsub('#','', text2)
  text2 = gsub('ê','e', text2)
  text2 = gsub('ã','a', text2)
  text2 = gsub('â','a', text2)
  text2 = gsub('ô','o', text2)
  text2 = gsub('ß','ss', text2)
  text2 = gsub('ä','ae', text2)
  text2 = gsub('ö','oe', text2)
  text2 = gsub('ü','ue', text2)
  text2 = gsub('-','', text2, fixed = TRUE)
  text2 = gsub('!','', text2, fixed = TRUE)
  text2 = gsub('?','', text2, fixed = TRUE)
  text2 = gsub('.','', text2, fixed = TRUE)
  text2 = gsub(',','', text2, fixed = TRUE)
  text2 = gsub("'",'', text2, fixed = TRUE)
  text2 = gsub('"','', text2, fixed = TRUE)

  
print(text2)

  # abspeichern als Fliesstext
  fileConn<-file(paste0(ALIGNFOLDER, FILENAME, ".txt"), encoding = "UTF-8")
  writeLines(text2, fileConn)
  close(fileConn)

  
}  

#####################################################################################
mymkdir <-function(dir){
  if (!file.exists(dir)){
  dir.create(file.path(dir))
  }
}


##########################################################################################
#run aligner
run.aligner <- function(ALIGNERHOME, TRANSFOLDER, ALIGNFOLDER, FILENAME,READWORDS=TRUE, READSEGMENTS=TRUE, encoding = "UTF8"){

  

      
# write non-canonical forms into text and copy to TMP folder 
excludeHashtags(TRANSFOLDER, FILENAME, ALIGNFOLDER, encoding = encoding)
  
  cat('\n\nRunning Aligner at Word level\n############################\n')
 if(READWORDS==TRUE){        
      system( #get access to the system (LINUX)    
      paste("export PATH=\"", ALIGNERHOME, ":$PATH\";",
            " export ALIGNERHOME=\"", gsub('//','', gsub('bin|deu', '', ALIGNERHOME), fixed = TRUE), "\";", #create the environment for the aligner 
            " export ALANG=\"deu\";",
            " export LANG=C;",
            "cd ",  ALIGNFOLDER, ";", #go to the directory with the downsampled files
#             " ", ALIGNERHOME, "Alignphones ",  FILENAME,  ".wav;", 
            " ", ALIGNERHOME, "Alignwords ", FILENAME, ".wav",
            sep = "") #paste
          )#system
 }
  cat('\n\nRunning Aligner at Segment level\n############################\n')
if(READSEGMENTS==TRUE){
      system( #get access to the system (LINUX)    
      paste("export PATH=\"", ALIGNERHOME, ":$PATH\";",
            " export ALIGNERHOME=\"", gsub('//','', gsub('bin|deu', '', ALIGNERHOME), fixed = TRUE), "\";", #create the environment for the aligner 
            " export ALANG=\"deu\";",
            " export LANG=C;",
            "cd ",  ALIGNFOLDER, ";", #go to the directory with the downsampled files
            " ", ALIGNERHOME, "Alignphones ",  FILENAME,  ".wav;", 
#             " ", ALIGNERHOME, "Alignwords ", FILENAME, ".wav",
            sep = "") #paste
          )#system  
}  
}

#########################################################################################################################
run.aligner.postwordlevel <- function(
ALIGNERHOME, 
CORRGRIDFOLDER, 
TMPFOLDER, 
FILENAME,
GRIDFOLDER,WORDLEVELNAME = 'words.korr', SEGMENTLEVELNAME ='segments', IGNORE = '<P>', 
MC = 4, DurThreshold = 0.15){
  library(stringr)
  library(car)
  library(audio)
  library(tuneR)
  library(signal)
  library(foreach)
  #library(doMC)
   #registerDoMC(MC)
   
#########################  
  ##read word level annotation
  GRIDFILE = paste0(FILENAME, '.TextGrid')
  WB = read_praat_boundaries_postcorrection(CORRGRIDFOLDER,GRIDFILE)
  wordannot = WB[WB$Tier %in% WORDLEVELNAME,]
  rownames(wordannot) = 1:nrow(wordannot)
  wordannot$dur = wordannot$s.end - wordannot$s.start
  wordannot$durtake = wordannot$dur>DurThreshold
  WAV = downsample(readWave(paste(ROOTFOLDER, FILENAME, '.wav', sep = "")), 16000)#get	
  

  # filter signal to get rid of low frequencies
  WAV@left = filtersig(WAV@left, 3, 50, samplingrate=WAV@samp.rate, ftype = "high", recursive = TRUE)

  cat('Number of entries in file: ', nrow(wordannot), '\n')
  
  wordannot$Content[grep('?',wordannot$Content, fixed = TRUE)] = '<UNVERSTÄNDLICH>'
  
   wordannot$Content[wordannot$Content == 'hä'] = '<HÄSITATION>'
  
 ########
 ## process
  #iword = 61
    a = foreach (iword = 1:nrow(wordannot),  .errorhandling='pass') %dopar%{   
      teil = wordannot[iword, ]
      
      cat('.')

      fn = paste0('tmp',FILENAME,'_', iword, '_', teil$Content)
      ## take word and preprocess
	  from = floor(teil$s.start*WAV@samp.rate)
	  to = ceiling(teil$s.end*WAV@samp.rate)
	  OUT = WAV[from:to] #cut vowel out
	  
	  window = hanning(200)
	  left = window[1:grep(max(window),window)[1]]
	  right = window[grep(max(window),window)[1]:length(window)]
	  
	  rest = length(OUT)-length(window)
	  fade = c(left, rep(1,rest-1), right) 
	  
	  #fade edges out
	  OUT@left = round(OUT@left*fade,0)
	  OUT = normalize(OUT, unit  = "16")
		  
		  
	Tmp.wav.name = paste(TMPFOLDER, fn, '.wav', sep = "")
	
	
	if(!teil$Content %in% IGNORE){
	tuneR::writeWave(OUT, Tmp.wav.name)
# 	writeWave(OUT, Tmp.wav.name, FALSE)
	 }
      
	######### align segments
	TAKE = !teil$Content %in% IGNORE & teil$durtake
	
	if (TAKE)  {  
			
	    fileConn<-file(paste0(TMPFOLDER,  fn, '.txt'), open = 'w',encoding = 
    'UTF-8'); 
	    writeLines(teil$Content, fileConn) 
	    close(fileConn) 
	
# 	      
	########### aligner ###################
		  
	    b = accessAligner(TMPFOLDER, ALIGNERHOME, fn)
	    
	    ROOTFOLDER = TMPFOLDER
	    ALIGNFOLDER = TMPFOLDER
	    GRIDFOLDER = TMPFOLDER
	    FILENAME = fn
	    change.AlignerToPraat(TMPFOLDER, TMPFOLDER, TMPFOLDER,fn,READWORDS=FALSE, READSEGMENTS=TRUE) 
      # 
	  } else {	

	  ################ find boundaries autocorrelation
	  if(!teil$Content %in% IGNORE){
	  try({
# 	  cat('findboundaries \n')
	  
	  fbd = findboundaries_distancebased(OUT, SineLength=200, WindowSize = 0.005, 
	  Shift = 0.001, Stepsize = 0.001, PLOT=FALSE, TMPFOLDER=TMPFOLDER, fn = fn)
	  
	  change.AlignerToPraat(TMPFOLDER, TMPFOLDER, TMPFOLDER,fn,READWORDS=FALSE, 
	  READSEGMENTS=TRUE) 
	  })
	  }
	  
	  }#end ifelse
	  
# 	    system( #get access to the system (LINUX)    
# 	    paste("cd ",TMPFOLDER, ";", #go to the directory with the downsampled files
# 	    "rm ", fn,".mfc;",#delete vector file
# 	    "rm ", fn,".phone*;",#delete vector file
# 	    "rm ", fn,".txt;",#delete vector file
# 	    sep = "") #paste
# 	    ,inter = TRUE
# 	    )#system

    }#for iword
    
###########################################
 ## delete unnecessary stuff

 ###################
 ## read textgrids
 
 SB.big = data.frame()#new dataframe for segments
 
  SB.big<-foreach (iword = 1:nrow(wordannot), .combine = rbind, .errorhandling='pass') %dopar%{ 

   
	teil = wordannot[iword, ]
	dur = teil$s.end - teil$s.start
	
	fn = paste0('tmp', FILENAME,'_', iword, '_', teil$Content, '.TextGrid')
	
	teil$durtake = NULL
    
      if (file.exists(paste0(TMPFOLDER, fn))){
	  SB <- read_praat_boundaries_postcorrection(TMPFOLDER,fn)
	  
	  SB$dur = dur 
	  SB$s.start2 = SB$s.start+teil$s.start
	  SB$s.end2 = SB$s.end+teil$s.start
	  SB$s.end2[nrow(SB)] = teil$s.end
	  
	  
	  SB$iword = iword
	  out = SB
	  
	  
	  
	} else {
	    teil$s.start2 = teil$s.start
	    teil$s.end2 = teil$s.end
	    
	    IS.IGNORE =  teil$Content %in% IGNORE
	    
	    if (!IS.IGNORE){#add _X only when teil$Content does not contain >
	      teil$Content = paste0(teil$Content, '_X')
	    }
	    
	    
	    teil$dur = dur
	    teil$iword = iword
	    out = teil	
	  }
  
#   	D = c(D, ncol(out))
out
  }#for

SB.big$s.start = SB.big$s.start2
SB.big$s.end = SB.big$s.end2

SB.big$s.end[nrow(SB.big)] = wordannot$s.end[nrow(wordannot)]

	  
wo = !wordannot$durtake
wordannot$Content[wo] = paste0('AFS_', wordannot$Content[wo])
  
  
TG = list(words = wordannot, segments = SB.big)#create a list with word/segment tier

#write new textgrid for entire signal including both, wordlevel and segmentlevel
# signalduration = length(WAV)/WAV$rate
signalduration = wordannot$s.end[nrow(wordannot)]
########## open new textgrid file 
#open file and delete everything in it
fileConn<-file(paste(GRIDFOLDER, FILENAME, '.whole.TextGrid', sep =""), open = 'w',encoding = 'UTF-8'); close(fileConn) 

# open file in append mode and leave it open
fileConn<-file(paste(GRIDFOLDER, FILENAME, '.whole.TextGrid', sep =""), open = 'a', encoding = 'UTF-8')

############ first lines
writeLines(c('File type = "ooTextFile"','Object class = "TextGrid"', ''), fileConn) 
writeLines(c('xmin = 0'), fileConn) 
writeLines(c(paste('xmax = ', signalduration, sep = "")), fileConn)
writeLines(c('tiers? <exists> '), fileConn)
writeLines(c(paste('size = ', length(TG), sep ="")), fileConn)

writeLines(c('item []:'), fileConn)


############  write tiers into the file         
for (iTier in 1:length(TG)){ #go through Tiers


  z = 0#counter for intervalls
  
  Intervalls = TG[[iTier]]  #pass all intervalls in one tier
  
  #### Insert tier information  
  writeLines(c(paste('    item [', iTier, ']:', sep = "")), fileConn)          
  writeLines(c('        class = "IntervalTier"'), fileConn)          
  writeLines(c(paste('        name = "', names(TG)[iTier] ,'"', sep ="")), fileConn)  
  writeLines(c('        xmin = 0'), fileConn) 
  writeLines(c(paste('        xmax = ', signalduration, sep = "")), fileConn)   
  writeLines(c(paste('        intervals: size = ', dim(Intervalls)[1], sep = "")), fileConn) 
  
  
  ############# insert boundaries found by the aligner     
  for (iIntervall in 1:dim(Intervalls)[1]){
    
    writeLines(c(paste('        intervals [', iIntervall, ']:', sep = "")), fileConn) 
    writeLines(c(paste('            xmin = ', Intervalls$s.start[iIntervall], sep = "")), fileConn) 
    writeLines(c(paste('            xmax = ', Intervalls$s.end[iIntervall], sep = "")), fileConn) 
    writeLines(c(paste('            text = "', Intervalls$Content[iIntervall], '"', sep = "")), fileConn) 
    
  } #iIntervalls
  
  
}#iTier

close(fileConn)

invisible(TG)

}#function









##################################################################################################
# aligner reader

segment_read_aligner_boundaries <- function (ALIGNFOLDER,FILENAME,EXCLUDEPAUSES=TRUE, READWORDS=TRUE, READSEGMENTS=TRUE){
 library(car)
#load Aligner file
read.text <- function(FileId, split = " ", encoding = "UTF-8"){id = file(FileId);  txt = strsplit(readLines(id, encoding = encoding), split = split);  close(id); return(txt)}
  
  if(READWORDS==TRUE){words.txt =  read.text(paste(ALIGNFOLDER, FILENAME, ".words",sep=""))} else {words.txt=NULL}
  if(READSEGMENTS==TRUE){phones.txt = read.text(paste(ALIGNFOLDER, FILENAME, ".phoneswithQ",sep=""))} else {phones.txt=NULL}

        
### Preferences
        zL = 0 #line counter
        WB = data.frame(s.start = rep(NaN, length(words.txt)+length(phones.txt) ) )#outputfile

## Read Words
 if(READWORDS==TRUE){ 
Start = 0 #set beginning of file
      for (iLine in 2:length(words.txt)){ #skip first line since = #
        zL = zL+1
       what = words.txt[[iLine]] 
       Ende = as.numeric(what[1])
    
        #Replace Umlauts 
        was = what[3]

        was = gsub('"a', 'ä', was)
        was = gsub('"u', 'ü', was)
        was = gsub('"o', 'ö', was)
        was = gsub('"s', 'ß', was)
        was = gsub('"A', 'Ä', was)
        was = gsub('"U', 'Ü', was)
        was = gsub('"O', 'Ö', was)
        
        WB$s.start[zL] = Start
        WB$s.end[zL] = Ende
        WB$Word[zL] = was #get the word
        WB$Tier[zL] = "Word"
        Start = Ende #replace Start for the next look
      }#for iLine

}#if READWORDS

      ## Read Segments 
  if(READSEGMENTS==TRUE){  
      Start = 0 #set beginning of file
      for (iLine in 2:length(phones.txt)){ #skip first line since = #
        zL = zL+1
        what = phones.txt[[iLine]] 
        Ende = as.numeric(what[1])
        
        WB$s.start[zL] = Start
        WB$s.end[zL] = Ende
        WB$Word[zL] = what[3] #get the segment
        WB$Tier[zL] = "Segment"
        Start = Ende #replace Start for the next look
      }#for iLine
  }  #if READSEGMENTS
  
  
## Delete pauses
 if (EXCLUDEPAUSES == TRUE){
WB = WB[!WB$Word == "_p:_",] #from segments
WB = WB[!WB$Word == "<P>",] #from words
} #if EXCLUDEPAUSES
  
  
# WB$Start = WB$s.start
# WB$Ende = WB$s.end

  
#exclude unused rows  
WB = WB[!is.na(WB$s.start),]
  
####### create new variables given what level has to be read
##########################################################################   
if (READSEGMENTS == TRUE & READWORDS == FALSE){
  segments = WB[WB$Tier == "Segment",]
  segments$Sgmt = segments$Word
  output = list(segments = segments)
  return(output)
} 
  
##########################################################################  
if (READSEGMENTS == FALSE & READWORDS == TRUE){
  slowa = WB[WB$Tier == "Word",]
  slowa$Sgmt = slowa$Word
  output = list(words = slowa)
  return(output)
}  

########################################################################## 
if (READSEGMENTS == TRUE & READWORDS == TRUE){
  segments = WB[WB$Tier == "Segment",]
  segments$Sgmt = segments$Word
  slowa = WB[WB$Tier == "Word",]
  slowa$Sgmt = slowa$Word
  
 ## add word-names to segments using the time interval
    nWords = length(slowa$Start)
      for (iWord in 1:(nWords) ) {  
          a1 = slowa$s.start[iWord] #beginning of the interval
          b1 = slowa$s.end[iWord] #end of the interval
          name = slowa$Word[iWord] #which word
          strg = paste(a1,":",b1,"=\"",name, "\"", sep ="")
          #Recode the vector with the given name in the given interval and check which  slots were replaced
         replaced =  which(( Recode(segments$s.start, strg ) == name) == TRUE)
         segments$Word[replaced] = name #replace the name in the given slots
      }#for 
  
output = list(segments = segments, words = slowa)
return(output)  
}  
  

  
}

#######################################################################################################
# Created by Tomaschek, Fabian 01.11.2012


change.AlignerToPraat <- function (ROOTFOLDER, ALIGNFOLDER,GRIDFOLDER,FILENAME,READWORDS=TRUE, READSEGMENTS=TRUE){
library(tuneR)
# print(1)

WB = segment_read_aligner_boundaries(ALIGNFOLDER,FILENAME,EXCLUDEPAUSES=FALSE, READWORDS=READWORDS, READSEGMENTS=READSEGMENTS)#use function to get segmentation from aligner
W = readWave(paste(ROOTFOLDER,  FILENAME, '.wav', sep =""))
signalduration = length(W@left)/W@samp.rate

if(sum(names(WB) %in% 'segments')>0) {WB$segments$s.end[nrow(WB$segments)] = signalduration}
if(sum(names(WB) %in% 'words')>0) {WB$words$s.end[nrow(WB$words)] = signalduration}

########## open new textgrid file 
#open file and delete everything in it
fileConn<-file(paste(GRIDFOLDER, FILENAME, '.TextGrid', sep =""), open = 'w',encoding = 'UTF-8'); close(fileConn) 

# open file in append mode and leave it open
fileConn<-file(paste(GRIDFOLDER, FILENAME, '.TextGrid', sep =""), open = 'a', encoding = 'UTF-8')

# print(2)

############ first lines
writeLines(c('File type = "ooTextFile"','Object class = "TextGrid"', ''), fileConn) 
writeLines(c('xmin = 0'), fileConn) 
writeLines(c(paste('xmax = ', signalduration, sep = "")), fileConn)
writeLines(c('tiers? <exists> '), fileConn)
writeLines(c(paste('size = ', length(WB), sep ="")), fileConn)
writeLines(c('item []:'), fileConn)


Zeugs = data.frame(matrix(c(
'^haha$', '<LACHEN>',
'^hehe$', '<LACHEN>',
'^ähm$', '<HÄSITATION>',
'^hmm$', '<HÄSITATION>',
'^hm$', '<HÄSITATION>',
'^mhm$', '<HÄSITATION>',
'^äh$', '<HÄSITATION>',
'^oke$', 'OK'
), byrow =TRUE, ncol = 2), stringsAsFactors=FALSE)




# print(3)
if('words' %in% names(WB)){
for (ichange in 1:nrow(Zeugs)){
  WB$words$Word = gsub(Zeugs[ichange,1], Zeugs[ichange,2], WB$words$Word)  
}
}



# print(4)



############  write tiers into the file         
for (iTier in 1:length(WB)){ #go through Tiers
 z = 0#counter for intervalls
  
Intervalls = WB[[iTier]]  #pass all intervalls in one tier
    
#### Insert tier information  
writeLines(c(paste('    item [', iTier, ']:', sep = "")), fileConn)          
writeLines(c('        class = "IntervalTier"'), fileConn)          
writeLines(c(paste('        name = "', names(WB)[iTier] ,'"', sep ="")), fileConn)  
writeLines(c('        xmin = 0'), fileConn) 
writeLines(c(paste('        xmax = ', signalduration, sep = "")), fileConn)   
writeLines(c(paste('        intervals: size = ', dim(Intervalls)[1], sep = "")), fileConn) 

  
############# insert boundaries found by the aligner     
# print(5)

 for (iIntervall in 1:nrow(Intervalls)){

    writeLines(c(paste('        intervals [', iIntervall, ']:', sep = "")), fileConn) 
  writeLines(c(paste('            xmin = ', Intervalls$s.start[iIntervall], sep = "")), fileConn) 
  writeLines(c(paste('            xmax = ', Intervalls$s.end[iIntervall], sep = "")), fileConn) 
  writeLines(c(paste('            text = "', Intervalls$Sgmt[iIntervall], '"', sep = "")), fileConn) 
 
 } #iIntervalls
  

}#iTier
  
close(fileConn)
}

########################################################################################################
#written by Fabian Tomaschek, edited by Lea Hofmaier 

read_praat_boundaries_postcorrection <- function (CORRGRIDFOLDER,GRIDFILE)  {
  
  filename = paste(CORRGRIDFOLDER, GRIDFILE,sep ="")
  file.id = file(filename)
  txt = readLines(file.id)
  close(file.id)
  cat("\n  Reading annotation\n")
  
  ###################################################################################################################################
  ### Check where the tiers are located and get their names ############
  Tiers= which(grepl("name", txt) == TRUE) #Find the lines of the tiers
  Tierstr = strsplit(txt[Tiers], " ") #find the names of the tiers
  Tiernames =as.character(c())
  Tierloc = c()
  for (iTiers in 1:length(Tiers)){
    a = which(grepl("=", Tierstr[[iTiers]]) == TRUE) #where is '=' located, 1 before the Tier name
    b = Tierstr[[iTiers]][a+1] #get the Tier name
    cc = strsplit(b, "")[[1]]#it is a structure, which is why [[1]]
    d = cc[!grepl( "\"", cc)] # get rid of the \" 
    Tiernames[iTiers] = paste(d[1:length(d)], collapse = "") #concatenate the word again
  }
  
  
  ###############################################
  ## Get the lines for all annotations
  wo.int = which(grepl("intervals ", txt) == TRUE)#all intervals
  
  #extract name and time
  anf= txt[wo.int+1]
  ende = txt[wo.int+2]
  was = txt[wo.int+3]
  
  #split long character strings
  anf = strsplit(anf, " ")
  ende = strsplit(ende, " ")
  was = strsplit(was, " ")
  
  
  #create list
  WB = data.frame(s.start = rep(NaN, length(anf)) )
  
  #go from line to line and take.......
  for (iline in 1:length(anf)){
    WB$s.start[iline] = as.numeric( anf[[iline]] [ length(anf[[iline]]) ] )#..... starting point
    WB$s.end[iline] =   as.numeric( ende[[iline]] [ length(ende[[iline]]) ] ) ####... ending point
    
    wort = strsplit(was[[iline]] [ length(was[[iline]]) ] , "")[[1]] #...... the word
    wort = wort[!grepl( "\"", wort)]#Take word withouth \" string
    
    WB$Content[iline] = paste(wort[1:length(wort)], collapse="") #concatenate lots of string fragments and store
  }
  
  
  
  ##Insert the Tiernames as factors
  nTier = length(Tiers)
  WB$Tier = WB$Content
  
  if(nTier == 1) {WB$Tier = Tiernames}
  if(nTier > 1){
    for (iTier in 1:(nTier-1) ) { 
      #for all but the last tier 
      a1 = Tiers[iTier]
      b1 = Tiers[iTier+1]
      name = Tiernames[iTier]
      
      replaced =  which(( Recode(wo.int, paste(a1,":",b1,"=\"",name, "\"", sep ="")) == name) == TRUE)#check which  slots were replaced
      
      WB$Tier[replaced] = name
    }#for
    
    
    #the last tier 
    a2 = Tiers[nTier] 
    b2 = max(wo.int)
    name = Tiernames[nTier]
    replaced =  which(( Recode(wo.int, paste(a2,":",b2,"=\"",name, "\"", sep ="")) == name) == TRUE)#check which  slots were replaced
    WB$Tier[replaced] = name
  }#if
  return(WB)
}




##########################################################################
## normieren
##########################################################################

normieren <- function (x,norm_min, norm_max){
y <-( (x - min(x, na.rm=TRUE)) * ((norm_max-norm_min)/(max(x, na.rm=TRUE)-min(x, 
na.rm=TRUE))) ) + norm_min
return (y)}




##########################################################################
## filtersig
##########################################################################
# filtersig: non-regressive filter for single vector
# Sub-routine created by Tomaschek Fabian, 2012-10-24

filtersig <- function (signal, poles, freqint, samplingrate, ftype, algo = 'butter', 
recursive = FALSE){
library(signal)

	#check whether the signal package is installed, needed for filtering
	is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
	if (!is.installed('signal')){install.packages('signal')}
	
	#Choose the algorythm to calculate filter coefficients
	if (algo == 'butter'){fc = butter(poles,freqint/(samplingrate/2),type=ftype)}
	if (algo == 'FIR'){fc = fir1(poles,freqint/(samplingrate/2),type=ftype)}

#choose whether the recursive or non-recursive filter has to be taken. 
#recursive is possible only with butter!
if (recursive == FALSE){
	correction = ceiling(poles/2) #calculate correction
	newsignal1 = filter(fc, signal) #filter (package signal)
	newsignal = 
c(newsignal1[c((1+correction):length(newsignal1))],rep(NaN,correction)) #adjust correction
	return(newsignal)#return
	}

if (recursive == TRUE){
	if(algo != 'butter') {fc = butter(poles,freqint/(samplingrate/2),type=ftype)} 
#recalculate fc, in case algo == FIR. 
	return(filtfilt(fc, signal))#return
	}
	
}

##########################################################################
## velocity
##########################################################################

velocity <-function(timeaxis,signal, steps){
  cell.diff <- function(subtrahend, steps){
    minuend = subtrahend[-c(1:steps)]
    subtrahend2 = subtrahend[-(c(length(subtrahend)-steps+1:length(subtrahend)))]
    return(minuend-subtrahend2)
  } 
  
  neusig = rep(NaN, length(signal))
  neusig[2:(length(signal)-1)] =  cell.diff(signal, steps) / cell.diff(timeaxis,steps)
  return(neusig) #return
}


##########################################################################
## findboundaries_distancebased
##########################################################################
# load(list.files(pattern='rda'))
# save(list=ls(), file = '~/FILTER.rda')

findboundaries_distancebased = function(WAVIN, 
SineLength=200,SineFreq=400,fadeLength=0.01, WindowSize = 0.005, Shift = 0.001, Stepsize = 
0.001, filterPoles=5, filterFrequency = 250, Threshold=65, PLOT=FALSE, LimitXaxis= TRUE, 
TMPFOLDER='', fn = ''){
# Written by Fabian Tomaschek, XI.15.

# WAVIN: tuneR wav object
# SineLength: Duration of sine wave in ms added left/right to the signal
# WindowSize: Duration of the two windows in which the spektrum is analyzed
# Shift: Shift between first and second window in seconds
# Stepsize: Steps along the signal in seconds
# PLOT: Plot the result, TRUE/FALSE
# filterFrequency
# Threshold: in percent of maximal velocity change
# TMPFOLDER: If provided, a txt file is stored with the found boundaries
# fn: file name for the textfile. only necessary if TMPFOLDER provided

library(tuneR)
library(signal)

##############################################################
##   PROCESS

SR = WAVIN@samp.rate
Sigdur = length(WAVIN@left)/SR

## add sine 
if(SineLength!=0){
Sine = syntsine(SineFreq, duration = SineLength, samplerate = SR)

    if(SineFreq == 1){
    Sine = rep(0, (SineLength/1000)*SR)
    }

} else 
{Sine = c()}

Sigin = normieren(WAVIN@left, -1,1)



#### fade signal
fadeout = seq(1,0, length = fadeLength*SR)
fadein = seq(0,1, length = fadeLength*SR)

if(Sigin[1] != 0){
Sigin = Sigin*c(fadein, rep(1,length(Sigin)-fadeLength*2*SR), fadeout)
}

Sine = Sine*c(fadein, rep(1,length(Sine)-fadeLength*2*SR), fadeout)

## create signal
Sig <- c(Sine, Sigin, Sine)


## investigate least squares
Time =   seq(from = 0, to = (length(Sig)-1)/SR, length = length(Sig))
Steps = seq(from=min(Time)+WindowSize, to=max(Time)-WindowSize, by = Stepsize);


SummedDistance <-rep(NA, length(Steps))


for (istep in 1:length(Steps)){
  boundary = Steps[istep]
  
  Pow = JuxtaSpectrWindow(Sig, boundary, SR, WindowSize, Shift, Freq=TRUE)
  
  V = sum(abs(Pow$PowerL-Pow$PowerR), na.rm=TRUE)
  SummedDistance[istep] = V;
}


## repair inf/nan
SummedDistance[!is.finite(SummedDistance)] = NaN
wonan = which(is.na(SummedDistance), TRUE)
if(length(wonan)>0){
  for (inan in 1:length(wonan)){
  SummedDistance[wonan[inan]] =  mean(c(SummedDistance[wonan[inan]-1], 
SummedDistance[wonan[inan]+1]), na.rm=TRUE)
  }
}


### speech envelope
# Sp.envel = normieren(filtersig(abs(WAVIN@left),5, 10, SR, 'low', recursive=TRUE), 0, 1)

## exclude Sine time points for plotting envelope
xlim=c(SineLength/1000, Sigdur+SineLength/1000 )
Signaltime = Time[Time>=min(xlim) & Time < max(xlim)]

if(!LimitXaxis){xlim = range(Time)}

### filter summed distance
# Cr.filt = filtersig(Corr, P, F, SR, 'low', recursive=TRUE)
SD.filt = filtersig(SummedDistance, filterPoles, filterFrequency, SR, 'low', 
recursive=TRUE)



## velocity of change
V.sd = velocity(Steps, SD.filt, 2)


## acceleration of change & find boundaries
A.sd2 = velocity(Steps, V.sd, 2)
A.sd1 = abs(A.sd2)

what2Max = max(A.sd1[Steps>min(Signaltime, na.rm=TRUE) & Steps<max(Signaltime, 
na.rm=TRUE)], na.rm=TRUE)

A.sd = as.numeric(scale(A.sd1, center = FALSE, scale = what2Max/100))

A.sd.t = A.sd2>1 #absolute maxima of change
wo = c(abs(A.sd.t[1:(length(A.sd.t)-1)] - A.sd.t[2:length(A.sd.t)]), 0)

wo[is.na(wo)] = 0
woboundary.area = Steps[wo==1]
#exclude boundaries beyond signal
woboundary.area = woboundary.area[woboundary.area>SineLength/1000 & woboundary.area < 
Sigdur+SineLength/1000 ]
#add word boundaries

woboundary.area = c(SineLength/1000, woboundary.area, Sigdur+SineLength/1000)

BoundaryArea.Time = c()
BoundaryArea = c()

for (ibound in 1:(length(woboundary.area)-1)){
  from = woboundary.area[ibound]
  to = woboundary.area[ibound+1]
  
  T= seq(from, to,  by=0.001)
  BoundaryArea.Time = c(BoundaryArea.Time,T)

  wo = Steps>from & Steps<to
  
  if(sum(wo)>0){
  whatMax = max(A.sd[wo], na.rm = TRUE) 
  BoundaryArea = c(BoundaryArea, rep(whatMax , length(T)))
  } else {
  
    BoundaryArea = c(BoundaryArea, rep(BoundaryArea[length(BoundaryArea)] , length(T)))
  }

}


## find boundaries
# V.sd.t = V.sd>0 #absolute maxima of change
# wo = c(abs(V.sd.t[1:(length(V.sd)-1)] - V.sd.t[2:length(V.sd)]), 0)
# wo[is.na(wo)] = 0
# woboundary = Steps[wo==1]

woboundary = woboundary.area #boundaries on the basis of acceleration minima (i.e. speed maximum

#exclude boundaries beyond signal
woboundary = woboundary[woboundary>SineLength/1000 & woboundary < Sigdur+SineLength/1000 ]
  
# exclude wbs when the change does not surpass acceleration threshold
# woboundary = woboundary[woboundary %in% BoundaryArea.Time[BoundaryArea>Threshold] ]

NwithinWordBoundaries = length(woboundary)
woboundary = c(SineLength/1000, woboundary, Sigdur+SineLength/1000)





## correlations between windows
D <- c()
if (NwithinWordBoundaries > 0){
for (ibound in 1:(length(woboundary)-2)){
  fromto1 = woboundary[c(ibound, ibound+1)]*SR
  fromto2 = woboundary[c(ibound+1, ibound+2)]*SR

  first = spectralanalysis(Sig[min(fromto1):max(fromto1)], SR)
  second = spectralanalysis(Sig[min(fromto2):max(fromto2)], SR)
  
  L.F = log(first$power)
  L.S = log(second$power)
  
  ## analyse spektra
  X = first$frequency
  mod.LF = lm(L.F~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8))
    
  X = second$frequency
  mod.LS = lm(L.S~X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8))
  
  newdat = data.frame(X = (1:1000)*((SR/2)/1000))
  
  newdat$L.F = predict(mod.LF, newdat)
  newdat$L.S = predict(mod.LS, newdat)
  
  newdat$diff = abs(newdat$L.F - newdat$L.S)
   
#   plot(first$frequency,L.F, type = 'l', ylim = range(c(L.F, L.S)))
#   lines(second$frequency, L.S, col = 'red')
#   lines(newdat$X, newdat$L.F, col = 'blue', lty=2)
#   lines(newdat$X, newdat$L.S, col = 'magenta', lty=2)

D = c(D, sum(newdat$diff ))
}

D = ceiling(D)
D = c(D, NA)

} else { D = NA}



####################
## write phoneswithQ
boundaries = woboundary[-1]- SineLength/1000


if (nchar(TMPFOLDER) >0){
fileConn<-file(paste0(TMPFOLDER,  fn, '.phoneswithQ'), open = 'w',encoding = 
'UTF-8'); 
writeLines('#', fileConn) 


if (length(boundaries) == 1){
    writeLines(paste0(boundaries, ' 121 ASF_NBF'), fileConn)


  } else {
    for (ibound in 1:length(boundaries)){
    writeLines(paste0(boundaries[ibound], ' 121 ASF_', ibound, '_', D[ibound]), fileConn)
    }
  }

close(fileConn) 
}




######## 
## plot 
if (PLOT ){
      par(mfrow=c(1,1))
      plot(Time, Sig, type = 'l', ylim = c(-1,1),xlim=xlim, lwd = 0.5, col = 'grey')

	LG<-c('Sequential correlation', 'Velocity of change', 'Acceleration of change')
	COL.samp = rainbow(length(LG))
	N = 1
	## sboundaries
	abline(v = c(SineLength/1000, woboundary[length(woboundary)]), col = 'red', lwd=2)
	abline(v= woboundary, lty=2, col = 'green', lwd=2)

	## least squares
	lines(Steps, normieren(SD.filt,0,1), col = COL.samp[N], lty=1, lwd = 2)
	N = N+1

      ## velocity
      par(new=TRUE)
      plot(Steps, Y<-V.sd, type ='l', col = COL.samp[N], ylim = c(max(Y, na.rm=TRUE)*-1, 
      max(V.sd, 
      na.rm=TRUE)), lwd = 2, lty = 2, xlim = xlim, xlab = '', ylab = '', 
 xaxt='n', yaxt='n')
      abline(h=0, col=COL.samp[N], lwd = 2)
      N = N+1

      ## acceleration
      par(new=TRUE)
      plot(Steps, Y<-A.sd2, type ='l', col = COL.samp[N], ylim = c(max(Y, na.rm=TRUE)*-1, 
      max(Y, na.rm=TRUE)), lwd = 2, xlim = xlim, xlab = '', ylab = '', 
 xaxt='n', yaxt='n')
      abline(h=c( 100, Threshold), col = COL.samp[N])
      N = N+1

      ## area
      lines(BoundaryArea.Time, BoundaryArea, col ='black')
     
      legend('bottomright', legend = LG, 
      col =rainbow(length(LG)), lty=rep(1,length(LG)), lwd = rep(2,length(LG)))
#       text(woboundary[2:(length(D)+1)], y = -10, labels = round(D,2), cex = 1.5)
}  



##############################################
## create dataframe with boundaries and return
SB = data.frame (s.start  = c(0, boundaries[-length(boundaries)]), s.end = boundaries)
SB$Segment = 1:nrow(SB)
SB$Difference2next = D



invisible(SB)
}

###########################################
## JuxtaSpectrWindow
###########################################

JuxtaSpectrWindow = function(Sig, boundary, SR, WindowSize,Shift,Freq=TRUE)
{

WS.samp = floor(SR*WindowSize)
OL.samp = floor(SR*Shift)

  sampLocboundary = round(boundary*SR,0)
  winLeft = c(sampLocboundary-WS.samp, sampLocboundary)
  winRight = winLeft+OL.samp

  sigLeft = Sig[winLeft[1]:winLeft[2]]
  
  if(min(winLeft[1]:winLeft[2]) == 0){
    sigLeft = Sig[winLeft[1]:winLeft[2]+1]
  }
   
  sigRight = Sig[winRight[1]:winRight[2]]

  #sprektrale analyse
  PowerL = spectralanalysis(sigLeft, SR)
  PowerR = spectralanalysis(sigRight, SR)
  out = data.frame(PowerL = log(PowerL$power), PowerR = log(PowerR$power))

  if(Freq){
   library(seewave) 
  out = data.frame(
  PowerL = log(PowerL$power), 
  PowerR = log(PowerR$power), 
  Freq.Hertz =PowerR$frequency, 
  Freq.Mel = mel(PowerR$frequency) )
  }
  
  return(out)
}

# load(list.files(pattern='rda'))
# save(list=ls(), file = '~/FILTER.rda')


###############################################
## access Aligner
##############################################

accessAligner <- function(TMPFOLDER,ALIGNERHOME, fn ){
b<-list(
	      try({
	      system( #get access to the system (LINUX)    
	      paste("export 
	      PATH=\"", ALIGNERHOME, ":$PATH\";",
	      " export ALIGNERHOME=\"~/Proggys/Aligner\";", 
	      #create the environment for the aligner 
	      " export ALANG=\"deu\";",
	      " export LANG=C;",
	      "cd ",  TMPFOLDER, ";", #go to the directory with the downsampled files
	      " ", ALIGNERHOME, "Alignphones ", fn,".wav;", 
	      sep = "") #paste
	      ,inter = TRUE)#system
	      }, silent = TRUE)#try
	      )#list
	      
	      "~/Proggys/Aligner/bin/deu/"
	      
invisible(b)
	      
}




##############################################################
##  syntsine
##############################################################
syntsine=function(frequency,intensity=rep(1,length(frequency)), phase 
=rep(1,length(frequency)), duration=200, samplerate=16000)
{

# %% produce signal
    dur = duration/1000;
    timeaxis =   seq(from = 0, to = dur, by = 1/(samplerate-1))

    
    sfreq = length(frequency);
    intensity = intensity/sfreq;
    signalharmonics = matrix(0, ncol = sfreq, nrow = length(timeaxis))
    

#     %produce harmonics
    for (iharm in 1:sfreq){
        omega = 2 * pi * timeaxis * frequency[iharm];
        signalharmonics[,iharm] = cos(omega + phase[iharm] ) * intensity[iharm]; 
    } 
    
    signal = rowSums(signalharmonics); 
    signal =  ( (signal - min(signal)) * (2/(max(signal)-min(signal))) ) -1;
return(signal)    
}

##############################################################
##   Spectral Analysis
##############################################################

spectralanalysis = function(Signal,SR,  Phase = FALSE){
    library(signal)
    angle <- function (h){
    return( atan2(Im(h), Re(h)))
    }

#     Signal = Signal * hanning(length(Signal))
    spk1 = fft(Signal) 

    # poweranalysis = (abs(spk1)/length(Sig))^2
    realpart = 1:round((length(Signal)/2),0)
#     poweranalysis = (abs(spk1)/length(signal))^2
    poweranalysis = abs(spk1)
    
    poweranalysis=poweranalysis[realpart]
    
    frequency = realpart*(SR/length(Signal))

    out = list(power = poweranalysis, frequency = frequency)

        
    if (Phase){
    ang1 = angle(spk1)[realpart] #Phasenwinkel, i.e. frequenz stärke
    out = list(power = poweranalysis, angle = ang1, frequency = frequency)
    }

    return(out)

}#function






