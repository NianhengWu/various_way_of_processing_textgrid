
# source('/home/fabian/Working_Data/SWG/SWG_Processing_Scripts_FT/functions.R')


library(zoo)
library(stringr)
library(tuneR)
library(foreach)
#library(doMC)
#registerDoMC(detectCores())
#registerDoMC(1)

################################################################
## manual preprocessing of TextGrid files

# changeTextgrids TEST.txt 


###########



setwd('/home/nianheng/Documents/hiwi/01januar/karen/done_Rawdata/')
DIR = getwd()


	source('/home/nianheng/Documents/hiwi/01januar/karen/code/SWG_functions.R')




#####################################################################################
## STEP 0 to STEP 1
## Cut out phrases for Variables of interest
#####################################################################################
# CopyTokenFiles(DIR)
#copying is performed in "SWG_create_folders_copy_paste_files.R"

 DIR.out = '/home/nianheng/Documents/hiwi/01januar/karen/'
######nianheng change#########
setwd(DIR)
FNS= list.files(pattern = 'wav')
for (ifn in 1:length(FNS)){
wav.fn = FNS[ifn]
CreatePhrasesForAlignment(DIR, DIR.out, wav.fn)

}
###
setwd(DIR)
FNS= list.files(pattern = 'wav')
for (file in FNS){
wav.fn = file
file
CreatePhrasesForAlignment(DIR, DIR.out, wav.fn)
file
}



#####################################################################################
## STEP 1 to STEP 2 
## Run Aligner 
#####################################################################################
source('/home/fabian/Dropbox/Shared_Directories/KEC_DROPBOX/KEC/Aligner_DONOTCHANGE/aligner_functions_in_V2017_09_28.R')




################# PREFERENCES #############################################################

DIR_SWGDATA = '/media/fabian/57372273-d38d-4512-8472-8542ad21de89/SWG/SWG_Data/'

## Aligner first preferences
ALIGNLANG = 'deu'
#where the aligner files are located on your system
ALIGNERHOME = "/home/fabian/Proggys/Aligner/bin/deu/" 

# folder of the experiment files, last / needed!
ALIGNFOLDER = paste0(DIR_SWGDATA, "TMP/")
    mymkdir(ALIGNFOLDER)
    encoding = "UTF-8"

### File set up 
setwd(paste0(DIR, '/SWG_Data/'))
SPEAKER.IN = list.files(pattern = 'S0_S1', recursive = TRUE)
# Names = c( 'Angela', 'Herbert', 'Elke','Louise', 'Markus', 'Ricarda') 
Names = unique(gsub('S0|[0-9]|-| |I', '', list.files (path = 'SWG_Data/', pattern = 'S0')))

SPEAKER = grep(paste(Names, collapse = '|'), SPEAKER.IN, value = TRUE)

ispeaker = 1
# ispeaker = grep('S008-17-I-1-Rupert', SPEAKER)


for (ispeaker in 1:length(SPEAKER)){ #
    print(SPEAKER[ispeaker])
    load(SPEAKER[ispeaker], verbose = TRUE)
  dim(PROTOCOL_S0_S1)
  
#     length(PROTOCOL_S0_S1$FN.out)
#     length(unique(PROTOCOL_S0_S1$FN.out))
    what.speaker = PROTOCOL_S0_S1$FN[1]


    ROOTFOLDER = paste0(DIR_SWGDATA,what.speaker, '/Step1_Phrasen/') 
    TRANSFOLDER =  paste0(DIR_SWGDATA,what.speaker, '/Step1_Phrasen/') 
    GRIDFOLDER =  paste0(DIR_SWGDATA,what.speaker, '/Step2_allinierte_Phrasen/') 





files = sort(PROTOCOL_S0_S1$FN.out)
files = files[!files == "NA"]

    # ifile = 1

    for (ifile in 1:length(files)){

    FILENAME = files[ifile]
    
        sig = downsample.forAligner(ROOTFOLDER,ALIGNFOLDER,FILENAME )
        
        run.aligner(ALIGNERHOME,TRANSFOLDER,ALIGNFOLDER,FILENAME,READWORDS=TRUE, READSEGMENTS=TRUE, encoding = 'UTF-8' ) 
        
        change.AlignerToPraat(ROOTFOLDER, ALIGNFOLDER, GRIDFOLDERlength(files),FILENAME,READWORDS=TRUE, READSEGMENTS=TRUE) 
        
     
    } #ifile

   X = list.files(path = 'TMP')
        file.remove(paste0('TMP/', X))
} #ispeaker


#####################################################################################
## STEP 2 to STEP 3
## Create trigrams
#####################################################################################
DIR = '/media/fabian/57372273-d38d-4512-8472-8542ad21de89/SWG/'


source(paste0(DIR, 'SWG_Processing_Scripts_FT/SWG_functions.R'))

library(zoo)
library(stringr)
library(tuneR)
library(foreach)
library(doMC)
registerDoMC(detectCores())


### File set up 
setwd(paste0(DIR, '/SWG_Data/'))
SPEAKER.IN = list.files(pattern = 'S0_S1', recursive = TRUE)
Names = unique(gsub('S0|[0-9]|-| |I', '', list.files (path = 'SWG_Data/', pattern = 'S0')))
SPEAKERS = grep(paste(Names, collapse = '|'), SPEAKER.IN, value = TRUE)

CreateTrigrams(DIR, SPEAKERS)



################
## copy to bins of ten

CopyToBins(DIR, Names)
