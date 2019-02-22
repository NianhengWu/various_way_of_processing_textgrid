#Script to get the Alignment
#Make sure that the in the directory in question you have a text file with the words in question which should be aligned. IMPORTANT: The file has to have the EXACT name as the Wav file.
################# FUNCTION NEEDED #############################################################

setwd('/home/nianheng/Documents/hiwi/02februar/TEST/')
source('/home/nianheng/Documents/hiwi/01januar/karen/code/SWG_functions.R')


#source('/home/nianheng/Documents/hiwi/10october/SWG/SWG_Processing_Scripts/SWG_functions.R')

################# PREFERENCES #############################################################

#setwd('/home/nianheng/Documents/hiwi/10october/SWG/SWG_Data/01_phrasen/')
SPEAKER = list.files()

ispeaker = grep('S007-82-I-2-Egbert', SPEAKER)

NEWFOLDER = "/home/nianheng/Documents/hiwi/02februar/done/"
for (ispeaker in 1:length(SPEAKER)){

ROOTFOLDER = paste0('/home/nianheng/Documents/hiwi/02februar/TEST/') 

# folder of the experiment files, last / needed!
#ALIGNFOLDER = "/home/nianheng/Documents/hiwi/10october/SWG/SWG_Data/ALIGNFOLDER/"
ALIGNFOLDER = "/home/nianheng/Documents/hiwi/02februar/trash/"

TRANSFOLDER =  paste0('/home/nianheng/Documents/hiwi/02februar/TEST/') 

GRIDFOLDER =  paste0("/home/nianheng/Documents/hiwi/02februar/done/") 

ALIGNERHOME = "/home/nianheng/Documents/hiwi/aligner_htk/Aligner-2-4beta/Aligner/bin/deu/" #where the aligner files are located on your system
ALIGNLANG = 'deu'


files = list.files(path = ROOTFOLDER, pattern='wav')
ifile = grep('S007-82-I-2-Egbert_[0-9]*', files)
#outputFile = file("/home/nianheng/Documents/hiwi/01januar/karen/error.txt", open = "a")

for (ifile in 1:length(files)){
# Take the original files (from /rawdata) and resample them to 16kHz mono files. 
# The files are stored in the new directory /ALIGN
#word level annotation
    
    encoding = 'UTF8'
	
	tryCatch(
		{
			FILENAME = gsub('.wav', '', files[ifile], fixed = TRUE)
			sig = downsample.forAligner(ROOTFOLDER,ALIGNFOLDER,FILENAME )
			run.aligner(ALIGNERHOME,TRANSFOLDER,ALIGNFOLDER,FILENAME,READWORDS=TRUE, READSEGMENTS=TRUE, encoding = "UTF-8" ) 
			change.AlignerToPraat(ROOTFOLDER, ALIGNFOLDER, GRIDFOLDER,FILENAME,READWORDS=TRUE, READSEGMENTS=TRUE)
			oldTxt = paste0(TRANSFOLDER, FILENAME, ".txt")
			oldWav = paste0(TRANSFOLDER, FILENAME, ".wav")
			newTxt = paste0(NEWFOLDER, FILENAME, ".txt")
			newWav = paste0(NEWFOLDER, FILENAME, ".wav")
			file.rename(oldTxt, newTxt)
			file.rename(oldWav, newWav)
		}, error = function (e)
		{
			print(e)
			#cat(as.character(FILENAME),file = outputFile, append = TRUE, sep = "\n")
		}
	)
}
close(outputFile)

}





