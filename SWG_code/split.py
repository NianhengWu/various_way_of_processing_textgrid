"""
Author: Nianheng Wu
xjtuwunianheng@gmail.com, Eberhard Karls Universität Tübingen
"""
import os
import textgrid
from pydub import AudioSegment
import shutil
import traceback

#annotation = '<[A-ZÄÖÜß]*>|^$'

def read_file_names(path):
    file_list = filter(lambda x: x.endswith('.TextGrid'), os.listdir(path))
    return file_list


def read_text_grid(name):
    tg=textgrid.TextGrid()
    tg.read(name)

    for each_tier in tg.tiers:
        if each_tier.name.find('SWG') != -1:
            phrase_list = get_text(each_tier)
            return phrase_list

def get_text(tier):

    phrase_list =[]
    intervals = tier.intervals

    for each_sentence in intervals:

        if each_sentence.mark != '':
            new_phrase = phrase(each_sentence.minTime, each_sentence.maxTime, each_sentence.mark)
            phrase_list.append(new_phrase)

        elif each_sentence.mark == '':
            new_phrase = phrase(each_sentence.minTime, each_sentence.maxTime, '<P>')
            phrase_list.append(new_phrase)

    phrase_list = reorganize_phrase_list(phrase_list)

    return phrase_list


def reorganize_phrase_list(phrase_list):
    organized_phrase_list = []
    count = 1
    organized_phrase_list.append(phrase_list[0])

    for each_phrase in phrase_list:

        if each_phrase.phrase == '<P>':
            if organized_phrase_list[count-1].phrase == '<P>':
                last_empty_phrase = organized_phrase_list[count-1]
                last_empty_phrase.update_end_time(each_phrase.end_time)
                organized_phrase_list[count-1] = last_empty_phrase
            else:
                organized_phrase_list.append(each_phrase)
                count += 1
        else:
            organized_phrase_list.append(each_phrase)
            count += 1


    return organized_phrase_list



class phrase:
    def __init__(self, start_time, end_time, phrase):
        self.start_time = start_time
        self.end_time = end_time
        self.phrase = phrase

    def update_start_time(self, new_start_time):
        self.start_time = new_start_time

    def update_end_time(self, new_end_time):
        self.end_time = new_end_time

    def update_phrase(self, new_phrase):
        self.phrase = new_phrase



def strip_txt_and_wav (phrase_list, wav_path, empty_path, none_empty_path, filename):
    filename = filename[:-9]
    suffix = 0
    this_whole_wav = AudioSegment.from_wav(wav_path + filename + ".wav")

    for each_phrase in phrase_list:
        phrase_string = each_phrase.phrase

        if phrase_string != '<P>':
            phrase_string = clean_word(phrase_string)
            with open(none_empty_path+filename+"_"+str(suffix)+".txt", 'w', encoding= 'utf8') as file:
                file.write(phrase_string)
            this_wav = this_whole_wav[int(each_phrase.start_time* 1000) : int(each_phrase.end_time *1000)]
            this_wav.export(none_empty_path+filename+"_"+str(suffix)+".wav", format="wav")
            suffix += 1

        else:
            with open(empty_path+filename+"_"+str(suffix)+".txt", 'w', encoding= 'utf8') as file:
                file.write(phrase_string)
            this_wav = this_whole_wav[int(each_phrase.start_time * 1000) : int(each_phrase.end_time * 1000)]
            this_wav.export(empty_path + filename + "_" + str(suffix) + ".wav", format="wav")
            suffix += 1



def clean_word(word_string):
    word_string = word_string.replace("ä", "aE")
    word_string = word_string.replace("ö", "oE")
    word_string = word_string.replace("ü", "uE")
    word_string = word_string.replace("ß", "sS")
    word_string = word_string.replace("Ä", "AE")
    word_string = word_string.replace("Ö", "OE")
    word_string = word_string.replace("Ü", "UE")

    word_string = word_string.replace("â", "aA")
    word_string = word_string.replace("Â", "AA")
    word_string = word_string.replace("ã", "aN")
    word_string = word_string.replace("Ã", "AN")
    word_string = word_string.replace("ôi", "oY")
    word_string = word_string.replace("Ôi", "OY")
    word_string = word_string.replace("êi", "eY")
    word_string = word_string.replace("Êi", "EY")
    return word_string



if __name__ == '__main__':
    #textgrid_path = "/home/nianheng/Documents/hiwi/12December/Nianheng_Fabian/TextGrids_problem/"
    #audio_path = "/home/nianheng/Documents/hiwi/12December/Nianheng_Fabian/Wav_processed_no_names/"
    #path = '/home/nianheng/Documents/hiwi/01januar/karen/test/'
    path = "/home/nianheng/Documents/hiwi/01januar/karen/done_Rawdata/"
    problem = '/home/nianheng/Documents/hiwi/01januar/karen/problem/'
    empty_path = "/home/nianheng/Documents/hiwi/01januar/karen/empty/"
    none_empty_path = "/home/nianheng/Documents/hiwi/01januar/karen/none_empty/"

    textgrid_file_name = read_file_names(path)

    for each_file_name in textgrid_file_name:
        try:
            phrase_list = read_text_grid(path+each_file_name)
            strip_txt_and_wav(phrase_list, path, empty_path, none_empty_path, each_file_name)
        except Exception as e:
            print(each_file_name)
            shutil.copy(path + each_file_name, problem + each_file_name)
            shutil.copy(path + each_file_name[:-9]+'.wav', problem + each_file_name[:-9]+'.wav')
            traceback.print_tb(e.__traceback__)



