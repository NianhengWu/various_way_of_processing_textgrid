"""
Author: Nianheng Wu
xjtuwunianheng@gmail.com, Eberhard Karls Universität Tübingen
"""
import os
import errno
import csv
import textgrid
import codecs

class Transform:

    def __init__(self, rootpath, outputpath):
        self.rootpath = rootpath
        self.outputpath = outputpath

    def start(self):
        try:
            os.chdir(rootpath)
        except FileNotFoundError:
            print(errno.EPERM)
        if not os.path.exists(outputpath):
            self.create_a_csv()
        self.get_file_list()

    def get_file_list(self):
        file_list = os.listdir(self.rootpath)
        self.read_from_textgrid(file_list)

    def read_from_textgrid (self, file_list):
        '''
        Assume: a loads of textgrids in a path, each of which has a 'words' tier and a 'segments' tier.
                Segments tier includes all segments of every word in SAMPA.
        Read all textgrids files one by one. Read every segment corresponding to certain word and output
        infomation about this word into csv. Information including: starting/ending time, word, segments of
        the word, starting/ending time of this segment.
        :param file_list: all files in the path
        :return: nothing
        '''
        for each_file_name in file_list:
            interval_num = 0
            file_path = rootpath + each_file_name
            try:
                file_textgrid_obj = textgrid.TextGrid.fromFile(file_path)
            except UnicodeDecodeError:
                print(each_file_name + ': the encode is weird, not utf-8 or ansi')
                '''
                try:
                    converted_file_path = self.convert_to_uft8(file_path)
                    file_textgrid_obj = textgrid.TextGrid.fromFile(converted_file_path)
                except UnicodeDecodeError:
                    print(each_file_name+': the encode is weird, not utf-8 or ansi')
                '''

            tier_list = file_textgrid_obj.tiers

            for each_tier in tier_list:
                if each_tier.name == 'words':
                    tier_words = each_tier
                    intervals_words = tier_words.intervals
                elif each_tier.name == 'segments':
                    tier_segments = each_tier
                    intervals_segments = tier_segments.intervals

            try:
                for each_word in intervals_words:
                    word_start_time = each_word.minTime
                    word_end_time = each_word.maxTime
                    word_mark = each_word.mark
                    try:
                        while (intervals_segments[interval_num].minTime >= word_start_time) & (intervals_segments[interval_num].maxTime <= word_end_time):
                            segment_start_time = intervals_segments[interval_num].minTime
                            segment_end_time = intervals_segments[interval_num].maxTime
                            segment_mark = intervals_segments[interval_num].mark
                            self.output_as_csv(each_file_name, word_start_time, word_end_time, word_mark, segment_start_time, segment_end_time,segment_mark)
                            interval_num += 1
                    except IndexError:
                        interval_num = 0
            except AttributeError:
                print('tier words is empty or does not exist ')



    def output_as_csv (self, source_filename, word_start_time, word_end_time, word, segment_start_time, segment_end_time, segment):
        '''
        Get infomation of a word and it's segment and wirte into csv file as a line.
        :param source_filename: file's name
        :param word_start_time: start time of the word
        :param word_end_time: end time of the word
        :param word: value of the word
        :param segment_start_time: start time of the segment
        :param segment_end_time: end time of the segment
        :param segment: value of the segment
        :return: nothing
        '''
        with open(outputpath, mode = 'a') as output_file:
            csv_writer = csv.writer(output_file, delimiter=',', quotechar ='"', quoting=csv.QUOTE_MINIMAL)
            csv_writer.writerow([source_filename, word_start_time, word_end_time, word, segment_start_time, segment_end_time, segment])

    def create_a_csv (self):
        '''
        If the csv file you want to output does not exist in your path, this function will create it.
        '''
        with open (outputpath, 'w') as create_the_csv:
            csv_writer = csv.writer(create_the_csv, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            csv_writer.writerow(['Filename','Word_Start_Time','Word_End_Time','Word','Segment_Start_Time','Segment_End_Time','Segment'])
        create_the_csv.close()

    def convert_to_uft8 (self, file_path):
        '''
        Sometimes the textgrids are encoded by very weird ANSI code. This function means to convert ANSI to uft-8
        :param file_path: file path before converted
        :return: file path after converted
        '''
        with codecs.open(file_path, 'r', encoding = 'Windows-1251') as before_converted:
            lines = before_converted.read()
        with codecs.open (file_path, 'w', encoding= 'utf8') as after_converted:
            after_converted.write(lines)
        return file_path

if __name__ == '__main__':
    rootpath = '/home/nianheng/Desktop/notworking/'
    outputpath = '/home/nianheng/Documents/hiwi/10october/SWG/SWG_results.csv'
    transform = Transform(rootpath,outputpath)
    transform.start()
