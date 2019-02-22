"""
Author: Nianheng Wu
xjtuwunianheng@gmail.com, Eberhard Karls Universität Tübingen
"""
import os
import errno
import csv
import textgrid
import traceback
import regex


class Transform:

    def __init__(self, rootpath, outputpath, the_maps):
        self.rootpath = rootpath
        self.outputpath = outputpath
        self.maps = the_maps
        self.no_ddm = set()

    def start(self):
        try:
            os.chdir(root_path)
        except FileNotFoundError:
            print(errno.EPERM)
        if not os.path.exists(output_path):
            self.create_a_csv()
        self.get_file_list()

    def get_file_list(self):
        file_list = os.listdir(self.rootpath)
        self.read_from_textgrid(file_list)

    def read_from_textgrid (self, file_list):
        for each_file_name in file_list:
            interval_num = 0
            file_path = root_path + each_file_name
            try:
                file_textgrid_obj = textgrid.TextGrid.fromFile(file_path)
            except UnicodeDecodeError:
                print(each_file_name + ': the encode is weird, not utf-8 or ansi')

            tier_list = file_textgrid_obj.tiers

            for each_tier in tier_list:
                if each_tier.name == 'words':
                    tier_words = each_tier
                    intervals_words = tier_words.intervals
                elif each_tier.name == 'segments':
                    tier_segments = each_tier
                    intervals_segments = tier_segments.intervals

            count = 0

            try:
                for each_word in intervals_words:
                    word_start_time = each_word.minTime
                    word_end_time = each_word.maxTime
                    word_mark = each_word.mark

                    if (each_file_name, str(count)) in symbol_map.keys():
                        print(each_file_name)
                        value = symbol_map[(each_file_name, str(count))]
                        if value[0] == 'dash' and word_mark == value[1]:
                            word_mark = '-'+word_mark+'-'
                            print(word_mark)
                        elif value[0] == 'person' and word_mark == value[1]:
                            word_mark = '{removed}'
                        elif value[0] == 'other' and word_mark == value[1]:
                            word_mark = "'''removed'''"

                    else:
                        word_mark = self.recover(word_mark)

                    if word_mark in self.no_ddm:
                        DDM = ""
                        std = ""
                    else:
                        DDM, std = self.match_ddm(word_mark)
                        DDM = " ".join(DDM)
                        std = " ".join(std)
                        DDM_std = self.match_std(word_mark)
                        DDM_std = " ".join(DDM_std)
                        if DDM == "" and DDM_std == "":
                            self.no_ddm.add(word_mark)
                        else:
                            DDM = " ".join([DDM, DDM_std]).strip()

                    try:
                        while (intervals_segments[interval_num].minTime >= word_start_time) & \
                                (intervals_segments[interval_num].maxTime <= word_end_time):
                            segment_start_time = intervals_segments[interval_num].minTime
                            segment_end_time = intervals_segments[interval_num].maxTime
                            segment_mark = intervals_segments[interval_num].mark
                            self.output_as_csv(each_file_name[each_file_name.rfind("_")+1:-9], count, each_file_name,
                                               word_start_time, word_end_time, word_mark, segment_start_time,
                                               segment_end_time, segment_mark, DDM, std)
                            interval_num += 1
                    except IndexError:
                        interval_num = 0
                    if word_mark != '<P>':
                        count += 1

            except AttributeError as e:
                print(each_file_name+': tier words is empty or does not exist ')
                traceback.print_tb(e.__traceback__)

    def match_std(self, word):
        DDM2 = []
        for key in std_map.keys():
            if key.match(word) is not None:
                for each_value in std_map[key]:
                    DDM2.append(each_value+'-')
        return DDM2

    def match_ddm(self, word):
        ddm_list = []
        german_list = []
        for key in maps.keys():
            if key.match(word) is not None:
                for each_value in maps[key]:
                    swg = each_value[0]
                    std = each_value[1]
                    swg = swg.replace("^", "")
                    swg = swg.replace("$", "")
                    swg = swg.replace(".?", "")

                    std = std.replace("^", "")
                    std = std.replace("$", "")
                    std = std.replace(".?", "")

                    ddm_list.append(each_value[2]+'+')
                    german = word.replace(swg, std)
                    german = german.replace("^", "")
                    german = german.replace("$", "")
                    german_list.append(german)

        return ddm_list, german_list

    def recover(self, word_string):
        word_string = word_string.replace("aE", "ä")
        word_string = word_string.replace("oE", "ö")
        word_string = word_string.replace("uE", "ü")
        word_string = word_string.replace("sS", "ß")
        word_string = word_string.replace("AE", "Ä")
        word_string = word_string.replace("OE", "Ö")
        word_string = word_string.replace("UE", "Ü")

        word_string = word_string.replace("aA", "â")
        word_string = word_string.replace("AA", "Â")
        word_string = word_string.replace("aN", "ã")
        word_string = word_string.replace("AN", "Ã")
        word_string = word_string.replace("oY", "ôi")
        word_string = word_string.replace("OY", "Ôi")
        word_string = word_string.replace("eY", "êi")
        word_string = word_string.replace("EY", "Êi")
        return word_string

    def output_as_csv (self, fileid, wordid, source_filename, word_start_time, word_end_time, word, segment_start_time,
                       segment_end_time, segment, DDM, std):
        with open(output_path, mode = 'a', newline="") as output_file:
            csv_writer = csv.writer(output_file, delimiter=',', quotechar ='"', quoting=csv.QUOTE_MINIMAL)
            csv_writer.writerow([fileid, wordid, source_filename, word_start_time, word_end_time, word,
                                 segment_start_time, segment_end_time, segment, DDM, std])

    def create_a_csv (self):
        """
        If the csv file you want to output does not exist in your path, this function will create it.
        """
        with open (output_path, 'w', newline="") as create_the_csv:
            csv_writer = csv.writer(create_the_csv, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
            csv_writer.writerow(['File_ID','Word_ID','Filename','Word_Start_Time','Word_End_Time','Word_SWG',
                                 'Segment_Start_Time','Segment_End_Time','Segment', 'DDM', 'Word_German'])
        create_the_csv.close()


def read_ddm_files(path):
    file_list = filter(lambda x: x.endswith('.csv'), os.listdir(path))
    return file_list


def read_ddm_csv(path, filename):
    with open(path+filename, 'r', encoding="utf8") as file:
        reader = csv.reader(file)
        first = next(reader)
        if (len(first) == 2) and (first[1] != ""):
            file.seek(0)
            for row in reader:
                row = make_pattern(row)
                if row[0] != "" and row[1] != "":
                    compiled_pattern = regex.compile(row[0])
                    compiled_pattern2 = regex.compile(row[1])

                    if compiled_pattern not in maps.keys():
                        maps[compiled_pattern] = [(row[0], row[1], filename[:-4])]
                    else:
                        maps[compiled_pattern].append((row[0], row[1], filename[:-4]))

                    if compiled_pattern2 not in std_map.keys():
                        std_map[compiled_pattern2] = {filename[:-4]}
                    else:
                        std_map[compiled_pattern2].add(filename[:-4])

        elif (len(first) == 1) or (first[1] == ""):
            file.seek(0)
            for row in reader:
                row = make_pattern(row)
                if row[0] != "":
                    compiled_pattern = regex.compile(row[0])
                    if row[0] not in maps.keys():
                        maps[compiled_pattern] = [(row[0], row[0], filename[:-4])]
                    else:
                        maps[compiled_pattern].append((row[0], row[0], filename[:-4]))

    return maps


def make_pattern(line_list):
    new_line_list = []
    for each_word in line_list:
        each_word = each_word.strip()
        each_word = each_word.replace("\ufeff", "")
        if each_word.startswith("*"):
            pattern = '.?'+each_word[1:]
        else:
            pattern = "^"+each_word

        if each_word.endswith("xxx"):
            pattern = pattern[:-3]

        if pattern.endswith("*"):
            pattern = pattern[:-1]+'.?'
        else:
            pattern = pattern+"$"

        pattern = pattern.replace("[", "(")
        pattern = pattern.replace("]", ")?")
        new_line_list.append(pattern)
        print(new_line_list)

    return new_line_list


def read_record():
    the_symbol_map = {}
    with open(record_path) as record:
        record_reader = csv.reader(record)
        next(record_reader)
        for record_lines in record_reader:
            the_symbol_map[(record_lines[0], record_lines[1])] = (record_lines[3], record_lines[2])
    return the_symbol_map


if __name__ == '__main__':
    record_path = "/home/nianheng/Documents/hiwi/02februar/symbol_record.csv"
    root_path = '/home/nianheng/Documents/hiwi/01januar/karen/none_empty_aligned/'
    output_path = '/home/nianheng/Documents/hiwi/02februar/SWG_results_20190210.csv'
    ddm_path = '/home/nianheng/Documents/hiwi/02februar/DDM/'
    done_path = "/home/nianheng/Documents/hiwi/02februar/done/"
    maps = {}
    std_map = {}

    symbol_map = read_record()
    ddm_file_list = read_ddm_files(ddm_path)

    for each_file in ddm_file_list:
        read_ddm_csv(ddm_path, each_file)
    print(maps)
    transform = Transform(root_path, output_path, maps)
    transform.start()
