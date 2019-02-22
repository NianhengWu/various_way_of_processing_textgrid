import os
import regex
import shutil
import csv
import traceback

double_dash = regex.compile("-[a-zA-ZäöüÄÖÜß]+-")
half_word = regex.compile("[a-zA-ZäöüÄÖÜß]+---")
person_name = regex.compile("{[a-zA-ZäöüÄÖÜß]+}")
other_name = regex.compile("'''[a-zA-ZäöüÄÖÜß]+'''")

base = regex.compile('[a-zA-ZäöüÄÖÜß]+')


def output_as_csv(outputpath, word_count, word, file_name, symbol):

    with open(outputpath, mode='a', newline="") as output_file:
        csv_writer = csv.writer(output_file, delimiter=',', quotechar="'", quoting=csv.QUOTE_MINIMAL)
        csv_writer.writerow([file_name[:-4]+".TextGrid", word_count, word, symbol])


def create_a_csv(outputpath):

    with open(outputpath, 'w', newline="") as create_the_csv:
        csv_writer = csv.writer(create_the_csv, delimiter=',', quotechar="'", quoting=csv.QUOTE_MINIMAL)
        csv_writer.writerow(
            ['Filename', 'Word_Count', 'Word', 'Symbol'])
    create_the_csv.close()


def read_file_names(path):
    file_list = filter(lambda x: x.endswith('.txt'), os.listdir(path))
    return file_list


def read_txt(path, each_file_name, outputpath):
    with open(path+each_file_name, 'r', encoding='utf8') as f:
        line = f.readline()
    line_list = line.strip().split()
    word_count = 0
    for each_word in line_list:
        if double_dash.match(each_word) is not None:
            match_part = double_dash.match(each_word)
            output_as_csv(outputpath, word_count, match_part.group()[1:-1], each_file_name, 'dash')
        elif half_word.match(each_word) is not None:
            match_part = half_word.match(each_word)
            output_as_csv(outputpath, word_count, match_part.group()[:-3], each_file_name, 'half')
        elif person_name.match(each_word) is not None:
            match_part = person_name.match(each_word)
            output_as_csv(outputpath, word_count, match_part.group()[1:-1], each_file_name, 'person')
        elif other_name.match(each_word) is not None:
            match_part = other_name.match(each_word)
            output_as_csv(outputpath, word_count, match_part.group()[3:-3], each_file_name, 'other')
        if base.search(each_word) is not None:
            word_count += 1


if __name__ == '__main__':
    path = "/home/nianheng/Documents/hiwi/01januar/karen/none_empty_done/"
    # path = '/home/nianheng/Documents/hiwi/02februar/test/'
    problem = '/home/nianheng/Documents/hiwi/01januar/karen/trash/'

    outputpath = '/home/nianheng/Documents/hiwi/01januar/karen/symbol_record.csv'

    textgrid_file_name = read_file_names(path)
    create_a_csv(outputpath)

    for each_file_name in textgrid_file_name:
        try:
            file_list = read_file_names(path)
            read_txt(path, each_file_name, outputpath)
        except Exception as e:
            print(each_file_name)
            shutil.copy(path + each_file_name, problem + each_file_name)
            traceback.print_tb(e.__traceback__)
