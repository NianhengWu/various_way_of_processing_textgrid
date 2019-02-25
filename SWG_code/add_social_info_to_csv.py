import pandas

class AddSocialInfo:

    def __init__(self, rootpath, sourcepath):
        self.rootpath = rootpath
        self.sourcepath = sourcepath

        self.df_root = pandas.read_csv(rootpath, encoding = 'utf-8-sig')
        self.df_source = pandas.read_csv(sourcepath, encoding = 'utf-8-sig')

        self.df_root = self.df_root.astype(str)
        self.df_source = self.df_source.astype(str)

    def read_name(self):
        self.df_root['id'] = self.df_root.Filename.str[:7]
        self.df_root['sort'] = self.df_root.Filename.str[:7]
        self.df_root = self.df_root
        self.df_root = pandas.merge(self.df_root, self.df_source, on='id', how='outer')
        self.df_root = self.df_root.drop('id', axis= 1)
        self.df_root = self.df_root.replace('nan', '')
        self.df_root.to_csv('/home/nianheng/Documents/hiwi/02februar/SWG_withsocialinfo_20190210.csv')

if __name__ == '__main__':
    source_path = '/home/nianheng/Documents/hiwi/02februar/SWG speaker social factors.csv'
    root_path = '/home/nianheng/Documents/hiwi/02februar/SWG_results_20190210.csv'
    addSocialInfo = AddSocialInfo(root_path, source_path)
    addSocialInfo.read_name()