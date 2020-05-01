import datetime as dt
from psaw import PushshiftAPI  # pip install psaw
import pandas as pd
from pathlib import Path


def getposts(subname, output_dir_string):
    stopTime = 1420070461  # 01-01-2015
    api = PushshiftAPI()
    gen = api.search_submissions(subreddit=subname,after=stopTime)
    print('gen completed')
    submissions = list(gen)
    df = pd.DataFrame([obj.d_ for obj in submissions])
    print('df created')
    postDF = df
    output_file_posts = subname + "_p.csv"
    output_dir_posts = Path(output_dir_string)
    output_dir_posts.mkdir(parents=True, exist_ok=True)
    postDF.to_csv(output_dir_posts / output_file_posts)
    print('csv saved')

def getcomments(subname, output_dir_string):
    stopTime = 1420070461  # 01-01-2015
    api = PushshiftAPI()
    gen = api.search_comments(subreddit=subname, after=stopTime)
    print('gen completed')
    comments = list(gen)
    df = pd.DataFrame([obj.d_ for obj in comments])
    print('df created')
    output_file_comments = subname + "_c.csv"
    output_dir_comments = Path(output_dir_string)
    output_dir_comments.mkdir(parents=True, exist_ok=True)
    df.to_csv(output_dir_comments / output_file_comments)
    print('csv saved')


pathstr = '\\Users\\bolde\\Desktop'
