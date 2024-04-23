import datetime
import concurrent.futures
import queue
import praw
import pandas as pd
import time
reddit_read_only = praw.Reddit(client_id="b3I8cuQl46is2L103Wyezw",		 # your client id
							client_secret="bEIg7_ZhodfmQ1aLeoSmZyqXu6jr-Q",	 # your client secret
							user_agent="alozxw")	 # your user agent



posts_dict = {"Title": [], "Post Text": [], "Time": []}


subreddit = reddit_read_only.subreddit("Pets")

posts = subreddit.new(limit=1000)
for post in posts:
    time = post.created_utc
    created_datetime = datetime.datetime.fromtimestamp(time)
    posts_dict["Title"].append(post.title)
    posts_dict["Post Text"].append(post.selftext)
    posts_dict["Time"].append(created_datetime)


posts = subreddit.top(time_filter='year', limit=1000)
for post in posts:
    time = post.created_utc
    created_datetime = datetime.datetime.fromtimestamp(time)
    posts_dict["Title"].append(post.title)
    posts_dict["Post Text"].append(post.selftext)
    posts_dict["Time"].append(created_datetime)

posts = subreddit.top(time_filter='month', limit=1000)
for post in posts:
    time = post.created_utc
    created_datetime = datetime.datetime.fromtimestamp(time)
    posts_dict["Title"].append(post.title)
    posts_dict["Post Text"].append(post.selftext)
    posts_dict["Time"].append(created_datetime)


posts = subreddit.hot(limit=1000)
for post in posts:
    time = post.created_utc
    created_datetime = datetime.datetime.fromtimestamp(time)
    posts_dict["Title"].append(post.title)
    posts_dict["Post Text"].append(post.selftext)
    posts_dict["Time"].append(created_datetime)


all_posts = pd.DataFrame(posts_dict)

all_posts = all_posts.sort_values('Time', ascending=False)


all_posts.to_csv("data.csv", index=False)