import datetime
import concurrent.futures
import queue
import praw
import pandas as pd
import time
reddit_read_only = praw.Reddit(client_id="your client id",		 # your client id
							client_secret="your client secret",	 # your client secret
							user_agent="your user agent")	 # your user agent



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