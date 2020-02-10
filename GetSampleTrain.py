# Importing libraries
import pandas as pd
import numpy as np

# train dataset size
SIZE = 74180465

# There are seven weeks
round(SIZE/7)

# Getting subsamples of size round(SIZE/7)
df_chunk = pd.read_csv("data/train.csv.zip", chunksize=round(SIZE/7))

# creating a list for store chunks
chunk_list = []  # append each chunk df here 

# Each chunk is in df format
for chunk in df_chunk:
	# Getting sample of each chunk and appending to a list
    if (len(chunk.index) <= 1400000):
        index = np.random.choice(len(chunk.index), replace=False, size=1400000)
        df = chunk.iloc[index]
        chunk_list.append(df)

# Concatenate all dataframes
df = pd.concat(chunk_list)

# Eliminating possible duplicates
df = df.drop_duplicates()

# Converts to csv and saves the dataframe
df.to_csv("data/sample_train.csv")