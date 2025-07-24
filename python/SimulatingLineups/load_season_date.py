import pybaseball
import pandas as pd
from pathlib import Path

# TODO AMM - Make the year an argument
DATA_FILE_PATH = "data/batting_stats_2024.csv"

file_path = Path(DATA_FILE_PATH)
if not file_path.exists():
    print("Getting data from Fangraphs")
    data = pybaseball.batting_stats(2024, qual=350)
    data.to_csv(DATA_FILE_PATH)
else:
    print("Getting data from existing csv")
    data = pd.read_csv(DATA_FILE_PATH)
