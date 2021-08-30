# -*- coding: utf-8 -*-
# @Time    : 8/9/2021 1:32 PM
# @Author  : Paulo Radatz
# @Email   : pradatz@epri.com
# @File    : data_analysis.py
# @Software: PyCharm

import pandas as pd
import pathlib
import os
import matplotlib.pyplot as plt
import seaborn as sns

script_path = os.path.dirname(os.path.abspath(__file__))
output_file = pathlib.Path(script_path).joinpath("outputs", "results.csv")

df = pd.read_csv(output_file, index_col=0)

for column in df.columns:
    sns.barplot(x=df.index, y=column, data=df)
    plt.show()


