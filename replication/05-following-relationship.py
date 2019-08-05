#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
05-following-relationship.py
Purpose: check following relationship between followers and opinion leaders
@author: Keisuke Idemitsu
"""

import os
## set working directory
os.chdir("/Users/XXXXXXXXXX/")

import pyreadr
import numpy as np
import pandas as pd

data_folder = os.getcwd() + "/data/"  ## designate data store folder

## load opinion leader data
freq_accounts = pyreadr.read_r(data_folder + "freq_accounts.rdata")["freq_accounts"]

## create lists of each category opinion leaders
lib_leaders = freq_accounts.loc[freq_accounts["cluster"] == 1, "screen_name"]
con_leaders = freq_accounts.loc[
        (freq_accounts["cluster"] == 2) | (freq_accounts["cluster"] == 3) | (freq_accounts["cluster"] == 5),
        "screen_name"]
rw_leaders = freq_accounts.loc[freq_accounts["cluster"] == 3, "screen_name"]
media_leaders = freq_accounts.loc[freq_accounts["cluster"] == 4, "screen_name"]
matome_leaders = freq_accounts.loc[freq_accounts["cluster"] == 5, "screen_name"]

## load matrix of follow relationship from 03-ideology-estimation
file_name = data_folder + "y_imm_JP.pkl"
y_imm_JP = pd.read_pickle(file_name)

## create rows of the number of accounts each follower follows for each category
lib_follow_all = np.sum(y_imm_JP.loc[:, lib_leaders.values.tolist()], axis=1)/len(lib_leaders)
con_follow_all = np.sum(y_imm_JP.loc[:, con_leaders.values.tolist()], axis=1)/len(con_leaders)
rw_follow_all = np.sum(y_imm_JP.loc[:, rw_leaders.values.tolist()], axis=1)/len(rw_leaders)
media_follow_all = np.sum(y_imm_JP.loc[:, media_leaders.values.tolist()], axis=1)/len(media_leaders)
matome_follow_all = np.sum(y_imm_JP.loc[:, matome_leaders.values.tolist()], axis=1)/len(matome_leaders)

## merge them
all_follow = pd.concat([lib_follow_all, con_follow_all, rw_follow_all, media_follow_all, matome_follow_all], axis=1)
all_follow.columns = ["liberal","conservative","right_wing", "media","matome"]
all_follow["id_str"] = all_follow.index

## save file
pyreadr.write_rdata(data_folder + "all_follow.rdata", all_follow, df_name="all_follow")
