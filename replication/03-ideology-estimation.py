#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
03-ideology-estimation.py
Purpose: estimate opinion leaders ideology
@author: Keisuke Idemitsu
"""

import os
## set working directory
os.chdir("/Users/XXXXXXXXXX/")

import functions as fun
import pyreadr

data_folder = os.getcwd() + "/data/"  ## designate data store folder
followers_folder = data_folder + "followers_lists/"  ## designate folder of followers lists


## load opinion leader data
freq_accounts = pyreadr.read_r(data_folder + "freq_accounts.rdata")["freq_accounts"]

### get a list of followers who follow at least 5 popular accounts
userlsJP, filesListJP, censusJP = fun.getUserlist(followers_folder, freq_accounts, 5)
### get follow matrix
y_imm_JP = fun.convertToMatrix(userlsJP, filesListJP, censusJP)
### conduct Correspondence Analysis
ca_immJP, colcoord_immJP, rowcoord_immJP, res_immJP = fun.conductCA(y_imm_JP)
res_immJP.columns = ["immDim1","immDim2","immDim3","immDim4"]
### transfer to accounts file
freq_accounts = freq_accounts.merge(res_immJP, left_on="screen_name", right_index=True)


## save files
pyreadr.write_rdata(data_folder + "freq_accounts.rdata", freq_accounts, df_name="freq_accounts")
file_name = data_folder + "y_imm_JP.pkl"
y_imm_JP.to_pickle(file_name)
