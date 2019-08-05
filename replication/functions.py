#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
functions.py
Purpose: create functions to use ideology estimation
@author: Keisuke Idemitsu
"""

import pyreadr
import glob
import re
from collections import OrderedDict
import numpy as np
import pandas as pd
import prince


def main():
    pass


def getUserlist(followers_folder, accounts, count):
    '''Assumes "followers_folder" is a folder path, "accounts" is a dataframe,
    "count" is a minimum number of following accounts for each users.
    Returns a long list of username who follow more than "count" of 
    "census" (=popular accounts) and a file paths of followers list.'''
    ## load file
    census = list(accounts['screen_name'])
    
    ## choose files in census
    filesList = glob.glob(followers_folder + "*")
    filesListraw = [re.sub(followers_folder, "", x) for x in filesList]
    filesListraw = [re.sub(".rdata", "", x) for x in filesListraw]
    census = list(set(census) & set(filesListraw))
    
    ## create file path list
    filesList = [followers_folder + x + ".rdata" for x in census]
    
    ## create a function to count element
    def element_count(ls, dic):
        '''Assumes "ls" as a list, "dic" as a dictionary
        which key is each element and value is the number of counts.
        Returns a new dictionary append information from a list.'''
        for i in ls:
            if i in dic:
                dic[i] += 1
            else:
                dic[i] = 1
        return dic
    
    ## load file and count element
    userdic = dict()
    for i in filesList:
        followers = list(pyreadr.read_r(i)['followers']['followers'])
        userdic = element_count(followers, userdic)
    userls = [x for x,y in userdic.items() if y >= count]
    return(userls, filesList, census)


def convertToMatrix(userls, filesList, census):
    '''Assumes "userls" as a list of followers, "filesList" is a list of file paths,
    "census" is a list of popular users.
    Returns a large matrix which rows are census and columns are userlist.'''
    y = OrderedDict()
    zeros = list()
    for i,j in enumerate(filesList):
        followers = list(pyreadr.read_r(j)['followers']['followers'])
        y[i] = np.isin(userls, followers)*1
        if sum(y[i]) == 0:
            print(census[i], "is followed by 0 users!")
            zeros.append(census[i])
    y = pd.DataFrame.from_dict(y)
    y.index = userls
    y.columns = census
    y = y.drop(columns=zeros)  ## drop census who is followed by zero users
    return(y)


def conductCA(y):
    ca = prince.CA(
        n_components=4,
        n_iter=5,
        copy=True,
        check_input=True,
        engine='auto',
        random_state=42
        )
    ca = ca.fit(y)
    phi = ca.column_coordinates(y)
    theta = (phi - phi.mean()) / phi.std()
    rowcoord = ca.row_coordinates(y)
    return(ca, phi, rowcoord, theta)


if __name__=='__main__':
    main()
