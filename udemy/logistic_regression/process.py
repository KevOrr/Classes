#!/usr/bin/env python3

# Section 2.8

import numpy as np
import pandas as pd

def get_data(f):
    data = pd.read_csv(f).as_matrix()

    X = data[:, :-1]
    Y = data[:, -1]

    # Normalize N-products_viewed and Visit_duration
    X[:,1] = (X[:,1] - X[:,1].mean()) / X[:,1].std()
    X[:,2] = (X[:,2] - X[:,2].mean()) / X[:,2].std()

    # One-hot encode time
    n,_ = X.shape
    onehot_times = np.zeros((n, 4))
    onehot_times[np.arange(n), X[:,4].astype(int)] = 1
    X = np.concatenate((X[:, :-1], onehot_times), axis=1)

    # Logistic regression can only make a binary classification, so there can
    # only be two groups to classify
    return X[(Y == 0) | (Y == 1)], Y[(Y == 0) | (Y == 1)]
