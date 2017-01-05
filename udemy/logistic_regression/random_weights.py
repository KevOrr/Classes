#!/usr/bin/env python3

# Section 2.9
# Import sample data, and calculate output of logistic regression with random weights

import numpy as np

from process import get_data
from util import sigmoid

# Calculate sigmoid of weighted sum plus bias
def forward(X, W, b):
    return sigmoid(X.dot(W) + b)

# Calculate prediction accuracy
def classification_rate(Y, P):
    return np.mean(Y == P)

X, Y = get_data(open('ecommerce_data.csv'))

D = X.shape[1]

# 1e4 random trials
total = 0
for i in range(10000):
    W = np.random.randn(D)
    b = 0
    predictions = np.round(forward(X, W, b))
    total += classification_rate(Y, predictions)
print(total / 10000)
