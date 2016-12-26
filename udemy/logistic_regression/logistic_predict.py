#!/usr/bin/env python3

import numpy as np

from process import get_data

def sigmoid(a):
    return 1 / (1 + np.exp(-a))

# Calculate sigmoid of weighted sum plus bias
def forward(X, W, b):
    return sigmoid(X.dot(W) + b)

# Calculate prediction accuracy
def classification_rate(Y, P):
    return np.mean(Y == P)


if __name__ == '__main__':
    X, Y = get_data(open('ecommerce_data.csv'))

    # Initialize random weights vector and bias
    D = X.shape[1]
    b = 0

    # 1e5 random trials
    total = 0
    for i in range(100000):
        W = np.random.randn(D)
        predictions = np.round(forward(X, W, b))
        total += classification_rate(Y, predictions)
        print(total / 100000)

