#!/usr/bin/env python3

# Section 3.13
# Calculate cross-entropy error for random weights, and for closed-form solution to bayes classifier

import numpy as np
from util import sigmoid, cross_entropy

N = 100
D = 2

means = np.array(((-2,-2), (2,2)))
covar = np.eye(2)

# Artifically create 2 classes, center first 50 points at (-2,-2), last 50 at (2,2)
X = np.random.randn(N, D)
X[:N//2, :] = X[:N//2, :] + means[0] * np.ones((N//2,D))
X[N//2:, :] = X[N//2:, :] + means[1] * np.ones((N//2,D))
Xb = np.concatenate((np.ones((N, 1)), X), axis=1)

# Class labels, first 50 are 0, last 50 are 1
T = np.concatenate((np.zeros((N//2,)), np.ones((N//2,))))

# Random weights
w = np.random.randn(D+1)
Y = sigmoid(Xb @ w)
print('Random weights:', cross_entropy(T, Y))

# Closed form Bayes solution
w = ((means[1, None] - means[0, None]) @ np.linalg.inv(covar)).T
w = np.concatenate(((0,), w.reshape(D))) # Add weight for bias
Y = sigmoid(Xb @ w)
print('Closed form solution:', cross_entropy(T, Y))
