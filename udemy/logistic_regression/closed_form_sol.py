#!/usr/bin/env python3

import numpy as np
from logistic_predict import sigmoid


def cross_entropy(T, Y):
    # -sum(t_i*ln(y_i) + (1-t_i)ln(1-y_i)) = -sum(t_i*ln(y_i)) - sum((1-t_i)ln(1-y_i))
    return -np.dot(T, np.log(Y)) - np.dot(1-T, np.log(1-Y))

if __name__ == '__main__':
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
