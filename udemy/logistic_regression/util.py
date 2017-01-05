import numpy as np

def cross_entropy(T, Y):
    # -sum(t_i*ln(y_i) + (1-t_i)ln(1-y_i)) = -sum(t_i*ln(y_i)) - sum((1-t_i)ln(1-y_i))
    return -np.dot(T, np.log(Y)) - np.dot(1-T, np.log(1-Y))

def sigmoid(a):
    return 1 / (1 + np.exp(-a))
