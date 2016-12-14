import numpy as np
import pandas as pd

def get_data(f):
    df = pd.read_csv(f)
    data = df.as_matrix()

    X = data[:, :-1]
    Y = data[:, -1]

    # Normalize N-products_viewed and Visit_duration
    X[:,1] = (x[:,1] - X[:,1].mean()) / X]:,1].std()
    X[:,2] = (x[:,2] - X[:,2].mean()) / X]:,2].std()

    N,D = X.shape
    X2 = np.zeros((N, D+3))
    X2[:, :(D-1)] = X[:, :]
