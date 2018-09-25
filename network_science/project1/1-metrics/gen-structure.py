#!/usr/bin/env python3

import sys
import argparse
import random
import time

import networkx as nx
import scipy as sp, scipy.sparse
import numpy as np
import matplotlib.pyplot as plt

SAMPLING = [10, 100, 1000]

def neighborhoods(g, nodes):
    return {v for node in nodes for v in g.neighbors(node)}

def k_neighborhoods(g, nodes, k):
    current_set = set(nodes)
    for i in range(k):
        current_set = neighborhoods(g, current_set)

    return current_set

def node_by_attr(g, value, attr='old_label'):
    return next(filter(lambda n, d: d[attr] == value, g.nodes(data=True)))

def approximate_diameter(g, n_nodes):
    if n_nodes <= 0:
        raise ValueError('n_nodes must be positive')

    nodes = random.sample(g.nodes, n_nodes)

    total = 0
    for i in range(len(nodes)):
        for j in range(i+1, len(nodes)):
            try:
                total += nx.shortest_path_length(g, nodes[i], nodes[j])
            except nx.exception.NetworkXNoPath:
                pass

    return total / (len(nodes) * (len(nodes) - 1) / 2)

def fit_powerlaw(X, Y):
    def func_powerlaw(x, c, m):
        return c * x**m

    (c, m), _ = sp.optimize.curve_fit(func_powerlaw, X, Y, p0=np.asarray([2e4, 4]))
    return c, m

def floyd_warshall(g):
    A = nx.to_numpy_matrix(G, nodelist=nodelist, multigraph_weight=min,
                           weight=weight, nonedge=np.inf)
    n, m = A.shape
    I = np.identity(n)
    A[I == 1] = 0  # diagonal elements should be zero
    start = time.time()
    for i in range(n):
        print('Floyd-Warshall {}/{}'.format(i, n))
        A = np.minimum(A, A[i, :] + A[:, i])
    print('Floyd-Warshall time: {}'.format(time.time() - start))
    return A


def main(f):
    # Read in graph
    print('Loading graph from {}'.format(f.name))
    g = nx.read_edgelist(f, comments='#', nodetype=int)
    g = nx.convert_node_labels_to_integers(g, label_attribute='old_label')

    # Count nodes and edges
    print('Number of nodes in {}: {}'.format(f.name, len(g.nodes)))
    print('Number of edges in {}: {}'.format(f.name, len(g.edges)))

    # Get adjacency matrix, degree vector
    adj = sp.sparse.csr_matrix(nx.adjacency_matrix(g))
    degrees = np.array(adj.sum(0))[0]
    max_degree = degrees.max()

    # Find nodes with degree=1, and degree=max_degree
    loners = list(np.where(degrees == 1)[0])
    socialites = list(np.where(degrees == max_degree)[0])
    print('Number of nodes with degree=1 in {}: {}'.format(f.name, len(loners)))
    print('Node ids with highest degree ({}) in {}: {}'.format(
        max_degree, f.name, ', '.join(str(g.node[v]['old_label']) for v in socialites)))

    # Print average degree of each neighborhood of degree-1 nodes
    # for loner in loners:
    #     print('The average degree of {}\'s 2-hop neighborhood is {}'.format(
    #         loner,
    #         degrees[list(k_neighborhoods(g, [loner], 2))].mean()
    #     ))

    # Plot degree distribution using matplotlib
    print('Plotting degree distribution...')
    plt.hist(degrees, bins=30, log=True)
    plt.title('Degree distribution of {}'.format(f.name))
    plot_filename = '{}.degree_hist.png'.format(f.name)
    plt.savefig(plot_filename)
    print('Degree distribution of {} is in: {}'.format(f.name, plot_filename))

    # Approximate diameter
    print('Approximating diameter...')
    diameters = np.array([approximate_diameter(g, n) for n in SAMPLING])
    for n, diam in zip(SAMPLING, diameters):
        print('Approximate diameter in {} with sampling {} nodes: {:.2f}'.format(f.name, n, diam))
    print('Approximate diameter in {} (mean and variance): {:.2f}, {:.2f}'.format(
        f.name, np.array(diameters).mean(), np.array(diameters).var()))

    # Effective diameter
    # print('Calculating effective diameter...')
    # print('    Calculating all shortest path lengths...')
    # all_path_lengths = nx.floyd_warshall_numpy(g, weight=None)
    # unique, counts = np.unique(all_path_lengths, return_counts=True)
    # for i in range(1, len(counts)):
    #     counts[i] += counts[i-1] # cumulative

    # print('    Fitting hop-plot to calculate hop-plot exponent...')
    # _, hop_plot_exp = fit_powerlaw(unique, counts)
    # effective_diameter = (len(g.nodes)**2 / (len(g.nodes) + 2*len(g.edges))) ** (1/hop_plot_exp)
    # print('Effective diameter in {}: {:.2f}'.print(f.name, effective_diameter))

    # # Plot shortest path length distribution
    # print('Plotting shortest path length distribution...')
    # plt.hist(all_path_lengths[~np.eye(all_path_lengths.shape[0])], bins=30, log=True)
    # plt.title('Shortest path length distribution of {}'.format(f.name))
    # plot_filename = '{}.shortest_path_length_dist.png'.format(f.name)
    # plt.savefig(plot_filename)
    # print('Shortest path length distribution of {} is in: {}'.format(f.name, plot_filename))

    # Larget connected component
    components = [len(c) for c in nx.connected_components(g)]
    print('Fraction of nodes in larget connected component in {}: {:.2f}'.format(
          f.name, components[0]/len(g.nodes)))

    complement = nx.complement(g)
    complement_components = [len(c) for c in nx.connected_components(complement)]
    print('Fraction of nodes in largest connected component in complement of {}: {:.2f}'.format(
        f.name, components[0]/len(complement.nodes)))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('file', type=argparse.FileType(mode='rb'),
                        help='Graph to compute metrics for')

    args = parser.parse_args()
    main(args.file)
