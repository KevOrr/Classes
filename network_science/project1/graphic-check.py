#!/usr/bin/env python3

import argparse

import numpy as np



def is_graphic(degs):
    if len(np.transpose(np.nonzero(degs < 0))) > 0:
        return False, 0

    for i in range(degs.shape[0]):
        if degs[i] > degs.shape[0] - i:
            return False, i

        degs[i+1:degs[i]+i+1] -= 1
        degs[i] = 0
        if len(np.transpose(np.nonzero(degs < 0))) > 0:
            return False, i+1

    return True, i+1


def main(f):
    degs = np.loadtxt(f, dtype=np.int)

    # Sort in descending order
    degs[::-1].sort()

    graphic, num_iterations = is_graphic(degs)
    if graphic:
        print('{} is a graphic degree sequence'.format(f.name))
    else:
        print('{} is NOT a graphic degree sequence. '
              'It fails after {} iterations of Havel-Hakimi'.format(f.name, num_iterations))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument('file', type=argparse.FileType(mode='rb'),
                        help='Degree sequence file')

    args = parser.parse_args()
    main(args.file)
