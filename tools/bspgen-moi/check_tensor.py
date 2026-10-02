"""Which formula gives the stored moment of inertia? Scores the candidates against every model.

The tensor of each candidate is worked out exactly from the hull's source mesh, with the mass the POF stores, then inverted as the POF has
it. Its error is the largest difference from the stored tensor in any of the nine entries, over the largest entry of the stored diagonal.

    python check_tensor.py D:/tmp/fs1_pof
"""
import collections
import sys

import numpy as np

from models import inertia, load

ORIGIN = 'about the origin'
CENTRAL = 'about the center of mass'
DIAGONAL = 'central, with m|c|^2 on the diagonal'
LESS_ONE = 'central, with m(|c|^2 - 1) on the diagonal'
NAMES = [ORIGIN, CENTRAL, DIAGONAL, LESS_ONE]


def candidates(model):
    mass, volume, centroid = model['pof']['mass'], model['volume'], model['centroid']
    density = mass / volume
    central = inertia(model['second'] - volume * np.outer(centroid, centroid)) * density
    return {
        ORIGIN: inertia(model['second']) * density,
        CENTRAL: central,
        # the parallel axis theorem less its second term, m * c * cT
        DIAGONAL: central + mass * centroid.dot(centroid) * np.eye(3),
        # the same, less the mass times the identity; the 1 is in square metres
        LESS_ONE: central + mass * (centroid.dot(centroid) - 1.0) * np.eye(3),
    }


def error(tensor, stored):
    return np.abs(np.linalg.inv(tensor) - stored).max() / np.diag(stored).max()


def summary(label, errors):
    errors = sorted(errors)
    return (f'  {label:<44} {len(errors):>3} models   median {errors[len(errors) // 2] * 100:6.2f}%   within 1%: {sum(e <= .01 for e in errors):<3}'
            f' within 3%: {sum(e <= .03 for e in errors):<3} within 10%: {sum(e <= .10 for e in errors)}')


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    table = collections.defaultdict(list)
    print(f"{'model':<18} {'version':>7} {'hull':<7}" + ''.join(f'{name:>44}' for name in NAMES))
    for model in load(sys.argv[1]):
        errors = {name: error(tensor, model['stored']) for name, tensor in candidates(model).items()}
        for name in NAMES:
            table[name, model['closed']].append(errors[name])
        print(f"{model['name']:<18} {model['pof']['version']:>7} {'closed' if model['closed'] else 'open':<7}"
              + ''.join(f'{errors[name] * 100:43.2f}%' for name in NAMES))
    for closed in (True, False):
        print(f"\n{'closed' if closed else 'open'} hulls")
        for name in NAMES:
            print(summary(name, table[name, closed]))
