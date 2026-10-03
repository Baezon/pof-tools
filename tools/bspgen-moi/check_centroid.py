"""Is the stored center of mass the centroid of the hull? And which way round are the axes?

The size of the hull's bounding box settles the order of the axes and the scale, but not their signs. So each model is tried under every
choice of signs, and the one that puts the hull's centroid nearest the stored center of mass is counted. A model that is the same on both
sides can't tell the signs of x apart, which is why the counts come out split between two choices.

    python check_centroid.py D:/tmp/fs1_pof
"""
import collections
import itertools
import sys

import numpy as np

from models import hull_in_pof_coords, integrals, pairs
from p3d import read_p3d
from pof_header import read_pof

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    np.set_printoptions(precision=4, suppress=True, linewidth=200)
    chosen = collections.Counter()
    for name, (pof_path, p3d_path) in pairs(sys.argv[1]).items():
        pof, p3d = read_pof(pof_path), read_p3d(p3d_path)
        if 'mass' not in pof or not pof['detail']:
            continue
        best = None
        for signs in itertools.product([1.0, -1.0], repeat=3):
            placed = hull_in_pof_coords(pof, p3d, np.array(signs))
            if placed is None:
                continue
            volume, first, _ = integrals(placed[0], placed[1])
            centroid = first / volume
            distance = np.linalg.norm(centroid - np.array(pof['com'])) / pof['radius']
            if best is None or distance < best[0]:
                best = (distance, signs, centroid)
        if best is None:
            print(f'{name:<18} has no hull in its source of the size the POF gives')
            continue
        chosen[tuple(int(sign) for sign in best[1])] += 1
        print(f"{name:<18} signs {tuple(int(sign) for sign in best[1])}  out by {best[0] * 100:6.3f}% of the radius   centroid {best[2]}  stored {np.array(pof['com'])}")
    print()
    for signs, count in chosen.most_common():
        print(f'{signs}: {count}')
