"""What does the stored tensor hold beyond the hull's tensor about its center of mass?

The exact central tensor is taken from the stored one, and what is left is divided by the mass. Three things show in it:

  - It is close to the same on each axis, and has little off the diagonal. Had the tensor been taken about some point other than the
    center of mass, it would single out the direction to that point.
  - On a large model it is |c|^2, the square of the distance from the origin to the center of mass.
  - On a small model it falls short of |c|^2 by 1, on every axis. The 1 is in square metres, and is lost in the noise of a large model.

So the term is m(|c|^2 - 1) times the identity. The last columns print what is left once |c|^2 is taken off, which should be -1.

    python solve_extra_term.py D:/tmp/fs1_pof
"""
import sys

import numpy as np

from models import inertia, load

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    np.set_printoptions(precision=3, suppress=True, linewidth=200)
    rows = []
    for model in load(sys.argv[1]):
        if not model['closed']:
            continue
        mass, volume, centroid = model['pof']['mass'], model['volume'], model['centroid']
        central = inertia(model['second'] - volume * np.outer(centroid, centroid)) * (mass / volume)
        extra = (np.linalg.inv(model['stored']) - central) / mass
        gyration = np.trace(central) / (3 * mass)
        rows.append((model['pof']['radius'], model['name'], centroid.dot(centroid), np.diag(extra) - centroid.dot(centroid), gyration,
                     np.abs(extra - np.diag(np.diag(extra))).max()))
    print(f"{'model':<18} {'radius':>8} {'|c|^2':>9} {'gyration^2':>11} {'off the diagonal':>17}   what is left on each axis, less |c|^2")
    for radius, name, offset, left, gyration, off in sorted(rows):
        print(f'{name:<18} {radius:8.2f} {offset:9.3f} {gyration:11.2f} {off:17.3f}   {left}')
    small = np.array([left for radius, _, _, left, gyration, _ in rows if gyration < 60])
    print(f'\nover the {len(small)} models with a gyration^2 under 60, where a 1 can be seen: mean {small.mean():.3f}, median {np.median(small):.3f}')
