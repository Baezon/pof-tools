"""Is BSPGEN's error chance, or is it the same each time?

Many FreeSpace 2 models were worked out afresh from a hull that hadn't changed since FreeSpace 1. That is two runs on the one mesh. This
sets the error of each against the exact answer, and against the other's.

If the error were chance, as from points thrown at random, the two runs would be no nearer each other than either is to the truth. If it
comes of a grid that falls the same way each time, they would agree with each other far better than with the truth.

    python check_runs.py D:/tmp/fs1_pof D:/tmp/fs2_pof
"""
import sys

import numpy as np

from models import load
from pof_header import stored_volume

if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    first = {model['name']: model for model in load(sys.argv[1]) if model['closed']}
    rows = []
    for model in load(sys.argv[1], pofs_from=sys.argv[2]):
        old = first.get(model['name'])
        if old is None or np.allclose(old['pof']['com'], model['pof']['com'], rtol=0, atol=1e-6):
            # not in both, or not worked out afresh
            continue
        extent = old['verts'].max(axis=0) - old['verts'].min(axis=0)
        # the hull sits where it did in all but a few, and the few are moved back by the centroid being taken from each its own
        rows.append((model['name'],
                     stored_volume(old['pof']) / old['volume'] - 1, stored_volume(model['pof']) / model['volume'] - 1,
                     (np.array(old['pof']['com']) - old['centroid']) / extent, (np.array(model['pof']['com']) - model['centroid']) / extent))

    print(f"{'model':<18} {'volume, in 1':>13} {'in 2':>9} {'2 less 1':>10}   center of mass over the extent, in 1 and in 2")
    np.set_printoptions(precision=4, suppress=True, linewidth=200)
    for name, v1, v2, c1, c2 in rows:
        print(f'{name:<18} {v1 * 100:12.3f}% {v2 * 100:8.3f}% {(v2 - v1) * 100:9.3f}%   {c1} {c2}')

    v1, v2 = np.array([row[1] for row in rows]), np.array([row[2] for row in rows])
    c1, c2 = np.array([row[3] for row in rows]).ravel(), np.array([row[4] for row in rows]).ravel()
    rms = lambda values: np.sqrt(np.mean(np.square(values)))
    print(f'\n{len(rows)} models')
    print(f'volume           error in 1, rms {rms(v1) * 100:.3f}%   in 2 {rms(v2) * 100:.3f}%   between them {rms(v2 - v1) * 100:.3f}%'
          f'   correlation {np.corrcoef(v1, v2)[0, 1]:.3f}')
    print(f'center of mass   error in 1, rms {rms(c1) * 100:.3f}%   in 2 {rms(c2) * 100:.3f}%   between them {rms(c2 - c1) * 100:.3f}%'
          f'   correlation {np.corrcoef(c1, c2)[0, 1]:.3f}')
