"""Where is BSPGEN's error? What it stored, less what is exact, is read as a lump of material: how much of it, and where it lies.

The volume, center of mass and tensor in a POF give the volume, first moment and second moment that BSPGEN arrived at, the tensor being
undone by the formula of check_tensor.py. Taking the exact ones of the hull from them leaves those of whatever BSPGEN counted that isn't
there, less whatever it missed that is. All of it is printed in cells, a cell being a cube a fifteenth of the hull's longest side across.

With one folder, each closed hull is set against the exact answer. With two, the FreeSpace 2 run is set against the FreeSpace 1 run on
the same hull, which leaves what changed between the two builds of BSPGEN.

    python check_lumps.py D:/tmp/fs1_pof
    python check_lumps.py D:/tmp/fs1_pof D:/tmp/fs2_pof
"""
import sys

import numpy as np

from models import load
from pof_header import stored_volume

CELLS = 15


def cell_of(model):
    return (model['verts'].max(axis=0) - model['verts'].min(axis=0)).max() / CELLS


def stored_moments(model):
    """Volume, first moment and second moment about the origin, as the header of the POF has them."""
    volume, mass = stored_volume(model['pof']), model['pof']['mass']
    center = np.array(model['pof']['com'], dtype=np.float64)
    tensor = np.linalg.inv(np.array(model['stored'], dtype=np.float64))
    central = (tensor - mass * (center @ center - 1.0) * np.eye(3)) * volume / mass
    second = np.trace(central) / 2 * np.eye(3) - central + volume * np.outer(center, center)
    return volume, center * volume, second


def exact_moments(model):
    return model['volume'], model['centroid'] * model['volume'], model['second']


def lump(moments, against, cell):
    """The one set of moments less the other, in cells."""
    return tuple((mine - theirs) / cell ** power for mine, theirs, power in zip(moments, against, (3, 4, 5)))


if __name__ == '__main__':
    if len(sys.argv) not in (2, 3):
        sys.exit(__doc__)
    np.set_printoptions(precision=3, suppress=True, linewidth=250)
    first = {model['name']: model for model in load(sys.argv[1])}
    rows = []
    if len(sys.argv) == 2:
        for model in first.values():
            if model['closed']:
                rows.append((model, lump(stored_moments(model), exact_moments(model), cell_of(model))))
    else:
        for model in load(sys.argv[1], pofs_from=sys.argv[2]):
            old = first.get(model['name'])
            if old is None or np.allclose(old['pof']['com'], model['pof']['com'], rtol=0, atol=1e-6):
                continue
            rows.append((old, lump(stored_moments(model), stored_moments(old), cell_of(old))))

    print(f"{'model':<16} {'cells':>8} {'lump':>8}   its first moment, then the diagonal of its second, then xy, yz and zx of its second")
    for model, (volume, moment, second) in rows:
        print(f"{model['name']:<16} {stored_volume(model['pof']) / cell_of(model) ** 3:8.2f} {volume:8.3f}   {moment} {np.diag(second)}"
              f' {np.array([second[0, 1], second[1, 2], second[2, 0]])}')
