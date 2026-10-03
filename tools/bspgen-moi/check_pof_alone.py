"""How near does the lattice of lattice.py come to the header with nothing to go on but the POF?

This is what pof-tools would have to work from. The hull is the detail 0 submodel as the POF has it: its verts, its polygons as BSPGEN
merged them, and the normals stored for them. The samples are judged by BSPGEN's ray test, the volume and the sums are kept in floats,
and the header that comes of them is set against the stored one.

For the mass, the center of mass and the tensor, what is printed is the largest difference in any entry, over the stored mass, the
hull's longest side and the largest entry of the stored tensor.

    python check_pof_alone.py D:/tmp/fs2_pof
"""
import os
import sys

import numpy as np

import pof_bsp
from lattice import header, inside_by_bspgen, lattice, samples_needed
from models import NOT_SHIPS
from pof_header import pofs_in, read_pof


def open_edges(polygons):
    count = {}
    for poly in polygons:
        for start, end in zip(poly['verts'], poly['verts'][1:] + poly['verts'][:1]):
            count[start, end] = count.get((start, end), 0) + 1
            count[end, start] = count.get((end, start), 0) - 1
    return sum(n for n in count.values() if n > 0)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    rows = []
    print(f"{'model':<16} {'version':>7} {'open edges':>10} {'samples':>8} {'off by':>8} {'mass':>10} {'center':>10} {'tensor':>10}")
    for path in pofs_in(sys.argv[1]):
        name = os.path.splitext(os.path.basename(path))[0].lower()
        if any(word in name for word in NOT_SHIPS):
            continue
        pof = read_pof(path)
        if 'mass' not in pof or not pof['detail'] or pof['mass'] <= 0:
            continue
        number = pof['detail'][0]
        submodel = pof['submodels'][number]
        verts = np.array(submodel['verts'], dtype=np.float64)
        data, offsets = pof_bsp.bsp_offsets(path)
        polygons = pof_bsp.polygons_of(pof_bsp.read_tree(data, offsets[number]))
        if len(verts) == 0 or not polygons:
            continue
        _, grids, steps = lattice(verts)
        inside = inside_by_bspgen(verts, polygons, grids)
        if not inside.any():
            continue
        places = [grids[axis] + submodel['offset'][axis] for axis in range(3)]
        need = samples_needed(pof, np.float32(np.float32(steps[0] * steps[1]) * steps[2]), int(inside.sum()))
        mass, center, tensor = header(inside, places, steps, pof['version'])
        stored = np.array(pof['moi'])
        row = dict(name=name, open=open_edges(polygons), off=None if need is None else int(inside.sum()) - need, mass=abs(mass / pof['mass'] - 1),
                   center=np.abs(center - pof['com']).max() / (verts.max(axis=0) - verts.min(axis=0)).max(),
                   tensor=np.abs(tensor - stored).max() / np.abs(stored).max())
        rows.append(row)
        print(f"{name:<16} {pof['version']:>7} {row['open']:>10} {need or 'no count':>8} {'' if need is None else format(row['off'], '+d'):>8}"
              f" {row['mass'] * 100:9.4f}% {row['center'] * 100:9.4f}% {row['tensor'] * 100:9.3f}%", flush=True)

    median = lambda values: sorted(values)[len(values) // 2]
    for label, some in (('whole hulls', [row for row in rows if row['open'] == 0]), ('hulls with edges open', [row for row in rows if row['open']])):
        if not some:
            continue
        print(f"\n{label}: {len(some)}, of which {sum(row['off'] is not None for row in some)} have a count of samples that gives the stored mass to the bit")
        print(f"  medians: mass {median([row['mass'] for row in some]) * 100:.4f}%, center {median([row['center'] for row in some]) * 100:.4f}%,"
              f" tensor {median([row['tensor'] for row in some]) * 100:.3f}%")
        print('  tensor within 0.01%, 0.1%, 1% and 3%: ' + ', '.join(str(sum(row['tensor'] <= within for row in some)) for within in (1e-4, 1e-3, 1e-2, 3e-2)))
