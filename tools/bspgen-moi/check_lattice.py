"""Does the lattice of lattice.py give what BSPGEN stored?

For each closed hull this lays the lattice, judges its samples three ways, and sets each against the header:
  - exactly, against the triangles of the source
  - by BSPGEN's ray test against the polygons of the POF, the verts being measured from the origin of the scene
  - the same, the verts being measured from the origin of the model, as the POF has them

The last two differ in the last bit of a vert here and there, which is enough to change which rays the test takes wrongly.

The volume is a float that had the volume of a cell added to it once for each sample inside. So the stored mass says how many samples
BSPGEN had inside, provided the cell is the right one: with a cell of another size, no count gives the stored mass to the bit. The first
columns are that count, and how many samples each way of judging is off it by.

The rest is how far the tensor made from each set of samples is from the stored one: the largest difference in any of the nine entries
over the largest entry of the stored diagonal, as in check_tensor.py, whose best formula with exact integrals is here for comparison.

It takes ten minutes.

    python check_lattice.py D:/tmp/fs1_pof
"""
import sys

import numpy as np

import fvi
from check_tensor import LESS_ONE, candidates, error
from lattice import header, inside_by_bspgen, inside_exactly, lattice, samples_needed
from models import as_bspgen_had_it, as_the_pof_has_it, load

WAYS = ('exactly', 'from the scene', 'from the model')


def judged(way, verts, polygons, triangles, pof):
    """(which samples are inside, where the samples are from the origin of the model, how long the cells are)"""
    if way == 'from the model':
        verts = as_the_pof_has_it(pof, verts)
    _, grids, steps = lattice(verts)
    inside = inside_exactly(verts, triangles, grids) if way == 'exactly' else inside_by_bspgen(verts, polygons, grids)
    to_model = verts.min(axis=0) - np.array(pof['submodels'][pof['detail'][0]]['verts']).min(axis=0)
    return inside, [fvi.stored(grids[axis] - to_model[axis]) for axis in range(3)], steps


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    rows = []
    print(f"{'model':<14} {'cells':<12} {'samples':>8}   off by: " + ''.join(f'{way:>16}' for way in WAYS)
          + f"   tensor off by: {'integrals':>10}" + ''.join(f'{way:>16}' for way in WAYS))
    for model in load(sys.argv[1]):
        hull = as_bspgen_had_it(model['pof'], model['pof_path'], model['p3d']) if model['closed'] else None
        if hull is None:
            continue
        pof = model['pof']
        row = dict(name=model['name'], integrals=error(candidates(model)[LESS_ONE], model['stored']))
        for way in WAYS:
            inside, places, steps = judged(way, *hull, pof)
            cell = np.float32(np.float32(steps[0] * steps[1]) * steps[2])
            need = samples_needed(pof, cell, int(inside.sum()))
            _, _, tensor = header(inside, places, steps, pof['version'])
            row[way] = dict(off=None if need is None else int(inside.sum()) - need,
                            tensor=np.abs(tensor - model['stored']).max() / np.diag(model['stored']).max())
            row['need'] = need
        rows.append(row)
        print(f"{row['name']:<14} {'x'.join(map(str, inside.shape)):<12} {row['need'] or 'no count':>8}           "
              + ''.join(f"{'' if row[way]['off'] is None else format(row[way]['off'], '+d'):>16}" for way in WAYS)
              + f"   {row['integrals'] * 100:24.3f}%" + ''.join(f"{row[way]['tensor'] * 100:15.3f}%" for way in WAYS), flush=True)

    found = [row for row in rows if all(row[way]['off'] is not None for way in WAYS)]
    print(f'\n{len(found)} of {len(rows)} closed hulls have a count of samples that gives the stored mass to the bit')
    median = lambda values: sorted(values)[len(values) // 2]
    print(f"\n{'':<24} {'samples off, median':>20} {'tensor off, median':>20}   within 0.01%, 0.1% and 1%, of {len(rows)}")
    print(f"{'integrals, by formula':<24} {'':>20} {median([row['integrals'] for row in rows]) * 100:19.3f}%   "
          + ' '.join(f"{sum(row['integrals'] <= within for row in rows):>4}" for within in (1e-4, 1e-3, 1e-2)))
    for way in WAYS:
        print(f"{way:<24} {median([abs(row[way]['off']) for row in found]):>20} {median([row[way]['tensor'] for row in rows]) * 100:19.3f}%   "
              + ' '.join(f"{sum(row[way]['tensor'] <= within for row in rows):>4}" for within in (1e-4, 1e-3, 1e-2)))
