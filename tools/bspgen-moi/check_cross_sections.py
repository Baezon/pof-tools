"""What are the cross sections in the header, and how did BSPGEN measure them?

From POF version 2014 on, the header follows its mass properties with a table of cross sections, each a depth along z and a radius.
BSPGEN's flag -c sets how many, and the models that have any have 25.

This rebuilds the table from the hull's source. The hull's bounding box is cut into as many slabs along z as there are cross sections,
and into as many strips along x. Through the middle of each strip, at the middle of each slab, a ray is cast along y. The radius of a
slab is how far the farthest point that its rays cross the hull at lies from the z axis of the scene, which is not that of the POF.

The rays are cast twice: by an exact test on the triangles of the source, and by BSPGEN's test as fvi.py has it on the polygons of the
POF. Where the two differ, BSPGEN agrees with the second. That is how its way of casting a ray is known.

With a second folder, the POFs are taken from it and the sources from the first.

    python check_cross_sections.py D:/tmp/fs1_pof
    python check_cross_sections.py D:/tmp/fs1_pof D:/tmp/fs2_pof
"""
import os
import sys

import numpy as np

import fvi
from models import as_bspgen_had_it, pairs
from p3d import read_p3d
from pof_header import pofs_in, read_pof
from sampling import ray_hits

WITHIN = (1e-3, 1e-5)


def exact_radii(verts, faces, xs, zs):
    """For each depth, how far from the z axis the rays along y reach, by a test that is exact. Nothing where no ray crosses."""
    depth, strip, y, _ = ray_hits(verts, faces, 1, zs, xs)
    reach = np.hypot(xs[strip], y)
    return np.array([reach[depth == row].max() if (depth == row).any() else 0.0 for row in range(len(zs))])


def bspgen_radii(verts, polygons, xs, zs):
    origins = np.array([(x, verts[:, 1].min(), z) for z in zs for x in xs])
    ray, crossed, _ = fvi.crossings(verts, polygons, origins, (0, 1, 0))
    reach = np.hypot(crossed[:, 0], crossed[:, 1])
    return np.array([reach[ray // len(xs) == row].max() if (ray // len(xs) == row).any() else 0.0 for row in range(len(zs))])


if __name__ == '__main__':
    if len(sys.argv) not in (2, 3):
        sys.exit(__doc__)
    pofs = {os.path.splitext(os.path.basename(path))[0].lower(): path for path in pofs_in(sys.argv[-1])}
    totals, seen = np.zeros((2, len(WITHIN)), dtype=int), set()
    entries = 0
    print(f"{'model':<14} {'entries':>7}   rebuilt to within a thousandth and a hundred thousandth: by the exact test, by BSPGEN's")
    for name, (_, source) in pairs(sys.argv[1]).items():
        if name not in pofs:
            continue
        pof = read_pof(pofs[name])
        table = np.array(pof.get('cross_sections', []))
        if len(table) == 0 or not pof['detail'] or table.tobytes() in seen:
            continue
        hull = as_bspgen_had_it(pof, pofs[name], read_p3d(source))
        if hull is None:
            continue
        seen.add(table.tobytes())
        verts, polygons, triangles = hull
        lo, hi = verts.min(axis=0), verts.max(axis=0)
        xs, zs = fvi.grid(lo[0], hi[0], len(table)), fvi.grid(lo[2], hi[2], len(table))
        radii = exact_radii(verts, triangles, xs, zs), bspgen_radii(verts, polygons, xs, zs)
        good = np.array([[(np.abs(found - table[:, 1]) <= within * table[:, 1]).sum() for within in WITHIN] for found in radii])
        if np.abs(zs - table[:, 0]).max() > 1e-3 * (hi[2] - lo[2]) or good.sum() == 0:
            print(f'{name:<14} {len(table):>7}   the source is not the scene this POF was made from: its hull lies elsewhere')
            continue
        totals += good
        entries += len(table)
        print(f"{name:<14} {len(table):>7}   {good[0, 0]:>3} {good[0, 1]:>3}   {good[1, 0]:>3} {good[1, 1]:>3}")
    print(f"{'all':<14} {entries:>7}   {totals[0, 0]:>3} {totals[0, 1]:>3}   {totals[1, 0]:>3} {totals[1, 1]:>3}")
