"""What should pof-tools' Retail mass model give, and how near the header does that come?

The Retail model is BSPGEN's formulas with its sampling put right: exact integrals over the detail 0 submodel alone, as the POF has it.
A hull that isn't closed has its flat holes capped as pof-tools caps them. It is passed over if a hole isn't flat, or if it faces
inwards and so encloses no volume.

For each model this prints the volume, the mass by BSPGEN's formula, the center of mass, and the diagonal of the inverse tensor for the
mass in the header. Then how far each is from the header: the mass over the stored mass, the center of mass over the hull's longest
side, and the tensor by its largest difference in any entry over the largest entry of the stored diagonal.

    python check_retail_mode.py D:/tmp/fs2_pof
"""
import os
import sys

import numpy as np

import pof_bsp
from models import NOT_SHIPS
from pof_header import AREA_MASS_VERSION, pofs_in, read_pof

# pof-tools' FLAT_HOLE_TOLERANCE
FLAT = 0.01


def capped_triangles(verts, polygons):
    """The hull as triangles, with a fan over each hole from its middle. None if a hole isn't flat."""
    keys = [(vert.astype(np.float32) + np.float32(0)).tobytes() for vert in verts]
    place = dict(zip(keys, verts))
    count, triangles = {}, []
    for poly in polygons:
        corners = poly['verts']
        for start, end in zip(corners, corners[1:] + corners[:1]):
            count[keys[start], keys[end]] = count.get((keys[start], keys[end]), 0) + 1
            count[keys[end], keys[start]] = count.get((keys[end], keys[start]), 0) - 1
        triangles.extend([verts[corners[0]], verts[b], verts[c]] for b, c in zip(corners[1:], corners[2:]))

    # the edges with none running against them are the rims of the holes, a hole being the edges that are joined to each other
    hole_of = {}

    def find(key):
        hole_of.setdefault(key, key)
        while hole_of[key] != key:
            hole_of[key] = hole_of[hole_of[key]]
            key = hole_of[key]
        return key

    rims = [(edge, times) for edge, times in count.items() if times > 0]
    for (start, end), _ in rims:
        hole_of[find(start)] = find(end)
    holes = {}
    for (start, end), times in rims:
        holes.setdefault(find(start), []).append((place[start], place[end], times))
    for hole in holes.values():
        starts = np.array([start for start, _, _ in hole])
        middle = starts.mean(axis=0)
        spreads = np.linalg.eigvalsh((starts - middle).T @ (starts - middle))
        if spreads.min() > FLAT * FLAT * spreads.max():
            return None, True
        triangles.extend([middle, end, start] for start, end, times in hole for _ in range(times))
    return np.array(triangles), bool(holes)


def retail(triangles, mass):
    """(volume, center of mass, inertia tensor for the mass) by BSPGEN's formulas. None if the triangles enclose no volume."""
    a, b, c = triangles[:, 0], triangles[:, 1], triangles[:, 2]
    volumes = np.einsum('ij,ij->i', a, np.cross(b, c)) / 6.0
    volume = volumes.sum()
    if volume <= 0:
        return None
    total = a + b + c
    center = (volumes[:, None] * total).sum(axis=0) / 4.0 / volume
    weighted = lambda v: np.einsum('n,ni,nj->ij', volumes, v, v)
    second = (weighted(a) + weighted(b) + weighted(c) + weighted(total)) / 20.0
    about_origin = np.trace(second) * np.eye(3) - second
    # the parallel axis theorem with the identity where |c|^2 times the identity should be
    return volume, center, (about_origin - volume * (np.eye(3) - np.outer(center, center))) * mass / volume


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    rows, holed, inwards = [], [], []
    print(f"{'model':<16} {'hull':<7} {'volume':>12} {'mass':>12}   {'center of mass':<34} {'inverse tensor, diagonal':<40} off by: mass, center, tensor")
    for path in pofs_in(sys.argv[1]):
        name = os.path.splitext(os.path.basename(path))[0].lower()
        if any(word in name for word in NOT_SHIPS):
            continue
        pof = read_pof(path)
        if 'mass' not in pof or not pof['detail'] or pof['mass'] <= 0:
            continue
        number = pof['detail'][0]
        submodel = pof['submodels'][number]
        data, offsets = pof_bsp.bsp_offsets(path)
        polygons = pof_bsp.polygons_of(pof_bsp.read_tree(data, offsets[number]))
        if len(submodel['verts']) == 0 or not polygons:
            continue
        verts = np.array(submodel['verts'], dtype=np.float64) + np.array(submodel['offset'], dtype=np.float64)
        triangles, capped = capped_triangles(verts, polygons)
        if triangles is None:
            holed.append(name)
            continue
        found = retail(triangles, pof['mass'])
        if found is None:
            inwards.append(name)
            continue
        volume, center, tensor = found
        if (np.linalg.eigvalsh(tensor) <= 0).any():
            print(f"{name:<16} {'capped' if capped else 'closed':<7} {volume:12.6g}   too small: the square metre that the formula takes off leaves no tensor")
            rows.append(dict(name=name, small=True))
            continue
        mass = 4.65 * volume ** 0.6667 if pof['version'] >= AREA_MASS_VERSION else volume
        inverse, stored = np.linalg.inv(tensor), np.array(pof['moi'], dtype=np.float64)
        row = dict(name=name, small=False, mass=abs(mass / pof['mass'] - 1), center=np.abs(center - pof['com']).max() / np.ptp(verts, axis=0).max(),
                   tensor=np.abs(inverse - stored).max() / np.abs(np.diag(stored)).max())
        rows.append(row)
        print(f"{name:<16} {'capped' if capped else 'closed':<7} {volume:12.6g} {mass:12.6g}   {np.array2string(center, precision=4, suppress_small=True):<34}"
              f" {np.array2string(np.diag(inverse), precision=4):<40} {row['mass'] * 100:7.3f}% {row['center'] * 100:7.3f}% {row['tensor'] * 100:8.3f}%")

    fit = [row for row in rows if not row['small']]
    median = lambda values: sorted(values)[len(values) // 2]
    print(f"\n{len(rows) + len(holed) + len(inwards)} models: {len(fit)} weighed, {len(rows) - len(fit)} too small,"
          f" {len(holed)} with a hole that isn't flat, {len(inwards)} facing inwards")
    print(f"medians: mass {median([row['mass'] for row in fit]) * 100:.2f}%, center {median([row['center'] for row in fit]) * 100:.2f}%,"
          f" tensor {median([row['tensor'] for row in fit]) * 100:.2f}%")
    print('tensor within 1%, 3% and 10%: ' + ', '.join(str(sum(row['tensor'] <= within for row in fit)) for within in (0.01, 0.03, 0.1)))
    print('with a hole that isn\'t flat: ' + ', '.join(holed))
    print('facing inwards: ' + ', '.join(inwards))
