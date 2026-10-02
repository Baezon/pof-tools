"""BSPGEN's way of weighing a hull, as far as it is known.

The hull's bounding box is cut into cells that are nearly cubes, of a size that puts some 5000 of them across the box as seen along z,
and a sample is taken at the middle of each. A ray is cast along z through each column of samples, by the test of fvi.py, and a sample is
inside the hull if an odd count of the ray's crossings lie below it. Each sample inside adds the volume of a cell to the volume, and its
place to the sums that the center of mass and the tensor are made from. Every sum is kept in a float.

check_lattice.py bears this out, and shows where it still falls short.
"""
import numpy as np

import fvi
from pof_header import AREA_MASS_VERSION
from sampling import ray_hits

# how many cells BSPGEN aimed to have across the box as seen along z
ACROSS = 5000.0


def lattice(verts):
    """(how many cells along each axis, where their middles are, how long they are) for the box of the verts."""
    lo, hi = verts.min(axis=0), verts.max(axis=0)
    side = np.sqrt((hi[0] - lo[0]) * (hi[1] - lo[1]) / ACROSS)
    counts = np.ceil((hi - lo) / side).astype(int)
    return counts, [fvi.grid(lo[axis], hi[axis], counts[axis]) for axis in range(3)], fvi.stored((hi - lo) / counts)


def odd_below(column, height, grids):
    """Which samples have an odd count of crossings at or below them, as [x, y, z]. A crossing is of a column, at a height."""
    flips = np.zeros((len(grids[0]) * len(grids[1]), len(grids[2]) + 1), dtype=int)
    np.add.at(flips, (column, np.searchsorted(grids[2], height)), 1)
    return (np.cumsum(flips, axis=1)[:, :-1] % 2 == 1).reshape(len(grids[0]), len(grids[1]), len(grids[2]))


def inside_exactly(verts, triangles, grids):
    i, j, height, _ = ray_hits(verts, triangles, 2, grids[0], grids[1])
    return odd_below(i * len(grids[1]) + j, height, grids)


def inside_by_bspgen(verts, polygons, grids):
    """By rays cast up z from the bottom of the box. Rays cast down z from each sample come to the same."""
    x, y = np.meshgrid(grids[0], grids[1], indexing='ij')
    origins = np.stack([x.ravel(), y.ravel(), np.full(x.size, verts[:, 2].min())], axis=1)
    column, crossed, _ = fvi.crossings(verts, polygons, origins, (0, 0, 1))
    return odd_below(column, crossed[:, 2], grids)


def volume_after(cell, samples):
    """The volume after each of so many samples, the volume of a cell being added to a float each time."""
    return np.cumsum(np.full(samples, cell, dtype=np.float32), dtype=np.float32)


def mass_of(volume, version):
    """The mass of the header. From version 2009 on its two constants are floats, and the power is worked at double width."""
    if version < AREA_MASS_VERSION:
        return np.float32(volume)
    return (float(np.float32(4.65)) * np.asarray(volume, dtype=np.float64) ** float(np.float32(0.6667))).astype(np.float32)


def samples_needed(pof, cell, near, window=20000):
    """How many samples give the stored mass to the bit, nearest to `near`. None if no count within the window does."""
    masses = mass_of(volume_after(cell, near + window), pof['version'])
    found = np.nonzero(masses == np.float32(pof['mass']))[0] + 1
    return int(found[np.abs(found - near).argmin()]) if len(found) else None


def sums(inside, places):
    """The sums over the samples inside: of x, y and z, of what goes on the diagonal of the tensor, and of xy, yz and zx.

    Each is kept in a float and added to at double width. The samples are taken with x outermost and y innermost.
    """
    i, k, j = np.nonzero(inside.transpose(0, 2, 1))
    x, y, z = places[0][i], places[1][j], places[2][k]
    terms = np.stack([x, y, z, y * y + z * z, x * x + z * z, x * x + y * y, x * y, y * z, z * x], axis=1)
    total = np.zeros(terms.shape[1], dtype=np.float32)
    for term in terms:
        total = (total + term).astype(np.float32)
    return total.astype(np.float64)


def header(inside, places, steps, version):
    """(mass, center of mass, inverse of the tensor) as BSPGEN would have written them for these samples."""
    cell = np.float32(np.float32(steps[0] * steps[1]) * steps[2])
    volume = float(volume_after(cell, int(inside.sum()))[-1])
    mass = float(mass_of(volume, version))
    total = sums(inside, places) * float(cell)
    center = total[:3] / volume
    about_origin = np.array([[total[3], -total[6], -total[8]], [-total[6], total[4], -total[7]], [-total[8], -total[7], total[5]]])
    # the parallel axis theorem with the identity where |c|^2 times the identity should be
    tensor = about_origin - volume * (np.eye(3) - np.outer(center, center))
    return mass, center, np.linalg.inv(tensor * mass / volume)
