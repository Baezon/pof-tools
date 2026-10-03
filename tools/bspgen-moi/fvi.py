"""BSPGEN's test of a ray against a polygon, as the cross sections in the header show it to have been.

It is `fvi_ray_plane` followed by `fvi_point_face`, both of `code/Math/Fvi.cpp` in the FreeSpace 2 source, with one difference: where
the source asks whether the first edge of a triangle runs within 0.0001 of straight along an axis, BSPGEN asked whether it runs exactly
straight, as the routine in Graphics Gems that both come from does. Five things about it matter to a count of crossings:
  - a polygon is tested as a fan of triangles from its first vert, the test stopping at the first triangle that holds the point
  - the edges of each triangle count as inside it, so a ray through an edge that two polygons share crosses both
  - the polygons are those BSPGEN had made by merging the triangles of the source, with the normals it stored for them
  - the coordinates are in metres, with the axes of the POF and the origin of the scene
  - where the first edge of a triangle is one step of a float off straight, the test divides a difference of two nearly equal numbers
    by that step, and the rounding of what went before decides the answer. A ray can then be taken to hit a triangle that it passes
    metres outside of, or to miss one that it passes through

The arithmetic is that of a compiler of 1997 on the x87: sums and products are carried at the width of a double, and are cut to single
precision on being stored in a variable. check_cross_sections.py bears all of this out.
"""
import numpy as np

# inches to metres, as a float has it
SCALE = float(np.float32(0.0254))
IJ = ((2, 1), (0, 2), (1, 0))


def stored(value):
    """As it is after being stored in a float."""
    return np.asarray(value, dtype=np.float64).astype(np.float32).astype(np.float64)


def grid(lo, hi, count):
    """The middles of `count` cells from lo to hi, by the sum that gives the depths of the cross sections to the bit."""
    step = stored((hi - lo) / count)
    return stored(lo + step * (np.arange(count) + 0.5))


def point_face(points, corners, normal):
    """Which of the points, all taken to lie in the polygon's plane, are on the polygon."""
    t = np.abs(normal)
    if t[0] > t[1]:
        i0 = 0 if t[0] > t[2] else 2
    else:
        i0 = 1 if t[1] > t[2] else 2
    i1, i2 = IJ[i0] if normal[i0] > 0 else IJ[i0][::-1]

    u0 = stored(points[:, i1] - corners[0][i1])
    v0 = stored(points[:, i2] - corners[0][i2])
    inter = np.zeros(len(points), dtype=bool)
    done = np.zeros(len(points), dtype=bool)
    with np.errstate(all='ignore'):
        for i in range(2, len(corners)):
            u1, u2 = stored(corners[i - 1][i1] - corners[0][i1]), stored(corners[i][i1] - corners[0][i1])
            v1, v2 = stored(corners[i - 1][i2] - corners[0][i2]), stored(corners[i][i2] - corners[0][i2])
            if u1 == 0:
                beta = stored(u0 / u2)
                alpha = stored((v0 - beta * v2) / v1)
            else:
                beta = stored((v0 * u1 - u0 * v1) / (v2 * u1 - u2 * v1))
                alpha = stored((u0 - beta * u2) / u1)
            in_range = (beta >= 0) & (beta <= 1)
            hit = in_range & (alpha >= 0) & (alpha + beta <= 1)
            # the C stops at the first triangle to hold the point, and leaves `inter` as the last triangle tried had it
            inter = np.where(~done & in_range, hit, inter)
            done |= inter
    return inter


def crossings(verts, polygons, origins, direction):
    """Every crossing of the rays with the polygons: (ray, the point crossed at, whether the polygon faces the ray's origin).

    A polygon is a dict of its `verts`, as indices, and its `normal`. Its plane is taken through the first of its verts.
    """
    verts, origins, direction = stored(verts), stored(origins), stored(direction)
    rays, points, facing = [], [], []
    for poly in polygons:
        normal = stored(poly['normal'])
        den = stored(-(normal @ direction))
        if den == 0:
            continue
        corners = verts[poly['verts']]
        dist = stored(stored(stored(origins - corners[0]) @ normal) / den)
        crossed = stored(origins + direction * dist[:, None])
        index = np.nonzero(point_face(crossed, corners, normal))[0]
        rays.append(index)
        points.append(crossed[index])
        facing.append(np.full(len(index), den > 0))
    if not rays:
        return np.zeros(0, dtype=int), np.zeros((0, 3)), np.zeros(0, dtype=bool)
    return np.concatenate(rays), np.concatenate(points), np.concatenate(facing)


def normal_of(corners):
    """The unit normal of a polygon from its first three verts, stored as a float at each step."""
    normal = stored(np.cross(stored(corners[1] - corners[0]), stored(corners[2] - corners[0])))
    return stored(normal / np.sqrt(stored(normal @ normal)))
