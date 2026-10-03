"""Sampled estimates of a closed mesh's mass properties, to set against what BSPGEN stored.

The first two families are built on rays cast along one axis through a grid laid over the other two:
  - rays: the length of each ray that lies inside the mesh is taken exactly
  - lattice: points are set along each ray too, and those inside the mesh are counted
  - cells: a lattice again, of cubes about the origin with a finer lattice inside each
  - slabs: the mesh is cut across an axis, and each cross-section is taken exactly
  - radial: rays out of the origin, one to each of a grid of directions
"""
import numpy as np


def ray_hits(verts, faces, axis, us, vs):
    """Where the rays along `axis` through the grid us by vs cross the mesh.

    Returns (i, j, w, sign) arrays, a row to a crossing: the ray's place in the grid, where along the axis it crosses, and +1 where the
    ray leaves the mesh or -1 where it enters.
    """
    u_axis, v_axis = [(1, 2), (2, 0), (0, 1)][axis]
    out = []
    for face in faces:
        a, b, c = verts[face]
        normal = np.cross(b - a, c - a)
        if normal[axis] == 0.0:
            continue
        ulo, uhi = np.searchsorted(us, [min(a[u_axis], b[u_axis], c[u_axis]), max(a[u_axis], b[u_axis], c[u_axis])])
        vlo, vhi = np.searchsorted(vs, [min(a[v_axis], b[v_axis], c[v_axis]), max(a[v_axis], b[v_axis], c[v_axis])])
        if ulo == uhi or vlo == vhi:
            continue
        i, j = np.meshgrid(np.arange(ulo, uhi), np.arange(vlo, vhi), indexing='ij')
        u, v = us[i], vs[j]
        # twice the signed areas of the point with each edge, in the projection
        e0 = (b[u_axis] - a[u_axis]) * (v - a[v_axis]) - (b[v_axis] - a[v_axis]) * (u - a[u_axis])
        e1 = (c[u_axis] - b[u_axis]) * (v - b[v_axis]) - (c[v_axis] - b[v_axis]) * (u - b[u_axis])
        e2 = (a[u_axis] - c[u_axis]) * (v - c[v_axis]) - (a[v_axis] - c[v_axis]) * (u - c[u_axis])
        inside = ((e0 >= 0) & (e1 >= 0) & (e2 >= 0)) | ((e0 <= 0) & (e1 <= 0) & (e2 <= 0))
        if not inside.any():
            continue
        u, v, i, j = u[inside], v[inside], i[inside], j[inside]
        w = a[axis] - (normal[u_axis] * (u - a[u_axis]) + normal[v_axis] * (v - a[v_axis])) / normal[axis]
        out.append((i, j, w, np.full(len(w), np.sign(normal[axis]))))
    if not out:
        return tuple(np.zeros(0, dtype=kind) for kind in (int, int, float, float))
    return tuple(np.concatenate(column) for column in zip(*out))


def grid(lo, hi, count, placing):
    """`count` samples across lo to hi: at the middle of each of `count` cells, or from end to end with the ends among them."""
    if placing == 'middles':
        step = (hi - lo) / count
        return lo + (np.arange(count) + 0.5) * step, step
    step = (hi - lo) / (count - 1)
    return lo + np.arange(count) * step, step


def nudge(samples, step):
    """Moved off the verts and edges that a grid so often lands on, by too little to matter to anything else."""
    return samples + step * 1.0e-7


def ray_estimate(verts, faces, axis, counts, placing='middles', bounds=None):
    """Volume, centroid and second moment about the origin, from rays along `axis`, each standing for a column of the grid's cell."""
    u_axis, v_axis = [(1, 2), (2, 0), (0, 1)][axis]
    lo, hi = bounds if bounds is not None else (verts.min(axis=0), verts.max(axis=0))
    us, du = grid(lo[u_axis], hi[u_axis], counts[0], placing)
    vs, dv = grid(lo[v_axis], hi[v_axis], counts[1], placing)
    us, vs = nudge(us, du), nudge(vs, dv)
    i, j, w, sign = ray_hits(verts, faces, axis, us, vs)
    cell = du * dv
    u, v = us[i], vs[j]
    volume = cell * np.sum(sign * w)
    first = np.zeros(3)
    first[u_axis] = cell * np.sum(sign * w * u)
    first[v_axis] = cell * np.sum(sign * w * v)
    first[axis] = cell * np.sum(sign * w * w / 2)
    second = np.zeros((3, 3))
    second[u_axis, u_axis] = cell * np.sum(sign * w * u * u)
    second[v_axis, v_axis] = cell * np.sum(sign * w * v * v)
    second[axis, axis] = cell * np.sum(sign * w ** 3 / 3)
    second[u_axis, v_axis] = second[v_axis, u_axis] = cell * np.sum(sign * w * u * v)
    second[u_axis, axis] = second[axis, u_axis] = cell * np.sum(sign * u * w * w / 2)
    second[v_axis, axis] = second[axis, v_axis] = cell * np.sum(sign * v * w * w / 2)
    return volume, first / volume, second


def lattice_inside(verts, faces, axis, counts, placing='middles', bounds=None):
    """The lattice points inside the mesh, as (points, cell volume, count of all the points)."""
    u_axis, v_axis = [(1, 2), (2, 0), (0, 1)][axis]
    lo, hi = bounds if bounds is not None else (verts.min(axis=0), verts.max(axis=0))
    us, du = grid(lo[u_axis], hi[u_axis], counts[0], placing)
    vs, dv = grid(lo[v_axis], hi[v_axis], counts[1], placing)
    ws, dw = grid(lo[axis], hi[axis], counts[2], placing)
    us, vs, ws = nudge(us, du), nudge(vs, dv), nudge(ws, dw)
    i, j, w, sign = ray_hits(verts, faces, axis, us, vs)
    # how many of the ray's points lie below each crossing
    below = np.searchsorted(ws, w)
    winding = np.zeros((len(us), len(vs), len(ws) + 1))
    np.add.at(winding, (i, j, np.zeros(len(i), dtype=int)), sign)
    np.add.at(winding, (i, j, below), -sign)
    inside = np.cumsum(winding, axis=2)[:, :, :-1] > 0.5
    ii, jj, kk = np.nonzero(inside)
    points = np.zeros((len(ii), 3))
    points[:, u_axis], points[:, v_axis], points[:, axis] = us[ii], vs[jj], ws[kk]
    return points, du * dv * dw, len(us) * len(vs) * len(ws)


def lattice_estimate(verts, faces, axis, counts, placing='middles', bounds=None):
    points, cell, _ = lattice_inside(verts, faces, axis, counts, placing, bounds)
    volume = cell * len(points)
    return volume, points.mean(axis=0), cell * points.T @ points


def ray_rule_volumes(verts, faces, axis, count, bounds=None):
    """The volume by fine rays under each rule for what is inside, given a mesh that may overlap itself or hold parts facing inwards.

    'signed' adds up the winding number, as an integral over the polygons does. 'union' takes wherever it is above zero, 'parity' wherever
    a ray has crossed an odd count of polygons, and 'any' wherever it is anything but zero.
    """
    u_axis, v_axis = [(1, 2), (2, 0), (0, 1)][axis]
    lo, hi = bounds if bounds is not None else (verts.min(axis=0), verts.max(axis=0))
    us, du = grid(lo[u_axis], hi[u_axis], count, 'middles')
    vs, dv = grid(lo[v_axis], hi[v_axis], count, 'middles')
    us, vs = nudge(us, du), nudge(vs, dv)
    i, j, w, sign = ray_hits(verts, faces, axis, us, vs)
    order = np.lexsort((w, i * len(vs) + j))
    ray, w, sign = (i * len(vs) + j)[order], w[order], sign[order]
    # the stretch from each crossing to the next on the same ray, and what has been crossed by the time it starts
    same = ray[1:] == ray[:-1]
    length = np.where(same, w[1:] - w[:-1], 0.0)
    starts = np.concatenate(([True], ~same))
    winding = np.cumsum(-sign)
    winding = (winding - np.maximum.accumulate(np.where(starts, winding + sign, -np.inf)))[:-1]
    crossed = np.arange(len(ray)) - np.maximum.accumulate(np.where(starts, np.arange(len(ray)), 0)) + 1
    crossed = crossed[:-1]
    cell = du * dv
    return {
        'signed': cell * np.sum(length * winding),
        'union': cell * np.sum(length * (winding > 0.5)),
        'parity': cell * np.sum(length * (crossed % 2 == 1)),
        'any': cell * np.sum(length * (np.abs(winding) > 0.5)),
    }


def cell_lattice_estimate(verts, faces, cell, across, axis=2):
    """Volume and first moment from cubes of side `cell`, one of them centred on the origin, each sampled at `across` points a side."""
    lo, hi = verts.min(axis=0), verts.max(axis=0)
    first_cell, last_cell = np.floor(lo / cell).astype(int) - 1, np.ceil(hi / cell).astype(int) + 1
    within = (np.arange(across) + 0.5) / across - 0.5
    samples = [nudge(((np.arange(first_cell[a], last_cell[a] + 1))[:, None] + within[None, :]).ravel() * cell, cell) for a in range(3)]
    u_axis, v_axis = [(1, 2), (2, 0), (0, 1)][axis]
    us, vs, ws = samples[u_axis], samples[v_axis], samples[axis]
    i, j, w, sign = ray_hits(verts, faces, axis, us, vs)
    below = np.searchsorted(ws, w)
    winding = np.zeros((len(us), len(vs), len(ws) + 1))
    np.add.at(winding, (i, j, np.zeros(len(i), dtype=int)), sign)
    np.add.at(winding, (i, j, below), -sign)
    inside = np.cumsum(winding, axis=2)[:, :, :-1] > 0.5
    weight = (cell / across) ** 3
    first = np.zeros(3)
    first[u_axis] = (inside.sum(axis=(1, 2)) * us).sum()
    first[v_axis] = (inside.sum(axis=(0, 2)) * vs).sum()
    first[axis] = (inside.sum(axis=(0, 1)) * ws).sum()
    return weight * inside.sum(), weight * first


def sections(verts, faces, axis, heights):
    """The cross-section of the mesh at each height along `axis`, taken exactly: rows of (area, area * mean u, area * mean v)."""
    u_axis, v_axis = [(1, 2), (2, 0), (0, 1)][axis]
    corners = verts[faces]
    out = np.zeros((len(heights), 3))
    for row, height in enumerate(heights):
        above = corners[:, :, axis] > height
        cut = above.any(axis=1) & ~above.all(axis=1)
        triangles, above = corners[cut], above[cut]
        # the corner that is alone on its side of the plane, and where the two edges from it meet the plane
        alone_above = above.sum(axis=1) == 1
        lone = np.where(alone_above, above.argmax(axis=1), (~above).argmax(axis=1))
        index = np.arange(len(triangles))
        apex, left, right = triangles[index, lone], triangles[index, (lone + 1) % 3], triangles[index, (lone + 2) % 3]
        a = apex + (left - apex) * ((height - apex[:, axis]) / (left[:, axis] - apex[:, axis]))[:, None]
        b = apex + (right - apex) * ((height - apex[:, axis]) / (right[:, axis] - apex[:, axis]))[:, None]
        cross = (a[:, u_axis] * b[:, v_axis] - b[:, u_axis] * a[:, v_axis]) * np.where(alone_above, 1.0, -1.0)
        out[row] = cross.sum() / 2, ((a[:, u_axis] + b[:, u_axis]) * cross).sum() / 6, ((a[:, v_axis] + b[:, v_axis]) * cross).sum() / 6
    return out


def slab_estimate(verts, faces, axis, count, rule='middles'):
    """Volume and first moment from `count` slabs along `axis`, the cross-sections being exact and the rule saying which ones are taken."""
    u_axis, v_axis = [(1, 2), (2, 0), (0, 1)][axis]
    lo, hi = verts[:, axis].min(), verts[:, axis].max()
    step = (hi - lo) / count
    if rule == 'middles':
        heights, weights = lo + (np.arange(count) + 0.5) * step, np.full(count, step)
    else:
        heights, weights = lo + np.arange(count + 1) * step, np.full(count + 1, step)
        heights[0], heights[-1] = lo + step * 1e-9, hi - step * 1e-9
        if rule == 'trapezoid':
            weights[[0, -1]] /= 2
        elif rule == 'simpson':
            weights = weights / 3
            weights[1:-1:2] *= 4
            weights[2:-1:2] *= 2
    cut = sections(verts, faces, axis, heights)
    volume = (cut[:, 0] * weights).sum()
    first = np.zeros(3)
    first[u_axis], first[v_axis], first[axis] = (cut[:, 1] * weights).sum(), (cut[:, 2] * weights).sum(), (cut[:, 0] * weights * heights).sum()
    return abs(volume), first * np.sign(volume)


def radial_estimate(verts, faces, polar, across, around, axis=2, batch=2000):
    """Volume and first moment from rays out of the origin, one to each cell of a grid of `across` by `around` directions about `axis`.

    Each ray stands for a cone, and the length of it inside the mesh is taken exactly.
    """
    theta = (np.arange(across) + (0.5 if polar == 'middles' else 0.0)) * np.pi / across
    edges = np.arange(across + 1) * np.pi / across
    weights = np.cos(edges[:-1]) - np.cos(edges[1:]) if polar == 'middles' else np.sin(theta) * np.pi / across
    phi = (np.arange(around) + 0.5) * 2 * np.pi / around
    theta, phi = np.meshgrid(theta, phi, indexing='ij')
    weights = np.repeat(weights, around) * 2 * np.pi / around
    local = np.stack([np.sin(theta) * np.cos(phi), np.sin(theta) * np.sin(phi), np.cos(theta)], axis=-1).reshape(-1, 3)
    directions = np.zeros_like(local)
    directions[:, {2: [0, 1, 2], 1: [2, 0, 1], 0: [1, 2, 0]}[axis]] = local
    directions = directions + 1e-9

    a, e1, e2 = verts[faces[:, 0]], verts[faces[:, 1]] - verts[faces[:, 0]], verts[faces[:, 2]] - verts[faces[:, 0]]
    normals = np.cross(e1, e2)
    q = np.cross(-a, e1)
    volume, first = 0.0, np.zeros(3)
    for start in range(0, len(directions), batch):
        d = directions[start:start + batch]
        p = np.cross(d[:, None, :], e2[None, :, :])
        with np.errstate(all='ignore'):
            inverse = 1.0 / np.einsum('rfk,fk->rf', p, e1)
            u = np.einsum('rfk,fk->rf', p, -a) * inverse
            v = np.einsum('rk,fk->rf', d, q) * inverse
            t = np.einsum('fk,fk->f', e2, q)[None, :] * inverse
        ray, face = np.nonzero((u >= 0) & (v >= 0) & (u + v <= 1) & (t > 0))
        t = t[ray, face]
        leaving = np.sign(np.einsum('rk,rk->r', d[ray], normals[face]))
        cone = weights[start + ray] * leaving
        volume += (cone * t ** 3 / 3).sum()
        first += ((cone * t ** 4 / 4)[:, None] * d[ray]).sum(axis=0)
    return volume, first
