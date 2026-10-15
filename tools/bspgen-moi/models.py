"""What the scripts here share: pairing each POF with its source, and the mass properties of a triangle mesh."""
import glob
import os
import struct

import numpy as np

from p3d import read_p3d
from pof_header import pofs_in, read_pof

# inches to metres; BSPGEN's log prints it as "0.025"
SCALE = 0.0254

# POF coordinates are (-X, Z, -Y) of the scene. That is a reflection, so the winding is reversed along with it.
AXES = [0, 2, 1]
SIGNS = np.array([-1.0, 1.0, -1.0])

# weapons, whose stored tensors are 10 to 50 times what their geometry gives, and two effects that aren't solid at all
NOT_SHIPS = ('missile', 'stiletto', 'harbinger', 'shivbomb', 'phoenix', 'synaptic', 'tsunami', 'interceptor', 'disruptor', 'laser', 'mx-50', 'fury',
             'hornet', 'shivmissile', 'subspacenode', 'warphole')


def pairs(folder):
    """The POFs of the folder that have a P3D of the same name, as {name: (pof path, p3d path)}."""
    found = {}
    for path in glob.glob(os.path.join(folder, '*')):
        base, extension = os.path.splitext(os.path.basename(path))
        found.setdefault(base.lower(), {})[extension.lower()] = path
    return {name: (files['.pof'], files['.p3d']) for name, files in sorted(found.items()) if '.pof' in files and '.p3d' in files}


def tree_of(pof, root):
    """The submodel and everything below it."""
    tree = [root]
    for number in tree:
        tree.extend(child for child, sm in pof['submodels'].items() if sm['parent'] == number and child not in tree)
    return tree


def integrals(verts, faces):
    """Signed volume, first moment and second moment about the origin, each triangle standing for the tetrahedron between it and the origin."""
    a, b, c = verts[faces[:, 0]], verts[faces[:, 1]], verts[faces[:, 2]]
    volumes = np.einsum('ij,ij->i', a, np.cross(b, c)) / 6.0
    total = a + b + c
    first = (volumes[:, None] * total).sum(axis=0) / 4.0
    weighted = lambda v: np.einsum('n,ni,nj->ij', volumes, v, v)
    second = (weighted(a) + weighted(b) + weighted(c) + weighted(total)) / 20.0
    return volumes.sum(), first, second


def inertia(second_moment):
    return np.trace(second_moment) * np.eye(3) - second_moment


def open_edges(verts, faces):
    """How many edges have no edge running against them, verts being told apart by position."""
    keys = [(vert + 0.0).astype(np.float32).tobytes() for vert in verts]
    count = {}
    for face in faces:
        for corner in range(3):
            start, end = keys[face[corner]], keys[face[(corner + 1) % 3]]
            count[start, end] = count.get((start, end), 0) + 1
            count[end, start] = count.get((end, start), 0) - 1
    return sum(n for n in count.values() if n > 0)


def hull_in_pof_coords(pof, p3d, signs=SIGNS):
    """The detail0 submodel's source mesh as (verts, faces, origin), placed by matching its bounding box to the one in the POF.

    None if the source has no such object, or its size isn't the POF's.
    """
    objects = {name.lower(): o for name, o in p3d['objects'].items()}
    submodel = pof['submodels'][pof['detail'][0]]
    hull = objects.get(submodel['name'].lower())
    if hull is None or len(hull['faces']) == 0:
        return None
    verts = hull['verts'][:, AXES] * SCALE * signs
    bmin, bmax = np.array(submodel['bmin']), np.array(submodel['bmax'])
    if not np.allclose(verts.max(axis=0) - verts.min(axis=0), bmax - bmin, rtol=2e-3, atol=0.02):
        return None
    origin = verts.min(axis=0) - bmin
    faces = hull['faces'][:, ::-1] if np.prod(signs) > 0 else hull['faces']
    return verts - origin, faces, origin


def load(folder, ships_only=True, pofs_from=None):
    """Every model of the folder whose hull could be placed and has a volume, with its integrals worked out.

    With `pofs_from`, the sources of the folder are set against the POFs of the same name in that one. A POF whose hull is of another
    size than the source's is passed over, as one whose model was changed after the source was made.
    """
    models = []
    for name, (pof_path, p3d_path) in pairs(folder).items():
        if pofs_from is not None:
            matches = [path for path in pofs_in(pofs_from) if os.path.splitext(os.path.basename(path))[0].lower() == name]
            if not matches:
                continue
            pof_path = matches[0]
        if ships_only and any(word in name for word in NOT_SHIPS):
            continue
        try:
            pof, p3d = read_pof(pof_path), read_p3d(p3d_path)
        except (ValueError, struct.error):
            continue
        if 'mass' not in pof or not pof['detail']:
            continue
        placed = hull_in_pof_coords(pof, p3d)
        if placed is None:
            continue
        verts, faces, origin = placed
        volume, first, second = integrals(verts, faces)
        stored = np.array(pof['moi'])
        if volume <= 0 or not np.all(np.diag(stored) > 0):
            continue
        models.append(dict(name=name, pof=pof, pof_path=pof_path, p3d=p3d, verts=verts, faces=faces, origin=origin, volume=volume,
                           centroid=first / volume, second=second, stored=stored, closed=open_edges(verts, faces) == 0))
    return models


def as_bspgen_had_it(pof, pof_path, p3d):
    """The hull as BSPGEN cast its rays at it: (verts, polygons, triangles), or None if the source's hull isn't the POF's.

    The verts are those of the source in metres, each as the float BSPGEN had, with the axes of the POF and the origin of the scene. The
    polygons are those of the POF, which BSPGEN made by merging the triangles of the source: each a dict of its verts, as indices into
    those verts, and of the normal stored for it. The triangles are those of the source, wound as the polygons are.
    """
    import fvi
    import pof_bsp
    objects = {name.lower(): o for name, o in p3d['objects'].items()}
    number = pof['detail'][0]
    submodel = pof['submodels'][number]
    hull = objects.get(submodel['name'].lower())
    if hull is None or len(hull['faces']) == 0:
        return None
    verts = fvi.stored(hull['verts'].astype(np.float32)[:, AXES].astype(np.float64) * SIGNS * fvi.SCALE)
    theirs = np.array(submodel['verts'], dtype=np.float64)
    if len(theirs) == 0 or not np.allclose(verts.max(axis=0) - verts.min(axis=0), theirs.max(axis=0) - theirs.min(axis=0), rtol=2e-3, atol=0.02):
        return None
    moved = verts - (verts.min(axis=0) - theirs.min(axis=0))
    nearest = [np.abs(moved - vert).max(axis=1).argmin() for vert in theirs]
    if max(np.abs(moved[near] - vert).max() for near, vert in zip(nearest, theirs)) > 2e-3:
        return None
    data, offsets = pof_bsp.bsp_offsets(pof_path)
    polygons = [dict(verts=[int(nearest[v]) for v in poly['verts']], normal=poly['normal'])
                for poly in pof_bsp.polygons_of(pof_bsp.read_tree(data, offsets[number]))]
    return verts, polygons, hull['faces'][:, ::-1]


def as_the_pof_has_it(pof, verts):
    """The verts of as_bspgen_had_it as floats measured from the origin of the model, which is how the POF has them."""
    import fvi
    theirs = np.array(pof['submodels'][pof['detail'][0]]['verts'], dtype=np.float64)
    moved = fvi.stored(verts - (verts.min(axis=0) - theirs.min(axis=0)))
    for number, vert in enumerate(moved):
        nearest = theirs[np.abs(theirs - vert).max(axis=1).argmin()]
        if np.abs(nearest - vert).max() <= 2e-3:
            moved[number] = nearest
    return moved
