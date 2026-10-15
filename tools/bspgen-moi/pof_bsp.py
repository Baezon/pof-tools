"""Reader for the BSP data of a POF submodel: its polygons as BSPGEN left them, and the tree they hang in."""
import struct

import numpy as np

from pof_header import read_string

EOF, DEFPOINTS, FLATPOLY, TMAPPOLY, SORTNORM, BOUNDBOX = range(6)


def bsp_offsets(path):
    """Where the BSP data of each submodel starts in the file, by submodel number, along with the bytes of the file."""
    data = open(path, 'rb').read()
    offsets = {}
    pos = 8
    while pos + 8 <= len(data):
        tag = data[pos:pos + 4]
        length = struct.unpack_from('<i', data, pos + 4)[0]
        if tag in (b'SOBJ', b'OBJ2'):
            p = pos + 8
            number = struct.unpack_from('<i', data, p)[0]
            _, p = read_string(data, p + 24 + 36)
            _, p = read_string(data, p)
            offsets[number] = p + 16
        pos += 8 + length
    return data, offsets


def read_tree(data, start):
    """The tree as nested dicts. A node holds the polygons met at its own level, and any sortnorms, each with its five lists."""
    node = {'polygons': [], 'sortnorms': []}
    p = start
    while True:
        tag, size = struct.unpack_from('<2i', data, p)
        if tag == EOF:
            return node
        if tag in (FLATPOLY, TMAPPOLY):
            count = struct.unpack_from('<i', data, p + 36)[0]
            stride = 4 if tag == FLATPOLY else 12
            verts = [struct.unpack_from('<H', data, p + 44 + i * stride)[0] for i in range(count)]
            node['polygons'].append({'normal': np.array(struct.unpack_from('<3f', data, p + 8), dtype=np.float64),
                                     'center': np.array(struct.unpack_from('<3f', data, p + 20), dtype=np.float64),
                                     'radius': struct.unpack_from('<f', data, p + 32)[0], 'verts': verts})
        elif tag == SORTNORM:
            lists = struct.unpack_from('<5i', data, p + 36)
            sortnorm = {'normal': np.array(struct.unpack_from('<3f', data, p + 8), dtype=np.float64),
                        'point': np.array(struct.unpack_from('<3f', data, p + 20), dtype=np.float64)}
            for name, offset in zip(('front', 'back', 'pre', 'post', 'on'), lists):
                sortnorm[name] = read_tree(data, p + offset) if offset else None
            node['sortnorms'].append(sortnorm)
        elif tag not in (DEFPOINTS, BOUNDBOX):
            raise ValueError(f'unknown BSP chunk {tag} at {p}')
        if size <= 0:
            raise ValueError(f'BSP chunk of size {size} at {p}')
        p += size


def polygons_of(tree):
    out = list(tree['polygons'])
    for sortnorm in tree['sortnorms']:
        for name in ('pre', 'back', 'on', 'front', 'post'):
            if sortnorm[name] is not None:
                out.extend(polygons_of(sortnorm[name]))
    return out


def fans(polygons):
    """The polygons as triangles, each fanned from its first vert."""
    return np.array([[poly['verts'][0], poly['verts'][i], poly['verts'][i + 1]] for poly in polygons for i in range(1, len(poly['verts']) - 1)], dtype=int)
