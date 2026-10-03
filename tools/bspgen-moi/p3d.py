"""Reader for the .p3d files that BSPGEN took as input.

A .p3d is a 3D Studio .3ds chunk file as exported by 3DS MAX, with these differences:
  - the root chunk is tagged 0xBEEF, where a .3ds has 0x4D4D
  - a face is 32 bytes: three u16 vertex indices, a u16 of flags, and six floats of texture coordinates, a pair to a corner
  - every object carries a 9 byte chunk tagged 0xDEAD

Run it on a file to print the chunk tree:  python p3d.py Fighter01.P3D
"""
import struct
import sys

import numpy as np

ROOT, OBJECT, TRIMESH, VERTS, FACES, MATRIX, DEAD = 0xBEEF, 0x4000, 0x4100, 0x4110, 0x4120, 0x4160, 0xDEAD
OBJ_NODE, NODE_ID, NODE_HDR, INSTANCE_NAME, PIVOT, POS_TRACK = 0xB002, 0xB030, 0xB010, 0xB011, 0xB013, 0xB020

FACE_SIZE = 32

# chunks that hold nothing but other chunks
CONTAINERS = {ROOT, 0x4D4D, 0x3D3D, 0xAFFF, 0xA010, 0xA020, 0xA030, 0xA040, 0xA041, 0xA050, 0xA052, 0xA053, 0xA084, 0xA087, 0xA100, 0xA200, 0xA204,
              0xA210, 0xA220, 0xA230, 0xA33A, TRIMESH, 0xB000, 0xB001, OBJ_NODE, 0xB003, 0xB004, 0xB005, 0xB006, 0xB007}

NAMES = {ROOT: 'ROOT', 0x0002: 'VERSION', 0x3D3D: 'EDIT', 0x3D3E: 'MESH_VERSION', 0x0100: 'MASTER_SCALE', 0xAFFF: 'MATERIAL', 0xA000: 'MAT_NAME',
         0xA200: 'TEXMAP', 0xA300: 'MAP_FILE', OBJECT: 'OBJECT', DEAD: 'DEAD', TRIMESH: 'TRIMESH', VERTS: 'VERTS', FACES: 'FACES', 0x4130: 'FACE_MAT',
         0x4140: 'UVS', 0x4150: 'SMOOTH', MATRIX: 'MATRIX', 0xB000: 'KEYFRAMER', 0xB00A: 'KF_HDR', 0xB008: 'KF_SEG', 0xB009: 'KF_CURTIME',
         OBJ_NODE: 'OBJ_NODE', NODE_ID: 'NODE_ID', NODE_HDR: 'NODE_HDR', INSTANCE_NAME: 'INSTANCE_NAME', PIVOT: 'PIVOT', 0xB014: 'BOUNDBOX',
         POS_TRACK: 'POS_TRACK', 0xB021: 'ROT_TRACK', 0xB022: 'SCL_TRACK'}


def walk(data, start=0, end=None, depth=0, out=None):
    """Every chunk as (depth, position, tag, length), in file order."""
    end = len(data) if end is None else end
    out = [] if out is None else out
    pos = start
    while pos + 6 <= end:
        tag, length = struct.unpack_from('<HI', data, pos)
        if length < 6 or pos + length > end:
            raise ValueError(f'chunk {tag:04X} at {pos} has the impossible length {length}')
        out.append((depth, pos, tag, length))
        body = pos + 6
        if tag == OBJECT:
            walk(data, data.index(b'\0', body) + 1, pos + length, depth + 1, out)
        elif tag == FACES:
            count = struct.unpack_from('<H', data, body)[0]
            walk(data, body + 2 + count * FACE_SIZE, pos + length, depth + 1, out)
        elif tag in CONTAINERS:
            walk(data, body, pos + length, depth + 1, out)
        pos += length
    return out


def read_string(data, pos):
    return data[pos:data.index(b'\0', pos)].decode('latin1')


def read_p3d(path):
    """The objects of the scene, by name, and the nodes of its hierarchy.

    An object has 'verts' (n by 3, in scene units) and 'faces' (n by 3 vertex indices).
    A node has 'name', 'parent' (a node id, or -1), 'pivot' and 'pos'.
    """
    data = open(path, 'rb').read()
    objects, nodes = {}, []
    current = node = None
    for depth, pos, tag, length in walk(data):
        body = pos + 6
        if tag == OBJECT:
            name = read_string(data, body)
            current = objects[name] = {'name': name, 'verts': np.zeros((0, 3)), 'faces': np.zeros((0, 3), dtype=int)}
        elif tag == VERTS:
            count = struct.unpack_from('<H', data, body)[0]
            current['verts'] = np.frombuffer(data, dtype='<f4', count=count * 3, offset=body + 2).reshape(count, 3).astype(np.float64)
        elif tag == FACES:
            count = struct.unpack_from('<H', data, body)[0]
            face = np.dtype([('verts', '<u2', 3), ('flags', '<u2'), ('uvs', '<f4', 6)])
            current['faces'] = np.frombuffer(data, dtype=face, count=count, offset=body + 2)['verts'].astype(int)
        elif tag == MATRIX:
            current['matrix'] = np.array(struct.unpack_from('<12f', data, body)).reshape(4, 3)
        elif 0xB001 <= tag <= 0xB007:
            # only the object nodes are kept, the rest being cameras and lights and the like
            node = {}
            if tag == OBJ_NODE:
                nodes.append(node)
        elif tag == NODE_ID:
            node['id'] = struct.unpack_from('<h', data, body)[0]
        elif tag == NODE_HDR:
            node['name'] = read_string(data, body)
            node['parent'] = struct.unpack_from('<h', data, body + len(node['name']) + 5)[0]
        elif tag == PIVOT:
            node['pivot'] = np.array(struct.unpack_from('<3f', data, body))
        elif tag == POS_TRACK:
            node['pos'] = np.array(struct.unpack_from('<3f', data, pos + length - 12))
    return {'objects': objects, 'nodes': nodes}


def describe(data, pos, tag, length):
    body = pos + 6
    if tag in (OBJECT, 0xA000, 0xA300, INSTANCE_NAME):
        return read_string(data, body)
    if tag == NODE_HDR:
        name = read_string(data, body)
        return f"{name}, parent {struct.unpack_from('<h', data, body + len(name) + 5)[0]}"
    if tag in (VERTS, FACES, 0x4140):
        return f"{struct.unpack_from('<H', data, body)[0]} of them"
    if tag == 0x4130:
        name = read_string(data, body)
        return f"{name}, on {struct.unpack_from('<H', data, body + len(name) + 1)[0]} faces"
    if tag == 0x0100:
        return str(struct.unpack_from('<f', data, body)[0])
    if tag == PIVOT:
        return str(struct.unpack_from('<3f', data, body))
    if tag == POS_TRACK:
        return str(struct.unpack_from('<3f', data, pos + length - 12))
    if tag == NODE_ID:
        return str(struct.unpack_from('<h', data, body)[0])
    if tag == MATRIX:
        return str(np.array(struct.unpack_from('<12f', data, body)).round(4).tolist())
    if tag not in CONTAINERS and length <= 40:
        return data[body:pos + length].hex(' ')
    return ''


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    data = open(sys.argv[1], 'rb').read()
    for depth, pos, tag, length in walk(data):
        print('  ' * depth + f'{tag:04X} {NAMES.get(tag, ""):<13} {length:>7}  {describe(data, pos, tag, length)}')
