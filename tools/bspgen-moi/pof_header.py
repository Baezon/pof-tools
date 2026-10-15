"""Reader for what BSPGEN wrote into a POF: the header with its cross sections, the submodel headers, and its log.

It reads both the FreeSpace 1 chunks (OHDR, SOBJ) and the FreeSpace 2 ones (HDR2, OBJ2), and none of the geometry.

Run it on a file to print what it holds, or on a folder to count the versions in it:
    python pof_header.py Fighter01.POF
    python pof_header.py D:/tmp/fs1_pof
"""
import collections
import glob
import os
import struct
import sys

# the mass in the file is the volume itself before this version, and 4.65 * volume^(2/3) from it on
AREA_MASS_VERSION = 2009
# each cross section is a depth along z and a radius; how many there are is what the flag -c of BSPGEN sets
CROSS_SECTIONS_VERSION = 2014


def read_string(data, pos):
    length = struct.unpack_from('<i', data, pos)[0]
    return data[pos + 4:pos + 4 + length].split(b'\0')[0].decode('latin1'), pos + 4 + length


def read_verts(data, bsp):
    """The verts of a submodel, from the DEFPOINTS chunk that opens its BSP data. Each is followed in the file by its normals."""
    tag, _, count, _, start = struct.unpack_from('<5i', data, bsp)
    if tag != 1:
        return []
    normals = data[bsp + 20:bsp + 20 + count]
    verts, p = [], bsp + start
    for n in normals:
        verts.append(struct.unpack_from('<3f', data, p))
        p += 12 + 12 * n
    return verts


def read_pof(path):
    data = open(path, 'rb').read()
    if data[:4] != b'PSPO':
        raise ValueError(f'{path} is not a POF')
    version = struct.unpack_from('<i', data, 4)[0]
    pof = {'version': version, 'submodels': {}, 'log': '', 'chunks': []}
    pos = 8
    while pos + 8 <= len(data):
        tag = data[pos:pos + 4]
        length = struct.unpack_from('<i', data, pos + 4)[0]
        p = pos + 8
        pof['chunks'].append(tag.decode('latin1'))
        if tag in (b'OHDR', b'HDR2'):
            if tag == b'OHDR':
                pof['n_models'], pof['radius'], pof['flags'] = struct.unpack_from('<ifi', data, p)
            else:
                pof['radius'], pof['flags'], pof['n_models'] = struct.unpack_from('<fii', data, p)
            pof['mins'] = struct.unpack_from('<3f', data, p + 12)
            pof['maxs'] = struct.unpack_from('<3f', data, p + 24)
            p += 36
            for key in ('detail', 'debris'):
                count = struct.unpack_from('<i', data, p)[0]
                pof[key] = list(struct.unpack_from(f'<{count}i', data, p + 4))
                p += 4 + 4 * count
            if version >= 1903:
                pof['mass'] = struct.unpack_from('<f', data, p)[0]
                pof['com'] = struct.unpack_from('<3f', data, p + 4)
                pof['moi'] = [struct.unpack_from('<3f', data, p + 16 + 12 * row) for row in range(3)]
                p += 52
            if version >= CROSS_SECTIONS_VERSION:
                count = struct.unpack_from('<i', data, p)[0]
                pof['cross_sections'] = [struct.unpack_from('<2f', data, p + 4 + 8 * i) for i in range(count)]
        elif tag in (b'SOBJ', b'OBJ2'):
            number = struct.unpack_from('<i', data, p)[0]
            if tag == b'OBJ2':
                radius, parent = struct.unpack_from('<fi', data, p + 4)
                offset = struct.unpack_from('<3f', data, p + 12)
            else:
                parent = struct.unpack_from('<i', data, p + 4)[0]
                offset = struct.unpack_from('<3f', data, p + 8)
                radius = struct.unpack_from('<f', data, p + 20)[0]
            p += 24
            bmin = struct.unpack_from('<3f', data, p + 12)
            bmax = struct.unpack_from('<3f', data, p + 24)
            name, p = read_string(data, p + 36)
            properties, p = read_string(data, p)
            pof['submodels'][number] = dict(name=name, parent=parent, offset=offset, radius=radius, bmin=bmin, bmax=bmax, properties=properties,
                                            verts=read_verts(data, p + 16))
        elif tag == b'PINF':
            pof['log'] = data[p:p + length].replace(b'\0', b'\n').decode('latin1').strip()
        pos += 8 + length
    return pof


def stored_volume(pof):
    """The volume BSPGEN arrived at, as the stored mass has it.

    The power is 0.6667 and not two thirds. That is what the loader uses, and the FreeSpace 2 models whose mass was converted from a
    FreeSpace 1 volume bear it out to the last digit. That BSPGEN used the same once it did the conversion itself is assumed.
    """
    return pof['mass'] if pof['version'] < AREA_MASS_VERSION else (pof['mass'] / 4.65) ** (1 / 0.6667)


def pofs_in(folder):
    return sorted(path for path in glob.glob(os.path.join(folder, '*')) if path.lower().endswith('.pof'))


if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    if os.path.isdir(sys.argv[1]):
        versions = collections.Counter(read_pof(path)['version'] for path in pofs_in(sys.argv[1]))
        for version, count in sorted(versions.items()):
            print(f'version {version}: {count}')
    else:
        pof = read_pof(sys.argv[1])
        print(f"version {pof['version']}, radius {pof['radius']}, detail levels {pof['detail']}, debris {pof['debris']}")
        print(f"bounding box {pof['mins']} to {pof['maxs']}")
        if 'mass' in pof:
            print(f"mass {pof['mass']}, which is a volume of {stored_volume(pof)}")
            print(f"center of mass {pof['com']}")
            for row in pof['moi']:
                print(f'moment of inertia, inverted  {row}')
        for depth, radius in pof.get('cross_sections', []):
            print(f'cross section at {depth:.3f}, of radius {radius:.3f}')
        for number, sm in sorted(pof['submodels'].items()):
            print(f"  {number:>3} {sm['name']:<28} parent {sm['parent']:>3}  offset {tuple(round(v, 3) for v in sm['offset'])}")
        print(pof['log'])
