"""Which geometry did BSPGEN weigh? Sets the volume its stored mass stands for against the source's.

For each model it prints the stored volume over the volume of the detail0 submodel alone, and over that of detail0 with everything below it.

    python check_volume.py D:/tmp/fs1_pof
"""
import sys

from models import SCALE, integrals, open_edges, pairs, tree_of
from p3d import read_p3d
from pof_header import read_pof, stored_volume

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    print(f"{'model':<18} {'version':>7} {'stored':>14} {'over hull':>10} {'over tree':>10}   submodels, of which open")
    for name, (pof_path, p3d_path) in pairs(sys.argv[1]).items():
        pof, p3d = read_pof(pof_path), read_p3d(p3d_path)
        if 'mass' not in pof or not pof['detail']:
            print(f"{name:<18} {pof['version']:>7} has no mass or no detail levels")
            continue
        objects = {key.lower(): o for key, o in p3d['objects'].items()}
        tree = tree_of(pof, pof['detail'][0])
        hull = total = 0.0
        opened = 0
        for number in tree:
            o = objects.get(pof['submodels'][number]['name'].lower())
            if o is None or len(o['faces']) == 0:
                continue
            # the scene is right handed and the POF isn't, which doesn't matter to the size of a volume
            volume = abs(integrals(o['verts'] * SCALE, o['faces'])[0])
            total += volume
            opened += open_edges(o['verts'], o['faces']) > 0
            if number == tree[0]:
                hull = volume
        stored = stored_volume(pof)
        ratio = lambda volume: f'{stored / volume:10.4f}' if volume else f"{'':>10}"
        print(f"{name:<18} {pof['version']:>7} {stored:14.2f} {ratio(hull)} {ratio(total)}   {len(tree)}, {opened}")
