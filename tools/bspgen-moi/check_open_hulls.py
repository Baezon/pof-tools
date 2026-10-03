"""How did BSPGEN weigh a hull with holes in it?

A ray through a closed mesh is inside it between each entry and the exit that follows. Through a mesh with holes, that depends on what
is made of a ray that enters twice, or leaves without having entered. Three readings are tried, along each axis in turn:
  - union: inside wherever more polygons have been left than entered, reckoning from the far end
  - parity: inside after an odd count of crossings, whichever way they faced
  - any: inside wherever the two counts differ

Each is printed as the volume it gives over the volume BSPGEN stored, so that 1 is a match.

    python check_open_hulls.py D:/tmp/fs1_pof
"""
import collections
import sys

import numpy as np

from models import load
from pof_header import stored_volume
from sampling import ray_rule_volumes

RULES = ('union', 'parity', 'any')

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    misses = collections.defaultdict(list)
    print(f"{'model':<18}" + ''.join(f'{rule + ", along x, y and z":>30}' for rule in RULES))
    for model in load(sys.argv[1]):
        if model['closed']:
            continue
        stored = stored_volume(model['pof'])
        ratios = {rule: [] for rule in RULES}
        for axis in range(3):
            volumes = ray_rule_volumes(model['verts'], model['faces'], axis, 250)
            for rule in RULES:
                ratios[rule].append(volumes[rule] / stored)
                misses[rule, axis].append(abs(volumes[rule] / stored - 1))
        print(f"{model['name']:<18}" + ''.join('   ' + ' '.join(f'{ratio:8.4f}' for ratio in ratios[rule]) for rule in RULES))
    print()
    for (rule, axis), missed in sorted(misses.items(), key=lambda item: np.median(item[1])):
        print(f"{rule:<7} along {'xyz'[axis]}   median miss {np.median(missed) * 100:6.2f}%   within 1%: {sum(m <= .01 for m in missed):<3}"
              f' within 3%: {sum(m <= .03 for m in missed)} of {len(missed)}')
