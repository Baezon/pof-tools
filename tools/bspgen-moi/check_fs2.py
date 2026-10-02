"""Did the BSPGEN that built FreeSpace 2 work as the one that built FreeSpace 1 did?

Many FreeSpace 1 models were used again in FreeSpace 2. This sorts them by what became of their mass properties, then scores the tensor
formulas against those that were worked out afresh, using the FreeSpace 1 source for the geometry.

    python check_fs2.py D:/tmp/fs1_pof D:/tmp/fs2_pof
"""
import collections
import os
import sys

import numpy as np

from check_tensor import LESS_ONE, NAMES, candidates, error, summary
from models import load
from pof_header import pofs_in, read_pof

# what the loader does to a mass that is a volume
AREA_MASS = lambda volume: np.float32(4.65) * np.float32(volume) ** np.float32(0.6667)


def fate(old, new):
    """What became of a model's mass properties between the two games."""
    old_hull = np.array(old['submodels'][old['detail'][0]]['verts'])
    new_hull = np.array(new['submodels'][new['detail'][0]]['verts'])
    same_hull = old_hull.shape == new_hull.shape and len(old_hull) > 0 and np.abs(np.sort(old_hull, axis=0) - np.sort(new_hull, axis=0)).max() < 1e-4
    old_tensor, new_tensor = np.array(old['moi']), np.array(new['moi'])
    old_mass = old['mass']
    if old['version'] < 2009:
        old_mass = AREA_MASS(old['mass'])
        old_tensor = old_tensor * (old['mass'] / old_mass)
    mass = abs(old_mass - new['mass']) / new['mass']
    tensor = np.abs(old_tensor - new_tensor).max() / np.abs(new_tensor).max()
    center = np.linalg.norm(np.array(old['com']) - np.array(new['com'])) / new['radius']
    if max(mass, tensor, center) < 2e-6:
        kept = 'converted by the formula of the loader' if old['version'] < 2009 else 'carried over as they were'
    else:
        kept = 'worked out afresh'
    return kept, same_hull, mass, tensor, center


if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    name_of = lambda path: os.path.splitext(os.path.basename(path))[0].lower()
    old_pofs = {name_of(path): path for path in pofs_in(sys.argv[1])}
    new_pofs = {name_of(path): path for path in pofs_in(sys.argv[2])}

    fates = collections.defaultdict(list)
    afresh = {}
    for name in sorted(set(old_pofs) & set(new_pofs)):
        old, new = read_pof(old_pofs[name]), read_pof(new_pofs[name])
        if 'mass' not in old or 'mass' not in new or not old['detail'] or not new['detail']:
            continue
        kept, same_hull, mass, tensor, center = fate(old, new)
        fates[kept, same_hull].append(name)
        if kept == 'worked out afresh' and same_hull:
            afresh[name] = (mass, tensor, center)
    for (kept, same_hull), names in sorted(fates.items()):
        print(f"{kept}, the hull {'unchanged' if same_hull else 'changed'}: {len(names)}")
        print('    ' + ', '.join(names))

    print('\nworked out afresh from an unchanged hull: how far the numbers moved')
    moved = np.array(list(afresh.values()))
    for column, label in enumerate(('mass', 'tensor', 'center of mass, over the radius')):
        values = np.sort(moved[:, column])
        print(f'  {label:<32} median {values[len(values) // 2]:.1e}   largest {values[-1]:.1e}')

    old_models = {model['name']: model for model in load(sys.argv[1])}
    print()
    print(f"{'model':<18}" + ''.join(f'{name:>44}' for name in NAMES) + f"{'the last of them, in FreeSpace 1':>44}")
    table = collections.defaultdict(list)
    for model in load(sys.argv[1], pofs_from=sys.argv[2]):
        if model['name'] not in afresh:
            continue
        errors = {name: error(tensor, model['stored']) for name, tensor in candidates(model).items()}
        old = old_models.get(model['name'])
        for name in NAMES:
            table[name].append(errors[name])
        line = f"{model['name']:<18}" + ''.join(f'{errors[name] * 100:43.2f}%' for name in NAMES)
        if old is not None:
            before = error(candidates(old)[LESS_ONE], old['stored'])
            table['before'].append(before)
            line += f'{before * 100:43.2f}%'
        print(line)
    print()
    for name in NAMES:
        print(summary(name, table[name]))
    print(summary('the last of them, in FreeSpace 1', table['before']))
