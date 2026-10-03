"""Collects the log that BSPGEN left in each POF of a folder, in its PINF chunk.

Prints the command line and the face counts of each model, then how many models were built with each set of flags.

    python bspgen_logs.py D:/tmp/fs2_pof
"""
import collections
import os
import re
import sys

from pof_header import pofs_in, read_pof

if __name__ == '__main__':
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    flags = collections.Counter()
    scales = collections.Counter()
    for path in pofs_in(sys.argv[1]):
        pof = read_pof(path)
        command = re.search(r'Command line: (.*)', pof['log'])
        command = command.group(1).strip() if command else ''
        started = re.search(r'Started with (\d+) vertices and (\d+) faces', pof['log'])
        ended = re.search(r'Ended with (\d+) vertices and (\d+) faces', pof['log'])
        scale = re.search(r'Scale factor:\s*(\S+)', pof['log'])
        faces = f'{started.group(2):>5} faces to {ended.group(2):<5}' if started and ended else f"{'':>20}"
        print(f"{os.path.basename(path):<28} version {pof['version']}  {faces}  {command}")
        flags[pof['version'], ' '.join(word for word in command.split()[1:] if word.startswith('-')) or 'no flags'] += 1
        scales[scale.group(1) if scale else 'not given'] += 1
    print()
    for (version, used), count in sorted(flags.items()):
        print(f'version {version}, {used}: {count}')
    for scale, count in scales.most_common():
        print(f'scale factor {scale}: {count}')
