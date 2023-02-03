import re
import subprocess
from typing import Set
from pathlib import Path

def importing_packages(mod: str) -> Set[str]:
    out = subprocess.check_output(
            ['rg', '-Ll', f'^import +(qualified +)?{mod}'],
            cwd='recent-trees',
            encoding='UTF-8'
    )
    return {Path(l).parts[0] for l in out.split() }

def ignored_package(pkg: str) -> bool:
    PKGS = [
        'base', 'ghci', 'ghc', 'ghc-boot', 'ghc-boot-th', 'ghc-lib-parser'
    ]
    for name in PKGS:
        if re.match(rf'{name}-[0-9]+(\.[0-9]+)*', pkg):
            return True

    return False


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('module')
    parser.add_argument('-s', '--show-packages', action='store_true')
    args = parser.parse_args()

    pkgs = importing_packages(args.module)
    pkgs = { p for p in pkgs if not ignored_package(p) }
    if args.show_packages:
        print('\n'.join(pkgs))
    print(len(pkgs))

main()
