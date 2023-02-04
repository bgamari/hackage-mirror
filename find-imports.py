import re
import subprocess
from typing import Set
from pathlib import Path

def importing_modules(mod: str) -> Set[Path]:
    out = subprocess.check_output(
            ['rg', '-Ll', f'^import +(qualified +)?{mod}'],
            cwd='recent-trees',
            encoding='UTF-8'
    )
    return { Path(l) for l in out.split() }

def ignored_package(pkg: str) -> bool:
    PKGS = [
        'base', 'ghci', 'ghc', 'ghc-boot', 'ghc-boot-th',
        'ghc-lib', 'ghc-lib-parser', 'ghc-instances',
        'rebase', 'lhc'
    ]
    for name in PKGS:
        if re.match(rf'{name}-[0-9]+(\.[0-9]+)*', pkg):
            return True

    return False

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('module')
    parser.add_argument('-m', '--modules', action='store_true')
    parser.add_argument('-p', '--packages', action='store_true')
    args = parser.parse_args()

    mod_to_pkg = lambda l: l.parts[0]
    mods = importing_modules(args.module)
    mods = { mod for mod in mods
             if not ignored_package(mod_to_pkg(mod)) }
    pkgs = { l.parts[0] for l in mods }
    if args.modules:
        print('\n'.join(str(m) for m in mods))
    if args.packages:
        print('\n'.join(pkgs))
    print(len(pkgs))

main()
