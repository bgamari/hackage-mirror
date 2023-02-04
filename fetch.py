#!/usr/bin/env python3

import queue
import os
import threading
import logging
import urllib.request
import subprocess
from pathlib import Path
from typing import List, Dict, NamedTuple

logging.basicConfig(level=logging.INFO)

N_WORKERS = 4
INDEX = Path('index')
TARBALLS = Path('tarballs')
TREES = Path('trees')
RECENT = Path('recent-trees')

HACKAGE_URL = 'https://hackage.haskell.org/'
INDEX_URL = f'{HACKAGE_URL}01-index.tar.gz'

def fetch_index():
    if not INDEX.exists():
        logging.info('fetching index')
        INDEX.mkdir()
        subprocess.run(f'curl {INDEX_URL} | tar -xzC {INDEX}', shell=True, check=True)

fetch_index()

class PackageSpec(NamedTuple):
    package: str
    version: str

    def pkg_ver(self):
        return f'{self.package}-{self.version}'

def parse_index_path(path: Path) -> PackageSpec:
    return PackageSpec(
            package=str(path.parts[1]),
            version=str(path.parts[2])
    )

def build_package_list() -> List[PackageSpec]:
    fs = list(INDEX.glob('**/*.cabal'))
    fs.sort(key=lambda f: f.stat().st_mtime, reverse=True)
    return [ parse_index_path(f) for f in fs ]

def fetch_packages(ps: List[PackageSpec]) -> None:
    TARBALLS.mkdir(exist_ok=True)
    TREES.mkdir(exist_ok=True)

    q = queue.Queue()
    for p in ps:
        q.put(p)

    def worker():
        while True:
            try:
                p = q.get_nowait()
            except queue.Empty as e:
                return
            fetch_package(p)

    workers = [
            threading.Thread(target=worker)
            for i in range(N_WORKERS)
    ]
    for worker in workers:
        worker.start()
    for worker in workers:
        worker.join()

def package_tarball_url(p: PackageSpec) -> str:
    pv = f'{p.package}-{p.version}'
    return f'{HACKAGE_URL}package/{pv}/{pv}.tar.gz'

def fetch_package(p: PackageSpec) -> None:
    pv = f'{p.package}-{p.version}'

    d = TARBALLS / p.package
    d.mkdir(exist_ok=True)
    tarball = d / f'{pv}.tar.gz'
    if not tarball.exists():
        logging.info(f'fetching {pv}')
        url = package_tarball_url(p)
        cp = subprocess.run(['wget', '--quiet', '-c', url], cwd=d)
        if cp.returncode != 0:
            logging.error(f'failed to fetch {pv}: {cp.returncode}')

    d = TREES / p.package
    d.mkdir(exist_ok=True, parents=True)
    if not (d / pv).exists():
        logging.info(f'extracting {pv}')
        (d / pv).mkdir(parents=True, exist_ok=True)
        cp = subprocess.run(['tar', '-xzf', tarball, '-C', d])
        if cp.returncode != 0:
            logging.error(f'failed to extract {pv}: {cp.returncode}')

def most_recent_versions(ps: List[PackageSpec]) -> Dict[str, PackageSpec]:
    from packaging import version
    res = {}
    for p in ps:
        if p.package in res:
            v0 = version.parse(res[p.package].version)
            v1 = version.parse(p.version)
            if v1 < v0:
                continue

        res[p.package] = p

    return res

def build_recent(ps: List[PackageSpec]):
    logging.info(f'building {RECENT}')
    versions = most_recent_versions(ps)
    RECENT.mkdir(exist_ok=True)
    for p in versions.values():
        src = Path('..') / TREES / p.package / p.pkg_ver()
        dst = RECENT / p.pkg_ver()
        if dst.exists():
            dst.unlink()
        os.symlink(src, dst)

def main(recent_only: bool) -> None:
    packages = build_package_list()
    if recent_only:
        packages = most_recent_versions(packages).values()

    Path('packages.lst').write_text(
            '\n'.join(f'{p.package} {p.version}' for p in packages)
    )

    fetch_packages(packages)
    build_recent(packages)

if __name__ == '__main__':
    main(False)
