from pathlib import Path
import glob
import re

def read_hs(f: Path) -> str:
    try:
        return f.read_text(encoding='UTF-8')
    except UnicodeDecodeError:
        return f.read_text(encoding='ISO-8859-1')

def main() -> None:
    stability_re = re.compile(r'stability *: *(\w+)', re.I)
    for f in glob.iglob('trees/**/*.hs', recursive=True):
        f = Path(f)
        m = stability_re.search(read_hs(f))
        if m is not None:
            print(f, m.group(1))

main()
