#!/usr/bin/env python3

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent
DEFAULT_PACK_DIR = ROOT / "nd_pack"
SUCCESS_TEXT = "Proof successful! Nothing left to prove."


def default_command() -> list[str]:
    if shutil.which("runghc") is not None:
        return ["runghc", "-isrc", "app/Main.hs"]

    if shutil.which("stack") is not None and (ROOT / "stack.yaml").exists():
        return ["stack", "run", "theodore-exe"]

    if shutil.which("cabal") is not None and (ROOT / "theodore.cabal").exists():
        return ["cabal", "run", "theodore-exe"]

    raise RuntimeError("Could not find stack or cabal in PATH.")


def default_build_command(command: list[str]) -> list[str] | None:
    if command[:2] == ["stack", "exec"]:
        return ["stack", "build", "theodore:exe:theodore-exe"]

    return None


def stringify_output(value: str | bytes | None) -> str:
    if value is None:
        return ""
    if isinstance(value, bytes):
        return value.decode(errors="replace")
    return value


def display_path(path: Path) -> str:
    try:
        return str(path.relative_to(ROOT))
    except ValueError:
        return str(path)


def run_case(command: list[str], thd_file: Path, timeout: int) -> tuple[bool, str]:
    completed = subprocess.run(
        command,
        input=f"{thd_file}\n",
        cwd=ROOT,
        text=True,
        capture_output=True,
        timeout=timeout,
        check=False,
    )

    output = completed.stdout + completed.stderr
    passed = completed.returncode == 0 and SUCCESS_TEXT in output
    return passed, output


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run Theodore against every .thd proof in nd_pack."
    )
    parser.add_argument(
        "files",
        nargs="*",
        type=Path,
        help="Optional .thd files to check. Defaults to nd_pack/*.thd.",
    )
    parser.add_argument(
        "--dir",
        type=Path,
        default=DEFAULT_PACK_DIR,
        help="Directory containing .thd files when no explicit files are provided.",
    )
    parser.add_argument(
        "--cmd",
        nargs="+",
        default=None,
        help="Command used to run Theodore. Defaults to runghc -isrc app/Main.hs, then stack/cabal.",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=30,
        help="Timeout in seconds for each proof file.",
    )
    parser.add_argument(
        "--show-output",
        action="store_true",
        help="Print Theodore output for every checked file.",
    )
    parser.add_argument(
        "--skip-build",
        action="store_true",
        help="Do not run the default one-time build before checking files.",
    )
    parser.add_argument(
        "--build-timeout",
        type=int,
        default=300,
        help="Timeout in seconds for the one-time default Stack build.",
    )
    return parser.parse_args()


def collect_files(args: argparse.Namespace) -> list[Path]:
    if args.files:
        return sorted(path.resolve() for path in args.files)

    pack_dir = args.dir.resolve()
    return sorted(pack_dir.glob("*.thd"))


def main() -> int:
    args = parse_args()

    try:
        command = args.cmd if args.cmd is not None else default_command()
    except RuntimeError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    files = collect_files(args)
    if not files:
        print("error: no .thd files found", file=sys.stderr)
        return 2

    build_command = None if args.skip_build else default_build_command(command)
    if build_command is not None:
        print("Building Theodore executable...", flush=True)
        try:
            build = subprocess.run(
                build_command,
                cwd=ROOT,
                text=True,
                capture_output=True,
                timeout=args.build_timeout,
                check=False,
            )
        except subprocess.TimeoutExpired as exc:
            print(f"error: build timed out after {args.build_timeout}s", file=sys.stderr)
            print(stringify_output(exc.stdout) + stringify_output(exc.stderr), file=sys.stderr)
            return 2

        if build.returncode != 0:
            print("error: build failed", file=sys.stderr)
            print((build.stdout + build.stderr).rstrip(), file=sys.stderr)
            return build.returncode

    failures: list[tuple[Path, str]] = []

    print(f"Running {len(files)} Theodore proof checks...", flush=True)
    for thd_file in files:
        try:
            passed, output = run_case(command, thd_file, args.timeout)
        except subprocess.TimeoutExpired as exc:
            partial = stringify_output(exc.stdout) + stringify_output(exc.stderr)
            failures.append((thd_file, f"Timed out after {args.timeout}s\n{partial}"))
            print(f"FAIL {display_path(thd_file)}", flush=True)
            continue

        status = "PASS" if passed else "FAIL"
        print(f"{status} {display_path(thd_file)}", flush=True)

        if args.show_output:
            print(output.rstrip())

        if not passed:
            failures.append((thd_file, output))

    passed_count = len(files) - len(failures)
    print(f"\nSummary: {passed_count}/{len(files)} passed")

    if failures:
        print("\nFailed files:")
        for thd_file, _ in failures:
            print(f"- {display_path(thd_file)}")
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
