import argparse
import importlib
import os.path
import pathlib
import pkgutil
import tarfile
import subprocess
import logging
from core import Test
from inspect import getmembers, isfunction

LOGGER = logging.getLogger("main")
logging.basicConfig(level=logging.INFO, format="%(message)s")


def divider(*argv, **kwargs):
    return "\n" + "=" * os.get_terminal_size().columns


def detect_tar_file(tar_location: str) -> str:
    allowed_extensions = ["tar.gz", "tgz"]
    for ext in allowed_extensions:
        if tar_location.endswith(ext):
            if not os.path.isfile(tar_location):
                raise FileNotFoundError(f"Could not find tar file in {tar_location}")
            LOGGER.info(f"Using declared tarball {tar_location}")
            return tar_location
    for file in [file for x in allowed_extensions for file in pathlib.Path(tar_location).glob(f"*.{x}")]:
        LOGGER.info(f"Found tar file {file}")
        return str(file)
    raise FileNotFoundError(f"Could not find tar file in {tar_location}")


def make_workspace(workspace: str) -> str:
    LOGGER.info(f"Creating workspace {workspace}")
    if not os.path.isdir(workspace):
        os.mkdir(workspace)
    return workspace


def extract_tar_file(tar_file: str, workspace: str) -> str:
    LOGGER.info(f"Extracting tar file {tar_file} to {workspace}")
    with tarfile.open(tar_file) as tar:
        tar.extractall(workspace)
    return workspace


def build_compiler(workspace: str):
    LOGGER.info(f"Cleaning compiler in {workspace}")
    subprocess.run(["make", "clean"], cwd=workspace, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    LOGGER.info(f"Building compiler in {workspace}")
    subprocess.run(["make"], cwd=workspace, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def check_compiler(workspace: str) -> [str, str]:
    LOGGER.info(f"Checking compiler in {workspace}")
    expected_compilers = [os.path.join(workspace, x) for x in ["insc_jvm", "insc_llvm"]]
    for compiler in expected_compilers:
        if not os.path.isfile(compiler):
            raise FileNotFoundError(f"Could not find compiler {compiler}")
        if not os.access(compiler, os.X_OK):
            raise PermissionError(f"Compiler {compiler} is not executable")
    return expected_compilers


def run_tests(tests_path: str, compilers: [str, str], workspace: str, verbose):
    LOGGER.info("Generating random tests")
    import random_tests
    random_tests.save_tests(tests_path + "/random")
    LOGGER.info(f"Running tests from {tests_path}")
    LOGGER.info(divider())
    Test.global_test_path = tests_path
    Test.global_workspace = workspace
    Test.global_jvm_compiler = compilers[0]
    Test.global_llvm_compiler = compilers[1]
    Test.global_divider_length = divider
    if verbose:
        Test.global_stdout_dump_path = open(f"{verbose}.out", "w")
        Test.global_stderr_dump_path = open(f"{verbose}.err", "w")
    import post_testing
    pkgpath = os.path.dirname(post_testing.__file__)
    for _, name, _ in pkgutil.iter_modules([pkgpath]):
        mod = importlib.import_module(name)
        all_functions = getmembers(mod, isfunction)
        for name, function in sorted(all_functions):
            if Test.is_test(function):
                function()
    LOGGER.info(divider())
    LOGGER.info("All tests passed")
    if verbose:
        Test.global_stdout_dump_path.close()
        Test.global_stderr_dump_path.close()


def main():
    main_file_path = os.path.dirname(os.path.realpath(__file__))
    parser = argparse.ArgumentParser("MRJP Compiler tester")
    parser.add_argument("-p", "--test-path", default="tests", help="Path to tests")
    parser.add_argument("-t", "--tar-location", default="", help="Path to tar file")
    parser.add_argument("-w", "--workspace-location", default="tmp", help="Workspace for compilers")
    parser.add_argument("-v", "--save-subprocesses-outputs", default=None, help="Verbose output dump file")
    args = parser.parse_args()
    tests_path = os.path.join(main_file_path, args.test_path)
    tar_location = os.path.join(main_file_path, args.tar_location)
    tar_file_location = detect_tar_file(tar_location)
    workspace = make_workspace(os.path.join(main_file_path, args.workspace_location))
    extract_tar_file(tar_file_location, workspace)
    build_compiler(workspace)
    compilers = check_compiler(workspace)
    run_tests(tests_path, compilers, workspace, args.save_subprocesses_outputs)


if __name__ == '__main__':
    main()
