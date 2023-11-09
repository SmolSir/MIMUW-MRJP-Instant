import os
from typing import Callable
from functools import wraps, partial
import subprocess
import logging
import re

LOGGER = logging.getLogger(__name__)

STACK_LIMIT_PATTERN = r".limit\s+stack\s+(\d+)"
LOCALS_LIMIT_PATTERN = r".limit\s+locals\s+(\d+)"
ALLOCA_PATTERN = r"=\s+alloca"


class TestException(Exception):
    pass


class TestContext:
    def __init__(self, ins_path, output_path, j_path, class_path, ll_path, bc_path, test_obj, workspace):
        self.ins_path = ins_path,
        self.output_path = output_path
        self.j_path = j_path
        self.class_path = class_path
        self.ll_path = ll_path
        self.bc_path = bc_path
        self.test_obj = test_obj
        self.workspace = workspace
        self._j_content_cached = None
        self._ll_content_cached = None

    @staticmethod
    def get_file_content(file_path: str) -> str:
        with open(file_path, "r") as file:
            return file.read()

    def get_stack_limits(self) -> [int]:
        jasmin_file_content = self.j_content
        stack_limits = re.findall(STACK_LIMIT_PATTERN, jasmin_file_content)
        return [int(x) for x in stack_limits]

    def get_locals_limits(self) -> [int]:
        jasmin_file_content = self.j_content
        locals_limits = re.findall(LOCALS_LIMIT_PATTERN, jasmin_file_content)
        return [int(x) for x in locals_limits]

    def count_alloca(self) -> int:
        llvm_file_content = self.ll_content
        alloca_count = re.findall(ALLOCA_PATTERN, llvm_file_content)
        return len(alloca_count)

    @property
    def j_content(self) -> str:
        if not self._j_content_cached:
            self._j_content_cached = self.get_file_content(self.j_path)
        return self._j_content_cached

    @property
    def ll_content(self) -> str:
        if not self._ll_content_cached:
            self._ll_content_cached = self.get_file_content(self.ll_path)
        return self._ll_content_cached

    def compile_jvm(self, path: str):
        self.test_obj.run_subprocess([self.test_obj.global_jvm_compiler, path])

    def compile_llvm(self, path: str):
        self.test_obj.run_subprocess([self.test_obj.global_llvm_compiler, path])

    def check_if_file_exists(self, file: str):
        self.test_obj.check_if_file_exists(file)

    def run_jvm(self, classpath: str, classname: str):
        self.test_obj.run_subprocess(
            ["java", "-cp", f"lib:{classpath}", classname],
            subprocess.DEVNULL,
            subprocess.PIPE
        )

    def run_llvm(self, file: str):
        self.test_obj.run_subprocess(
            ["lli", file],
            subprocess.DEVNULL,
            subprocess.PIPE
        )


class Test:
    global_test_path = "tests"
    global_jvm_compiler = ""
    global_llvm_compiler = ""
    global_workspace = ""
    global_stdout_dump_path = subprocess.DEVNULL
    global_stderr_dump_path = subprocess.DEVNULL
    already_tested = []
    global_divider_length = lambda: ""

    def __init__(self, test_filename: str, desc: str = ""):
        if test_filename is None:
            test_filename = ""
        self.test_filename = test_filename
        self.desc = desc
        self.test_path = os.path.join(self.global_test_path, self.test_filename)

    @property
    def ins_path(self):
        return self.test_path + ".ins"

    @property
    def output_path(self):
        return self.test_path + ".output"

    @property
    def real_jvm_output_path(self):
        return self.test_path + ".real_jvm_output"

    @property
    def real_llvm_output_path(self):
        return self.test_path + ".real_llvm_output"

    @property
    def j_path(self):
        return self.test_path + ".j"

    @property
    def class_path(self):
        return self.test_path + ".class"

    @property
    def ll_path(self):
        return self.test_path + ".ll"

    @property
    def bc_path(self):
        return self.test_path + ".bc"

    def __call__(self, func: Callable[[TestContext], bool]):
        @wraps(func)
        def wrapper(*_args, **_kwargs):
            self.print_run_info()
            if self.test_filename and self.test_filename not in self.already_tested:
                self.compile_jvm()
                self.compile_llvm()
                self.check_if_files_generated()
                self.run_jvm()
                self.run_llvm()
                self.check_output(self.real_llvm_output_path, "LLVM")
                self.check_output(self.real_jvm_output_path, "JVM")
                self.already_tested.append(self.test_filename)
            elif self.test_filename:
                LOGGER.info(f"This testfile has already been run, skipping compilation and output checks")

            LOGGER.info(f"Running after-checks")
            result = func(TestContext(
                self.ins_path,
                self.output_path,
                self.j_path,
                self.class_path,
                self.ll_path,
                self.bc_path,
                self,
                self.global_workspace
            ))
            if not result:
                raise TestException("Test failed in after-checks (without given reason)")

            LOGGER.info(f"All checks passed")

        wrapper.test_object = self
        return wrapper

    def __repr__(self):
        return str(self)

    def __str__(self):
        return f"Test({self.test_filename})"

    @staticmethod
    def is_test(func: Callable):
        return hasattr(func, "test_object")

    def print_run_info(self):
        LOGGER.info(self.global_divider_length())
        LOGGER.info(f"Running: {self.desc}")
        LOGGER.info(f"Testfile: {self.test_filename}")
        LOGGER.info(f"Testpath: {self.test_path}")

    def run_subprocess(self, cmd: [str], stdout=None, stderr=None):
        if stdout is None:
            stdout = self.global_stdout_dump_path
        if stderr is None:
            stderr = self.global_stderr_dump_path
        self._append_dump("=" * 120)
        self._append_dump(f"Running: {' '.join(cmd)}")
        subprocess.run(cmd, check=True, cwd=self.global_workspace, stdout=stdout, stderr=stderr)
        self._append_dump("^" * 120)

    def compile_jvm(self):
        self.run_subprocess([self.global_jvm_compiler, self.ins_path])

    def compile_llvm(self):
        self.run_subprocess([self.global_llvm_compiler, self.ins_path])

    def run_jvm(self):
        classpath = ':'.join(["lib", self.global_test_path])
        with open(self.real_jvm_output_path, "w") as output_file:
            self.run_subprocess(
                ["java", "-cp", classpath, self.test_filename],
                output_file,
                subprocess.PIPE
            )

    def run_llvm(self):
        with open(self.real_llvm_output_path, "w") as output_file:
            self.run_subprocess(
                ["lli", self.bc_path],
                output_file,
                subprocess.PIPE
            )

    def check_output(self, real_output_path: str, output_type: str):
        self.run_subprocess(
            ["diff", "-w", self.output_path, real_output_path],
            subprocess.DEVNULL,
            subprocess.DEVNULL
        )
        LOGGER.info(f"Output is correct for {output_type}")

    @staticmethod
    def check_if_file_exists(file: str):
        if not os.path.isfile(file):
            if file.endswith(".class"):
                LOGGER.warning(f"Could not find file {file}, but this is expected for .class files")
                LOGGER.warning(f"Please make sure that your classname has the same name as your testfile (including the upper/lowercase)")
            raise FileNotFoundError(f"Could not find file {file}")
        if not os.access(file, os.R_OK):
            raise PermissionError(f"File {file} is not readable")

    def check_if_files_generated(self):
        for file in [self.j_path, self.class_path, self.ll_path, self.bc_path]:
            self.check_if_file_exists(file)

    def _append_dump(self, message: str):
        def __append(dump_path):
            if dump_path != subprocess.DEVNULL:
                dump_path.write(message + "\n")
                dump_path.flush()

        __append(self.global_stdout_dump_path)
        __append(self.global_stderr_dump_path)


class TestGroup:
    def __init__(self, test_filename_prefix: str, n: int, desc: str = ""):
        self.test_filename_prefix = test_filename_prefix
        self.n = n
        self.desc = desc

    def __call__(self, func: Callable[[TestContext], bool]):
        @wraps(func)
        def wrapper(*_args, **_kwargs):
            for i in range(1, self.n + 1):
                test = Test(f"{self.test_filename_prefix}{i:02d}",
                            f"{self.desc} [{i}/{self.n}]")
                test(partial(func, i=i))()

        wrapper.test_object = self
        return wrapper
