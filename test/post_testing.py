import logging
import os
import re

from core import Test, TestContext, TestGroup
import random_tests

LOGGER = logging.getLogger(__name__)


def count(regex: str, text: str):
    return len(re.findall(regex, text))


@Test(test_filename="empty", desc="Empty file")
def test_empty(_: TestContext):
    return True


@Test(test_filename="test01", desc="Single number 42")
def test_test1(_: TestContext):
    return True


@Test(test_filename="test02", desc="Subtraction 44-2")
def test_test2(_: TestContext):
    return True


@Test(test_filename="test03", desc="Many adds 1+1+...+1")
def test_test3(_: TestContext):
    return True


@Test(test_filename="test04", desc="Two variables declaration")
def test_test04(_: TestContext):
    return True


@Test(test_filename="test05", desc="Many 1+1-1+ ... -1")
def test_test05(_: TestContext):
    return True


@Test(test_filename="test06", desc="Complicated expression")
def test_test06(_: TestContext):
    return True


@Test(test_filename="test07", desc="Just 65537")
def test_test07(_: TestContext):
    return True


@Test(test_filename=None, desc="Checks if compiler works with source files in the same directory as the compiler")
def test_same_directory(ctx: TestContext):
    # Test file will be created in compiler directory
    tmp_file = "i_hope_this_is_a_non_conflicting_file_name"
    tmp_file_ins = f"{tmp_file}.ins"
    with open(os.path.join(ctx.workspace, tmp_file_ins), "w") as file:
        file.write("42")
    ctx.compile_jvm(tmp_file_ins)
    ctx.compile_llvm(tmp_file_ins)
    for ext in ["ll", "bc", "j", "class"]:
        ctx.check_if_file_exists(os.path.join(
            ctx.workspace, f"{tmp_file}.{ext}"))

    ctx.run_jvm(ctx.workspace, tmp_file)
    ctx.run_llvm(os.path.join(ctx.workspace, f"{tmp_file}.bc"))
    return True


@Test(test_filename="alloca", desc="[LLVM] Many assignments to a single variable should result in at most one alloca")
def test_alloca(ctx: TestContext):
    assert ctx.count_alloca() <= 1
    return True


@Test(test_filename="constants", desc="[JVM] Specific opcodes should be used for small constants")
def test_constants(ctx: TestContext):
    assert count(r"\biconst_\d\b", ctx.j_content) == 6
    assert count(r"\bbipush\b", ctx.j_content) == 2
    assert count(r"\bsipush\b", ctx.j_content) == 2
    assert count(r"\bldc\b", ctx.j_content) == 2
    return True


@Test(test_filename="iload_and_istore", desc="[JVM] Specific opcodes should be used for variables with small indexes")
def test_iload_and_istore(ctx: TestContext):
    for n in range(1, 4):
        assert count(rf"\biload_{n}\b", ctx.j_content) == 1
        assert count(rf"\biload\s+{n}\b", ctx.j_content) == 0
        assert count(rf"\bistore_{n}\b", ctx.j_content) == 1
        assert count(rf"\bistore\s+{n}\b", ctx.j_content) == 0
    return True


@Test(test_filename="associativity_addition", desc="Addition should be right-associative")
def test_associativity_addition(ctx: TestContext):
    assert max(ctx.get_stack_limits()) <= 3
    assert count(
        r"\b40\b[\w\W]*\b30\b[\w\W]*\b20\b[\w\W]*\b10\b", ctx.j_content) == 1
    return True


@Test(test_filename="associativity_subtraction", desc="Subtraction should be left-associative")
def test_associativity_subtraction(ctx: TestContext):
    assert max(ctx.get_stack_limits()) <= 3
    return True


@Test(test_filename="associativity_multiplication", desc="Multiplication should be left-associative")
def test_associativity_multiplication(ctx: TestContext):
    assert max(ctx.get_stack_limits()) <= 3
    assert count(
        r"\b20\b[\w\W]*\b30\b[\w\W]*\b40\b[\w\W]*\b50\b", ctx.j_content) == 1
    return True


@Test(test_filename="associativity_division", desc="Division should be left-associative")
def test_associativity_division(ctx: TestContext):
    assert max(ctx.get_stack_limits()) <= 3
    return True


@Test(test_filename="swaps", desc="[JVM] Swaps should not be used for multiplication and addition")
def test_swaps(ctx: TestContext):
    assert count(r"\bswap\b", ctx.j_content) == 0
    return True


@Test(test_filename="limit_locals", desc="[JVM] Verify is .limit locals set correctly")
def test_limit_locals(ctx: TestContext):
    # Minimum is 42 variables (argument of main might be overwritten).
    # We allow two more as an upper bound.
    assert 42 <= max(ctx.get_locals_limits()) <= 42 + 2
    return True


@TestGroup(test_filename_prefix="random", n=len(random_tests.all), desc="Random test")
def test_zzz_random(ctx: TestContext, i: int):
    m = max(ctx.get_stack_limits())
    bound = random_tests.all[i - 1].stack_limit() + 1
    assert m <= bound, f"Stack limit set to {m}, but {bound} is sufficient"
    return True


@Test(test_filename="deep", desc="Long path")
def test_deep(ctx: TestContext):
    assert max(ctx.get_stack_limits()) <= 3
    return True


@Test(test_filename="multiline_instruction", desc="Multiline instruction")
def test_multiline_instruction(_: TestContext):
    return True


@Test(test_filename="non_commutativity_subtraction", desc="[JVM] Subtraction is not commutative, so optimizing stack requires swapping arguments")
def test_non_commutativity_subtraction(ctx: TestContext):
    assert count(r"\bswap\b", ctx.j_content) == 1
    return True


@Test(test_filename="non_commutativity_division", desc="[JVM] Division is not commutative, so optimizing stack requires swapping arguments")
def test_non_commutativity_division(ctx: TestContext):
    assert count(r"\bswap\b", ctx.j_content) == 1
    return True
