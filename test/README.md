# Usage

Run `python3 main.py` to run tests.
By default, the script will look for the tarball solution in the script's directory.

Optional arguments:
- `-v <name>` | `--save-subprocesses-outputs <name>` - writes verbose information to dump files `<name>.out` and `<name>.err`. This includes `insc_jvm`, `insc_llvm`, `java`, `lli` and `diff`
- `-t <file|dir>` | `--tar-location <file|dir>` - tarball parent path or tarball itself

We don't test for:

- invalid syntax,
- division by zero,
- use of undeclared variables,
- int32 overflow,
- printing an int without a swap,

as it is not required by the assignment - according to dr Benke - however it does not mean it cannot be checked in some lab groups.
