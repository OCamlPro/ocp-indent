## Regression tests

The tests in this folder document known bugs of ocp-indent and differ
from the rest of the test suite.

Each test compares the output of ocp-indent to what we expect it should
be using `diff -u`. In most cases the expected output is the input file
itself but in some we provide the expected output separately.

If the diff goes away, this means we'd have fixed the bug. If the output
of the diff changes though, that should be treated carefully as we don't
expect the rest of the formatting to change in most cases.
