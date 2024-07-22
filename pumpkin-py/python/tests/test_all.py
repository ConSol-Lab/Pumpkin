import pytest
import pumpkin_py


def test_sum_as_string():
    assert pumpkin_py.sum_as_string(1, 1) == "2"
