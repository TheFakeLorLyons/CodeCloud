#!/usr/bin/env python3
"""Trains a classifier on recorded sessions."""
import sys

from eeg import Stream
from eeg.filters import notch


def main(arguments):
    stream = Stream(arguments[1])
    return notch(stream.buffer, 60, 250)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
