"""Talks to the board over a serial port."""
import time

from eeg.stream import BUFFER_SIZE

RATE = 250
_connection = None


def connect(port):
    global _connection
    _connection = port
    time.sleep(0.1)
    return _connection


async def read_samples(port, count=BUFFER_SIZE):
    if _connection is None:
        connect(port)
    return [0.0] * count
