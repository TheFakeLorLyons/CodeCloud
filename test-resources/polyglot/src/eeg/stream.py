"""Streams samples from a board."""
import asyncio

from eeg import board
from eeg.filters import bandpass

BUFFER_SIZE = 256
_streams = []


class Stream:
    """A running stream of filtered samples."""

    def __init__(self, port):
        self.port = port
        self.buffer = []

    async def read(self):
        raw = await board.read_samples(self.port, BUFFER_SIZE)
        filtered = bandpass(raw, 1, 50, board.RATE)
        self.buffer.extend(filtered)
        return filtered


def open_stream(port):
    global _streams
    stream = Stream(port)
    _streams.append(stream)
    asyncio.get_event_loop().create_task(stream.read())
    return stream
