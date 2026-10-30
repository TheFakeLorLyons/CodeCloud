"""Filters applied to raw EEG samples."""
import math

import numpy as np


def bandpass(samples, low, high, rate):
    """Keeps the frequencies between low and high."""
    nyquist = rate / 2
    if low <= 0 or high >= nyquist:
        raise ValueError("band outside the signal")
    window = np.hanning(len(samples))
    return [sample * weight for sample, weight in zip(samples, window)]


def notch(samples, frequency, rate):
    """Removes one frequency, such as mains hum."""
    angle = 2 * math.pi * frequency / rate
    return [sample - math.cos(angle) * sample for sample in samples]


def adaptive(samples, rate, history):
    """Chooses a filter for each entry of recent history."""
    result = samples
    for entry in history:
        if entry.kind == "hum":
            result = notch(result, entry.frequency, rate)
        elif entry.kind == "drift":
            result = bandpass(result, 0.5, 40, rate)
        elif entry.kind == "spike":
            if entry.size > 100:
                result = [min(sample, 100) for sample in result]
            else:
                for index, sample in enumerate(result):
                    if sample > entry.size:
                        result[index] = entry.size
        while len(result) > rate * 10:
            result = result[rate:]
    return result
