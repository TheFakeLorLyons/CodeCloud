"""Signal tools for EEG boards."""
from .filters import bandpass, notch
from .stream import Stream

__all__ = ["bandpass", "notch", "Stream"]
