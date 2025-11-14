import binascii
import datetime as dt
import hashlib
import pickle
import struct
from functools import wraps
from pathlib import Path
from typing import Callable, ParamSpec, TypeVar

P = ParamSpec("P")
T = TypeVar("T")


class LocalCache:
    """This class provides a decorator will locally cache the results of a function (e.g. to avoid repetition of remote queries)

    This should only be used for local, trusted caching (it is pickle-based). It works by hashing the function name + parameters
    to generate a filename then storing an expiry date + pickle of function response in the file. When the class is first
    initialised it'll scan the local directory and clean up any expired files.

    Usage:
        local_cache = LocalCache(".cache/blah/cachify")

        @local_cache.cachify(lifetime=7)
        def some_function(args):
            blah = blah

    Args:
        cache_folder_path (str): Where to store the cache (optional)
    """

    def __init__(self, cache_folder_path: str | Path = "../.cache/cachify"):
        self.folder_path = Path(cache_folder_path)
        self.folder_path.mkdir(exist_ok=True, parents=True)

        # clean up any old files on init
        for file_path in self.folder_path.iterdir():
            if file_path.name[0] != ".":
                with file_path.open(mode="rb") as infile:
                    file_expiry = dt.datetime.fromtimestamp(
                        struct.unpack("f", infile.read(4))[0]
                    )

                if file_expiry < dt.datetime.utcnow():
                    file_path.unlink()

    def cachify(self, lifetime: int = 1):
        """Apply to a function as a decorator in order to locally cache function executions

        Creates a file using a hash of the function name, input parameters. When the wrapped function is called, this will
        look for a cached response, check expiry and then either return the cached response or execute the function. Executions
        that throw exceptions will not be cached.

        Usage:
            local_cache = LocalCache()

            @local_cache.cachify(lifetime=7)
            def some_function(args):
                blah = blah

        Args:
            lifetime: Number of days until cache file should expire
        """

        def decorator_cachify(func: Callable[P, T]) -> Callable[P, T]:
            @wraps(func)
            def wrapper(*args: P.args, **kwds: P.kwargs) -> T:
                function_execution_hash = hashlib.sha256(
                    str(
                        [func.__module__, func.__name__, args, sorted(kwds.items())]
                    ).encode()
                ).hexdigest()
                file_path = self.folder_path.joinpath(function_execution_hash)

                if file_path.exists():
                    try:
                        with file_path.open("rb") as infile:
                            file_expiry = dt.datetime.fromtimestamp(
                                struct.unpack("f", infile.read(4))[0]
                            )
                            if file_expiry > dt.datetime.utcnow():
                                return pickle.load(infile)
                    except (ValueError, binascii.Error, EOFError) as e:
                        print(
                            "Something went wrong loading cached execution of"
                            f" {func.__name__}. Re-executing"
                        )
                    file_path.unlink()

                result = func(*args, **kwds)
                file_expiry = dt.datetime.utcnow() + dt.timedelta(days=lifetime)
                Path(file_path).touch(exist_ok=True)
                with file_path.open("wb") as outfile:
                    outfile.write(struct.pack("f", file_expiry.timestamp()))
                    pickle.dump(result, outfile)
                return result

            return wrapper

        return decorator_cachify

    def clear_cache(self, delete_dir=False):
        """Removes all files from the cache directory (and by request the whole dir)"""
        for file_path in self.folder_path.iterdir():
            file_path.unlink()
        if delete_dir:
            self.folder_path.rmdir()
