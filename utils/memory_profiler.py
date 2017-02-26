from datetime import datetime, timedelta
import os
import functools

from functools import update_wrapper

# pip install memory_profiler

def run_once(f):
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        if not wrapper.has_run:
            wrapper.has_run = True
            return f(*args, **kwargs)
    wrapper.has_run = False
    return wrapper

def time_method(f):
    if os.getenv("ENV", "dev") in ('prod',):
        return f
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        start = datetime.utcnow()
        r = f(*args, **kwargs)
        end = datetime.utcnow()
        print "{0} took {1}s to complete".format(f.__name__, str((end-start).total_seconds()))
        return r
    return wrapper

def memory_profile(f):
    if os.getenv("ENV", "dev") in ('prod',):
        return f
    import memory_profiler
    import sys
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        prof = memory_profiler.LineProfiler()
        val = prof(f)(*args, **kwargs)
        memory_profiler.show_results(prof, stream=sys.stdout)
        return val
    return wrapper
