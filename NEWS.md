# pipetree 0.4.1.9001

## Additions

- Added `cfetch()`, which uses a __super__ simple local cache (`xfun::rds_cache()`) to wrap
  calls to `load_merged_partitions()`

# pipetree 0.4.1

## Modifications 

- Allow config path to be given using sys env variable.
- Change `load_merged_partitions()` to fetch the list of cached targets via `cache$list()` rather than `drake::cached(cache = cache)`, as it seems much faster.

## Minor

- Add more logs

# pipetree 0.4.0

## Modifications

- Now `get_project_metadata` includes information about the VCS versioning and 
  package version of the calling env, as well as the time at which the metadata was
  fetched.

# pipetree 0.3.0

- Add `load_merged_partitions`, for conveniently loading dataframes that have been 
  split into partitions, and combining the various chunks of each dataframe back into one.

# pipetree 0.2.1

- Added non-interactive mode for `get_project_metadata`, so it doesnt bust sessions

# pipetree 0.2.0

- Add function for getting metadata for a dataset
  `get_project_metadata`. 

# pipetree 0.1.1

- Import and export pipe operator

# pipetree 0.1.0

## Features

- Should be able to resolve paths using wrapper functions `get_cache()`, `get_paths`, `load_portrpaths`
