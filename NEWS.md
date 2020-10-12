# pipetree 0.6.0

## Additions

- Add progress bar to all cache accessors (wow!). 
  This can be toggled w/ an option that I haven't bothered to document.

# pipetree 0.5.0

## Additions

- Added `cfetch()`, which uses a __super__ simple local cache (`xfun::rds_cache()`) to wrap
  calls to `load_merged_partitions()`
- Added `export_target_set()` & `export_deidentified_notes()`, which conveniently export
  a set of targets from cache to a directory. Presently, the qs package is used for this.

## Modifications

- Modified `load_merged_partitions` to internally use `data.table::rbindlist()`, instead of
  `dplyr::bind_rows`. This should decrease memory usage, and increase speed.
  ( @datarichard [#25](https://github.com/mstr3336/pipetree/issues/25) )

## Potential Ouchies

- Added dependency on `qs` package. Care has been taken to ensure that this is available on HPC,
  but at some point, if maintaining this dependency on the HPC is too burdensome, it may be worthwhile
  to forgo the performance improvements afforded by `qs` and just use saveRDS.

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
