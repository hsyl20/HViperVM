ViperVM 0.3
===========

ViperVM is a runtime system for high-performance computing on heterogeneous
architectures (CPUs, GPUs, Xeon Phi...) written in Haskell. 

The basic idea is to reduce in parallel a pure functional program where some
functions are associated to computational kernels (OpenCL, etc.). The runtime
system automatically schedules these kernels on available accelerators,
performing memory management (allocation, release, transfer) appropriately.

See examples in "apps/" to have an idea of what ViperVM is capable of. This
software is still in its infancy. Read CHANGES to have a summary of what is
introduced in each release.
