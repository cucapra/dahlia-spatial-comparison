# GEMMNCubed Benchmarks in Spatial

This repo is used to automatically run GEMMNCubed benchmarks in [gemm.scala](https://github.com/tissue3/spatial-quickstart/blob/master/src/main/scala/gemm.scala) where the unrolling factor is set to 1 to 16.

## Prerequisites:
  
  * [Java 8](https://www.digitalocean.com/community/tutorials/how-to-install-java-with-apt-get-on-ubuntu-16-04)
  * [SBT](https://www.scala-sbt.org/1.0/docs/Setup.html)
  * gmp-devel, [pkg-config](http://pkgconfig.freedesktop.org/releases/pkg-config-0.28.tar.gz), [libisl](http://isl.gforge.inria.fr/).
  * Vivado v2017.2_sdx (64-bit)
## To setup:

Spatial-quickstart provides the scala project skeleton and a build.sbt configured to fetch Spatial from the Nexus Repository Manager without requiring a local installation or build of the compiler.  If you prefer the full language and compiler, visit the [Spatial repo](https://github.com/stanford-ppl/spatial)

Please follow these instructions to install spatial-quickstart:
  
```bash
git clone https://github.com/stanford-ppl/spatial-quickstart.git
```


## To run:

```bash
sh gemm_generate.sh
```

## Links:

  * [Source Code](https://github.com/stanford-ppl/spatial-lang)
  * [Website](https://spatial.stanford.edu)
  * [Documentation](http://spatial-lang.readthedocs.io/en/latest/)
  * [Forum](https://groups.google.com/forum/#!forum/spatial-lang-users)
