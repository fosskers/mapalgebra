package mapalgebra

import java.util.concurrent.TimeUnit

import geotrellis.raster._
import geotrellis.raster.render._
import org.openjdk.jmh.annotations._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class CompositeBench {
  var r: Tile = _
  var nir: Tile = _
  var b: Tile = _
  var cmap: ColorMap = _

  @Setup
  def setup(): Unit = {
    r   = DoubleArrayTile.empty(512, 512).mapDouble { (c, r, _) => (c * r).toDouble }
    nir = DoubleArrayTile.empty(512, 512).mapDouble { (c, r, _) => (c * r + 1).toDouble }
    b   = DoubleArrayTile.empty(512, 512).mapDouble { (c, r, _) => (c * r + 2).toDouble }

    val ramp = ColorRamp(Array[Int](0x003000ff, 0x1f4f14ff, 0x648744ff, 0x94c11cff, 0xc1f203ff,
                                    0xf1ff9fff, 0xf9e4e3ff, 0xca9196ff, 0x996561ff, 0x8e2612ff))
    val brks = Array(1, 10, 100, 1000, 10000, 20000, 30000, 40000, 50000, 60000)
    cmap = ColorMap(brks, ramp)

  }

  def ndvi(nir: Tile, red: Tile): Tile = (nir - red) / (nir + red)

  def evi(nir: Tile, red: Tile, blue: Tile): Tile =
    ((nir - red) / (nir + (red * 6) - (blue * 7.5) + 1)) * 2.5

  @Benchmark
  def ndviB: Tile = ndvi(nir, r)
  @Benchmark
  def eviB: Tile = evi(nir, r, b)

  @Benchmark
  def ndviPng: Png = ndvi(nir, r).renderPng(cmap)
  @Benchmark
  def eviPng: Png = evi(nir, r, b).renderPng(cmap)

}
