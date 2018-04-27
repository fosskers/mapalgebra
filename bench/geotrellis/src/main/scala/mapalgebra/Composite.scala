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
  var cmap: ColorMap = _

  /* 512x512 */
  var r9: Tile = _
  var nir9: Tile = _
  var b9: Tile = _

  /* 1024x1024 */
  var r10: Tile = _
  var nir10: Tile = _
  var b10: Tile = _

  /* 2048x2048 */
  var r11: Tile = _
  var nir11: Tile = _
  var b11: Tile = _

  /* 4096x4096 */
  var r12: Tile = _
  var nir12: Tile = _
  var b12: Tile = _

  @Setup
  def setup(): Unit = {
    r9   = DoubleArrayTile.empty(512, 512).mapDouble { (c, r, _) => (c * r).toDouble }
    nir9 = DoubleArrayTile.empty(512, 512).mapDouble { (c, r, _) => (c * r + 1).toDouble }
    b9   = DoubleArrayTile.empty(512, 512).mapDouble { (c, r, _) => (c * r + 2).toDouble }

    r10   = DoubleArrayTile.empty(1024, 1024).mapDouble { (c, r, _) => (c * r).toDouble }
    nir10 = DoubleArrayTile.empty(1024, 1024).mapDouble { (c, r, _) => (c * r + 1).toDouble }
    b10   = DoubleArrayTile.empty(1024, 1024).mapDouble { (c, r, _) => (c * r + 2).toDouble }

    r11   = DoubleArrayTile.empty(2048, 2048).mapDouble { (c, r, _) => (c * r).toDouble }
    nir11 = DoubleArrayTile.empty(2048, 2048).mapDouble { (c, r, _) => (c * r + 1).toDouble }
    b11   = DoubleArrayTile.empty(2048, 2048).mapDouble { (c, r, _) => (c * r + 2).toDouble }

    r12   = DoubleArrayTile.empty(4096, 4096).mapDouble { (c, r, _) => (c * r).toDouble }
    nir12 = DoubleArrayTile.empty(4096, 4096).mapDouble { (c, r, _) => (c * r + 1).toDouble }
    b12   = DoubleArrayTile.empty(4096, 4096).mapDouble { (c, r, _) => (c * r + 2).toDouble }

    val ramp = ColorRamp(Array[Int](0x003000ff, 0x1f4f14ff, 0x648744ff, 0x94c11cff, 0xc1f203ff,
                                    0xf1ff9fff, 0xf9e4e3ff, 0xca9196ff, 0x996561ff, 0x8e2612ff))
    val brks = Array(1, 10, 100, 1000, 10000, 20000, 30000, 40000, 50000, 60000)
    cmap = ColorMap(brks, ramp)

  }

  def ndvi(nir: Tile, red: Tile): Tile = (nir - red) / (nir + red)

  def evi(nir: Tile, red: Tile, blue: Tile): Tile =
    ((nir - red) / (nir + (red * 6) - (blue * 7.5) + 1)) * 2.5

  @Benchmark
  def ndviB: Tile = ndvi(nir9, r9)
  @Benchmark
  def eviB9: Tile = evi(nir9, r9, b9)
  @Benchmark
  def eviB10: Tile = evi(nir10, r10, b10)
  @Benchmark
  def eviB11: Tile = evi(nir11, r11, b11)
  @Benchmark
  def eviB12: Tile = evi(nir12, r12, b12)

  @Benchmark
  def ndviPng: Png = ndvi(nir9, r9).renderPng(cmap)
  @Benchmark
  def eviPng: Png = evi(nir9, r9, b9).renderPng(cmap)

}
