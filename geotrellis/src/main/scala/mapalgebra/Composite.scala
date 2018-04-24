package mapalgebra

import java.util.concurrent.TimeUnit

import geotrellis.raster._
import org.openjdk.jmh.annotations._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class CompositeBench {
  var r: Tile = _
  var nir: Tile = _
  var b: Tile = _

  @Setup
  def setup(): Unit = {
    r   = IntArrayTile.empty(512, 512).map { (c, r, _) => c * r }
    nir = IntArrayTile.empty(512, 512).map { (c, r, _) => c * r + 1 }
    b   = IntArrayTile.empty(512, 512).map { (c, r, _) => c * r + 2 }
  }

  def ndvi(nir: Tile, red: Tile): Tile = (nir - red) / (nir + red)

  def evi(nir: Tile, red: Tile, blue: Tile): Tile =
    ((nir - red) / (nir + (red * 6) - (blue * 7.5) + 1)) * 2.5

  @Benchmark
  def ndviB: Tile = ndvi(nir, r)
  @Benchmark
  def eviB: Tile = evi(nir, r, b)

}
