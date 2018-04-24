package mapalgebra

import java.util.concurrent.TimeUnit

import geotrellis.raster.{ IntArrayTile, Tile }
import org.openjdk.jmh.annotations._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class CreationBench {

  @Benchmark
  def constant256: Tile = IntArrayTile.fill(5, 256, 256)
  @Benchmark
  def constant512: Tile = IntArrayTile.fill(5, 512, 512)
  @Benchmark
  def function256: Tile = IntArrayTile.empty(256, 256).map { (c, r, _) => c * r }
  @Benchmark
  def function512: Tile = IntArrayTile.empty(512, 512).map { (c, r, _) => c * r }

}
