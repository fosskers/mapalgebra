package mapalgebra

import java.util.concurrent.TimeUnit

import geotrellis.raster._
import org.openjdk.jmh.annotations._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class LocalBench {
  var r: Tile = _
  var g: Tile = _
  var b: Tile = _

  @Setup
  def setup(): Unit = {
    // throws during benchmarks, but not in console
    // val tiles: Vector[Tile] = MultibandGeoTiff.streaming("../data/512x512.tif").tile.bands
    r = IntArrayTile.empty(512, 512).map { (c, r, _) => c * r }
    g = IntArrayTile.empty(512, 512).map { (c, r, _) => c * r + 1 }
    b = IntArrayTile.empty(512, 512).map { (c, r, _) => c * r + 2 }
  }

  @Benchmark
  def mapInt: Tile = r.map(_ + 17)
  @Benchmark
  def mapDouble: Tile = r.mapDouble(_ + 17)

  @Benchmark
  def adding: Tile = r + g
  @Benchmark
  def division: Tile = r / g

  @Benchmark
  def max: Tile = r.localMax(g)
  @Benchmark
  def min: Tile = r.localMin(g)

  @Benchmark
  def mean: Tile = (r + g + b) / 3

  @Benchmark
  def majority: Tile = r.localMajority(g, b)
  @Benchmark
  def minority: Tile = r.localMinority(g, b)

}
