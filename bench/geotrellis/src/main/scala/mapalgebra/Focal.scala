package mapalgebra

import java.util.concurrent.TimeUnit

import geotrellis.raster._
import geotrellis.raster.mapalgebra.focal.Square
import org.openjdk.jmh.annotations._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class FocalBench {
  var tile: Tile = _
  // var huge: Tile = _

  @Setup
  def setup(): Unit = {
    tile = IntArrayTile.empty(512, 512).map { (c, r, _) => c * r }
    // huge = UByteArrayTile.empty(46500, 46500).map { (c, r, _) => 5 }  // NegativeArraySizeException
  }

  @Benchmark
  def sum: Tile = tile.focalSum(Square(1))
  // @Benchmark
  // def sumHuge: Tile = huge.focalSum(Square(1))
  @Benchmark
  def mean: Tile = tile.focalMean(Square(1))
  @Benchmark
  def max: Tile = tile.focalMax(Square(1))
  @Benchmark
  def min: Tile = tile.focalMin(Square(1))
  @Benchmark
  def aspect: Tile = tile.aspect(CellSize(1, 1))
  @Benchmark
  def gradient: Tile = tile.slope(CellSize(1, 1))

}
