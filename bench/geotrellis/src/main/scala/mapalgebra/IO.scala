package mapalgebra

import java.util.concurrent.TimeUnit

import geotrellis.raster.{ Tile, MultibandTile }
import geotrellis.raster.io.geotiff.{ SinglebandGeoTiff, MultibandGeoTiff }
import org.openjdk.jmh.annotations._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class IOBench {

  @Benchmark
  def singleband: Tile = SinglebandGeoTiff("/home/colin/code/haskell/mapalgebra/data/gray512.tif").tile
  // def singleband: Tile = SinglebandGeoTiff("../data/gray512.tif").tile
  @Benchmark
  def multiband: MultibandTile = MultibandGeoTiff("/home/colin/code/haskell/mapalgebra/data/512x512.tif").tile
  // def multiband: MultibandTile = MultibandGeoTiff("../data/512x512.tif").tile

}
