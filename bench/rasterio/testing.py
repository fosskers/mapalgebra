import numpy as np
import rasterio
import timeit

# Get cell types of each band:
# {i: dtype for i, dtype in zip(dataset.indexes, dataset.dtypes)}

def f(x, y):
    return x * y

def c(x, y):
    return 5

data  = rasterio.open("512x512.tif")
red   = data.read(1)
green = data.read(2)
blue  = data.read(3)

a512  = np.fromfunction(f, (512, 512), dtype=float)
a1024 = np.fromfunction(f, (1024, 1024), dtype=float)
a2048 = np.fromfunction(f, (2048, 2048), dtype=float)
a4096 = np.fromfunction(f, (4096, 4096), dtype=float)
# huge  = np.fromfunction(c, (46500, 46500), dtype=int)  # MemoryError

# --- CREATION --- #

def constant256():
    return np.fromfunction(c, (256, 256), dtype=int)

def constant512():
    return np.fromfunction(c, (512, 512), dtype=int)

def fromFunction256():
    return np.fromfunction(f, (256, 256), dtype=int)

def fromFunction512():
    return np.fromfunction(f, (512, 512), dtype=int)

# --- IO --- #

def multiband():
    with rasterio.open("512x512.tif") as dst:
        dst.read(1)
        dst.read(2)
        dst.read(3)
        dst.read(4)

def singleband():
    with rasterio.open("gray512.tif") as dst:
        dst.read(1)

# --- LOCAL OPS --- #

def fmap17(npa):
    return npa + 17

def lsum(npa):
    return npa + npa

def ldiv(npa):
    return npa / npa

# --- COMPOSITE OPS --- #

def ndvi(nir, red):
    """Normalized Difference Vegetation Index"""
    return (nir - red) / (nir + red)

def evi(nir, red, blue):
    """Enhanced Vegetation Index"""
    return 2.5 * ((nir - red) / (nir + (6 * red) - (7.5 * blue) + 1))

if __name__ == '__main__':

    print("CREATION (ms)")
    print(timeit.timeit("constant256()", number = 50, globals = globals()) * 20)
    print(timeit.timeit("constant512()", number = 50, globals = globals()) * 20)
    print(timeit.timeit("fromFunction256()", number = 50, globals = globals()) * 20)
    print(timeit.timeit("fromFunction512()", number = 50, globals = globals()) * 20)

    print("")
    print("IO (ms)")
    print(timeit.timeit("multiband()", number = 10, globals = globals()) * 100)
    print(timeit.timeit("singleband()", number = 10, globals = globals()) * 100)

    print("")
    print("LOCAL OPS (ms)")
    print(timeit.timeit("fmap17(red)", number = 50, globals = globals()) * 20)
    print(timeit.timeit("lsum(red)", number = 50, globals = globals()) * 20)
    print(timeit.timeit("ldiv(red)", number = 50, globals = globals()) * 20)

    print("")
    print("COMPOSITE OPS (ms)")
    print(timeit.timeit("ndvi(green, red)", number = 50, globals = globals()) * 20)
    print(timeit.timeit("evi(green, red, blue)", number = 50, globals = globals()) * 20)
    print(timeit.timeit("evi(a512, a512, a512)", number = 50, globals = globals()) * 20)
    print(timeit.timeit("evi(a1024, a1024, a1024)", number = 50, globals = globals()) * 20)
    print(timeit.timeit("evi(a2048, a2048, a2048)", number = 50, globals = globals()) * 20)
    print(timeit.timeit("evi(a4096, a4096, a4096)", number = 50, globals = globals()) * 20)

    data.close
