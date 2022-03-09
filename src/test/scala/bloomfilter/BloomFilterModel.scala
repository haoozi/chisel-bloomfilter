package bloomfilter

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt


class BloomFilterModel(p: BloomFilterParams) {
    // limit hash width for ArrayBuffer read
    require(p.hash_width < 32)
    val mem = ArrayBuffer.fill(p.array_size)(false)

    def lookup(data: BigInt) = {
        p.hash_funcs.map({ func => mem(func.hash_scala(data).toInt) }) reduce (_ && _)
    }

    def insert(data: BigInt) = {
        p.hash_funcs.map({ func => func.hash_scala(data).toInt }).foreach { i => {mem(i) = true}}
    }

    def clear() = {
        (0 until p.array_size).foreach{ mem(_) = false }
    }
}