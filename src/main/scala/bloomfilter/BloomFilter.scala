package bloomfilter

import chisel3._
import chisel3.util._

import scala.math.BigInt
import javax.sql.rowset.spi.SyncResolver

abstract class HashFunc(val input_width: Int, val hash_width: Int) {

    def hash_scala(input: BigInt): BigInt
    def hash_chisel(input: UInt): UInt
}


class HashFunc_Modulo(input_width: Int, hash_width: Int) extends HashFunc(input_width, hash_width) {
    def hash_scala(input: BigInt) = {
        input % hash_width
    }

    def hash_chisel(input: UInt) = {
        input % (hash_width.U)
    }
}

class HashFunc_Modulo2(input_width: Int, hash_width: Int) extends HashFunc(input_width, hash_width) {
    // Test use only
    def hash_scala(input: BigInt) = {
        input % hash_width + 1
    }

    def hash_chisel(input: UInt) = {
        input % (hash_width.U) + 1.U
    }
}

class HashFunc_FNV1A(input_width: Int, hash_width: Int) extends HashFunc(input_width, hash_width) {
    // FNV1A works with bytes
    require(input_width % 8 == 0)

    // algorithm fnv-1a is
    // hash := FNV_offset_basis

    // for each byte_of_data to be hashed do
    //     hash := hash XOR byte_of_data
    //     hash := hash × FNV_prime

    // return hash

    
    // magic number for 32 bit fnv
    var fnvPrime = BigInt("1000193", 16)
    var fnvOffsetBasis = BigInt("811c9dc5", 16)

    def hash_scala(input: BigInt) = {
        var hash = fnvOffsetBasis.toInt
        val num_of_bytes = input_width / 8

        (0 until num_of_bytes).foreach { i => {
            val k = ((input >> (i * 8)) & BigInt(0xFF)).toInt
            hash = hash ^ k
            hash = hash * (fnvPrime.toInt)
        }}

        // pick up lower bits
        hash & ((1 << hash_width) - 1)
    }

    def hash_chisel(input: UInt) = {
        var hash = fnvOffsetBasis.U
        val num_of_bytes = input_width / 8

        (0 until num_of_bytes).foreach { i => {
            val k = input(8 * (i+1) - 1, 8 * i)
            hash = hash ^ k
            hash = hash * fnvPrime.U
        }}

        hash(hash_width - 1, 0)
    }
}

class HashFunc_MurMur3(input_width: Int, hash_width: Int, val seed: Int) extends HashFunc(input_width, hash_width) {

    // murmur 3 yields a 32 or 128 bits value
    // This implementation is 32-bit version and pick up lower bits
    require(hash_width <= 32)


    def rotl32_scala(x: Int, r: Int) = {
        // x: uint32, r: int8
        (x << r) | (x >> (32 - r))
    }

    def intReverseEndian(i: Int) = {
        val i0 = i & 0xFF
        val i1 = (i >> 8) & 0xFF
        val i2 = (i >> 16) & 0xFF
        val i3 = (i >> 24)

        (i0 << 24) | (i1 << 16) | (i2 << 8) | i3
    }


    def hash_scala(input: BigInt) = {
        //

        var h = seed;

        val c1 = 0xcc9e2d51
        val c2 = 0x1b873593

        val r1 = 15
        val r2 = 13

        val m = 5
        val n = 0xe6546b64

        val num_of_blocks = input_width / 32

        (0 until num_of_blocks).foreach { i => {
            var k = ((input >> (i * 32)) & BigInt("FFFFFFFF", 16)).toInt
            k = k * c1
            k = rotl32_scala(k, r1)
            k = k * c2

            h = h ^ k
            h = rotl32_scala(h, r2)
            h = h * m + n
        }}

        // tail
        if (num_of_blocks * 32 != input_width) {
            val tail_bits = (input >> (num_of_blocks * 32)).toInt
            val tail_bytes = (input_width % 32 + 7) / 8

            var k = intReverseEndian(tail_bits)
            k = k * c1
            k = rotl32_scala(k, r1)
            k = k * c2

            h = h ^ k
        }

        h = h ^ input_width

        h = h ^ (h >> 16)
        h = h * 0x85ebca6b
        h = h ^ (h >> 13)
        h = h * 0xc2b2ae35
        h = h ^ (h >> 16)
        
        // pick up lower bits
        val ret = h & ((1 << hash_width) - 1)
        ret
    }




    def rotl32_chisel(x: UInt, r: UInt) = {
        // x: uint32, r: int8
        (x << r) | (x >> (32.U - r))
    }

    def intReverseEndian_chisel(i: UInt) = {
        Reverse(i)
    }


    def hash_chisel(input: UInt) = {
        //

        var h = seed.U;

        val c1 = BigInt("cc9e2d51", 16).U
        val c2 = BigInt("1b873593", 16).U

        val r1 = 15.U
        val r2 = 13.U

        val m = 5.U
        val n = BigInt("e6546b64", 16).U

        val num_of_blocks = input_width / 32

        (0 until num_of_blocks).foreach { i => {
            var k = input(32 * (i+1) - 1, 32 * i)
            k = k * c1
            k = rotl32_chisel(k, r1)
            k = k * c2

            h = h ^ k
            h = rotl32_chisel(h, r2)
            h = h * m + n
        }}

        // tail
        if (num_of_blocks * 32 != input_width) {
            val tail_bits = input(input_width - 1, 32 * num_of_blocks)
            val tail_bytes = (input_width % 32 + 7) / 8

            var k = intReverseEndian_chisel(tail_bits)
            k = k * c1
            k = rotl32_chisel(k, r1)
            k = k * c2

            h = h ^ k
        }

        h = h ^ input_width.U

        h = h ^ (h >> 16.U)
        h = h * BigInt("85ebca6b", 16).U
        h = h ^ (h >> 13.U)
        h = h * BigInt("c2b2ae35", 16).U
        h = h ^ (h >> 16.U)
        
        // pickup lower bits
        h(hash_width - 1, 0)
    }

}

class BloomFilterCmds extends Bundle {
    val lookup = Bool()
    val insert = Bool()
    val clear = Bool()
}


case class BloomFilterParams(val hash_funcs: Seq[HashFunc], val data_width: Int, val array_size: Int) {
    val n_hash_funcs = hash_funcs.length
    val hash_width = hash_funcs.head.hash_width
    val hash_input_width = hash_funcs.head.input_width

    require(hash_input_width == data_width)
    require(data_width % 32 == 0)
    require(array_size > 0)
    require(n_hash_funcs > 0)
    require(array_size > n_hash_funcs)
}


class BloomFilter(p: BloomFilterParams) extends Module {

    val io = IO(new Bundle {
        val in = Flipped(Decoupled(new Bundle {
            val data = UInt(p.data_width.W)
            val cmd = Input(new BloomFilterCmds())
        }))

        val out = Valid(new Bundle {
            val exists = Bool()
        })
    })

    val mem = SyncReadMem(p.array_size, Bool())

    val s2_idle :: s2_lookup :: s2_insert :: s2_clear :: Nil = Enum(4)
    val s1_cmd = RegInit(UInt(), s2_idle)

    val mem_clear_counter = new Counter(p.array_size)
    val (mem_clear_counter_value, mem_clear_counter_wrap) = Counter(s1_cmd === s2_clear, p.array_size)

    val mem_read_en = io.in.fire && io.in.bits.cmd.lookup

    val hash_result = Wire(Vec(p.n_hash_funcs, UInt(p.hash_width.W)))
    val mem_read_result = Wire(Vec(p.n_hash_funcs, Bool()))

    (0 until p.n_hash_funcs).foreach { i => { 
        hash_result(i) := p.hash_funcs(i).hash_chisel(io.in.bits.data) 
        mem_read_result(i) := mem.read(hash_result(i), mem_read_en)
    }}

    io.in.ready := (s1_cmd =/= s2_clear)
    io.out.valid := false.B
    io.out.bits.exists := false.B


    // pipeline stage 1
    // Accept req and calculate hash, then send out mem read/write request (n port)

    when (io.in.fire) {
        when (io.in.bits.cmd.lookup) {
            // lookup
            s1_cmd := s2_lookup

        } .elsewhen (io.in.bits.cmd.insert) {
            // insert
            s1_cmd := s2_insert
            (0 until p.n_hash_funcs).foreach {
                i => { mem.write(hash_result(i), true.B) }
            }
        } .elsewhen (io.in.bits.cmd.clear) {
            // clear
            s1_cmd := s2_clear
        } .otherwise {
            s1_cmd := s2_idle
        }
    } .otherwise {
        when (s1_cmd === s2_clear) {
            mem.write(mem_clear_counter_value, false.B)
            when(mem_clear_counter_wrap) {
                s1_cmd := s2_idle
            }
        } .otherwise {
            s1_cmd := s2_idle
        }
    }

    // pipeline stage 2
    // Retrive data, return result

    when (s1_cmd === s2_lookup) {
        val value_exists = mem_read_result.reduce(_ && _)
        io.out.valid := true.B 
        io.out.bits.exists := value_exists
    } .elsewhen (s1_cmd === s2_insert) {
        // insert complete
        // Politely toggle valid bit
        io.out.valid := true.B
    } .elsewhen (s1_cmd === s2_idle) {
        when (RegNext(s1_cmd) === s2_clear) {
            // clear complete
            // Politely toggle valid bit
            io.out.valid := true.B
        }
    }
}