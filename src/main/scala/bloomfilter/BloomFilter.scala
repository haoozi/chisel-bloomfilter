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
    def hash_scala(input: BigInt) = {
        input % hash_width + 1
    }

    def hash_chisel(input: UInt) = {
        input % (hash_width.U) + 1.U
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
            mem.write(mem_clear_counter_value, false.B)
            // clearing := true.B
            s1_cmd := s2_clear
        } .otherwise {
            s1_cmd := s2_idle
        }
    } .otherwise {
        when (s1_cmd === s2_clear) {
            when(mem_clear_counter_wrap) {
                s1_cmd := s2_idle
            } .otherwise {
                // Not done yet
                mem.write(mem_clear_counter_value, false.B)
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