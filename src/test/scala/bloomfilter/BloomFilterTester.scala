// See README.md for license details.

package bloomfilter

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class BloomFilterTester extends AnyFlatSpec with ChiselScalatestTester {

    def bf_clear(dut: BloomFilter, array_size: Int) = {
        dut.io.in.ready.expect(true.B)

        dut.io.in.valid.poke(true.B)
        dut.io.in.bits.cmd.lookup.poke(false.B)
        dut.io.in.bits.cmd.insert.poke(false.B)
        dut.io.in.bits.cmd.clear.poke(true.B)
        dut.io.out.valid.expect(false.B)

        (0 until array_size) foreach {
            i => {
                dut.clock.step()
                dut.io.in.valid.poke(false.B)
                dut.io.in.ready.expect(false.B)
            }
        }

        dut.clock.step()
        dut.io.in.ready.expect(true.B)
        dut.io.out.valid.expect(true.B)
    }
    
    behavior of "BloomFilter"
    it should "correctly clear internal mem" in {
        val hash_funcs = Seq(new HashFunc_Modulo(32, 4), new HashFunc_Modulo2(32, 4))
        test(new BloomFilter(hash_funcs, 32, 16)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            bf_clear(dut, 16)
        }
    }


    // it should "correctly calculate imagOut onlyAdd=true" in {
    //     test(new ComplexALU(width=5, onlyAdder=true)) { dut =>
    //         for (imag1 <- -14 until 15; imag2 <- -14 until 15) {
    //             dut.io.c0.real.poke(0.S)
    //             dut.io.c0.imag.poke(imag1.S)

    //             dut.io.c1.real.poke(0.S)
    //             dut.io.c1.imag.poke(imag2.S)

    //             dut.io.out.imag.expect((imag1+imag2).S)
    //         }
    //     }
    // }

}
