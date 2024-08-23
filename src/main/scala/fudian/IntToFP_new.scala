package fudian

import chisel3._
import chisel3.WireDefault
import chisel3.util.Cat
import chisel3.util.Fill
import fudian.utils.CLZ
import chisel3.util.Mux1H
import firrtl.PrimOps.Mul
import chisel3.util.RegEnable

class IntToFP_new extends Module{
    val io = IO(new Bundle(){
        val src1 = Input(UInt(64.W))
        val tagOut = Input(UInt(2.W))
        val wflag = Input(Bool())
        val typ = Input(UInt(2.W))
        val rm = Input(UInt(3.W))
        val data = Output(UInt(64.W))
        val fflags = Output(UInt(5.W))
        val regEnable = Input(Bool())
    })

    val (src1, tagOut, wflag, typ, rm, regEnable) =
        (io.src1, io.tagOut, io.wflag, io.typ, io.rm, io.regEnable)

    val src1Reg = RegEnable(src1, regEnable)
    val rmReg = RegEnable(rm, regEnable)
    
// ******************************************************************************
// IntCvtFP
// ******************************************************************************
    // **********************************
    // IntCvtFP state1
    // **********************************
    val isIntCvtFP = RegEnable(wflag, regEnable)
    val isIntMv = RegEnable(!wflag, regEnable)
    val isSignedInt = WireDefault(false.B)
    isSignedInt := ~typ(0)
    val isLongInt = WireDefault(false.B)
    isLongInt := typ(1)

    val outputIsF16 = RegEnable(tagOut === 2.U, regEnable)
    val outputIsF32 = RegEnable(tagOut === 0.U, regEnable)
    val outputIsF64 = RegEnable(tagOut === 1.U, regEnable)

    val inputIsZero = RegEnable(Mux1H(
        Seq(
            isLongInt,
            !isLongInt
        ),
        Seq(
            !src1.orR,
            !src1(31,0).orR
        )
    ), regEnable)

    val src1ToLong = WireDefault(0.U(64.W))
    src1ToLong := Mux1H(
        Seq(
            isLongInt,
            !isLongInt && isSignedInt,
            !isLongInt && !isSignedInt
        ),
        Seq(
            src1,
            Cat(Fill(32, src1(31)), src1(31,0)),
            Cat(0.U(32.W), src1(31,0))
        )
    )

    val inputSign = WireDefault(false.B)
    inputSign := isSignedInt && Mux(isLongInt, src1(63), src1(31))
    val inputSignReg = RegEnable(inputSign, regEnable)
    val inputAbs = WireDefault(0.U(64.W))
    inputAbs := Mux(inputSign, (~src1ToLong).asUInt + 1.U, src1ToLong)
    val inputAbsReg = RegEnable(inputAbs, regEnable)
    val lzc = RegEnable(CLZ(Mux(inputSign, inputAbs, src1ToLong)), regEnable)

    // **********************************
    // IntCvtFP state2
    // **********************************
    val inNorm = (inputAbsReg << lzc)(62, 0)

    val expRaw = WireDefault(0.U(11.W))
    expRaw := Mux1H(
        Seq(
            outputIsF16,
            outputIsF32,
            outputIsF64
        ),
        Seq(
            // 63 + expBias -lzc
            63.U(11.W) + 15.U(11.W) - lzc,
            63.U(11.W) + 127.U(11.W) - lzc,
            63.U(11.W) + 1023.U(11.W) - lzc
        )
    )
    val sigRaw = WireDefault(0.U(52.W))
    sigRaw := Mux1H(
        Seq(
            outputIsF16,
            outputIsF32,
            outputIsF64
        ),
        Seq(
            inNorm.head(10),
            inNorm.head(23),
            inNorm.head(52)
        )
    )
    val roundBit = WireDefault(0.U(1.W))
    roundBit := Mux1H(
        Seq(
            outputIsF16,
            outputIsF32,
            outputIsF64
        ),
        Seq(
            inNorm.tail(10).head(1),
            inNorm.tail(23).head(1),
            inNorm.tail(52).head(1)
        )
    )
    val stickyBit = WireDefault(0.U(1.W))
    stickyBit := Mux1H(
        Seq(
            outputIsF16,
            outputIsF32,
            outputIsF64
        ),
        Seq(
            inNorm.tail(11).orR,
            inNorm.tail(24).orR,
            inNorm.tail(53).orR
        )
    )

    val rounder = Module(new RoundingUnit(52))
    rounder.io.in := sigRaw
    rounder.io.roundIn := roundBit
    rounder.io.stickyIn := stickyBit
    rounder.io.signIn := inputSignReg
    rounder.io.rm := rmReg

    val of = outputIsF16 && (inputAbsReg.head(48).orR || (inputAbsReg.tail(48).head(10).andR && (rounder.io.r_up & (sigRaw(9,0).andR))))
    val ix = of || rounder.io.inexact
    val fpExp = WireDefault(0.U(11.W))
    fpExp := Mux1H(
        Seq(
            inputIsZero,
            !inputIsZero && outputIsF16,
            !inputIsZero && outputIsF32,
            !inputIsZero && outputIsF64
        ),
        Seq(
            0.U,
            expRaw + (rounder.io.r_up & (sigRaw(9,0).andR)),
            expRaw + (rounder.io.r_up & (sigRaw(22,0).andR)),
            expRaw + (rounder.io.r_up & (sigRaw(51,0).andR))
        )
    )
    val fpSig = WireDefault(0.U(52.W))
    fpSig := rounder.io.out

    val rmin = rmReg === ROD || rmReg === RTZ || (rmReg === RDN && !inputSignReg) || (rmReg === RUP && inputSignReg)
    val resultIntCvtFP = WireDefault(0.U(64.W))
    resultIntCvtFP := Mux1H(
        Seq(
            of && rmin,
            of && !rmin,
            outputIsF16 && !of,
            outputIsF32,
            outputIsF64
        ),
        Seq(
            Cat(~0.U(48.W), inputSignReg, ~0.U(4.W), 0.U(1.W), ~0.U(10.W)),
            Cat(~0.U(48.W), inputSignReg, ~0.U(5.W), 0.U(10.W)),
            Cat(~0.U(48.W), inputSignReg, fpExp(4,0), fpSig(9,0)),
            Cat(~0.U(32.W), inputSignReg, fpExp(7,0), fpSig(22,0)),
            Cat(inputSignReg, fpExp, fpSig)
        )
    )
    val fflagsIntCvtFP = WireDefault(0.U(5.W))
    fflagsIntCvtFP := Cat(0.U(2.W), of, 0.U(1.W), ix)
// ******************************************************************************
// IntMv
// ******************************************************************************
    val resultIntMv = WireDefault(0.U(64.W))
    resultIntMv := Mux1H(
        Seq(
            outputIsF16,
            outputIsF32,
            outputIsF64
        ),
        Seq(
            Cat(~0.U(48.W), src1Reg(15,0)),
            Cat(~0.U(32.W), src1Reg(31,0)),
            Cat(src1Reg)
        )
    )
// ******************************************************************************
// Final result and fflags
// ******************************************************************************
    val result = Mux1H(
        Seq(
            isIntCvtFP,
            isIntMv
        ),
        Seq(
            resultIntCvtFP,
            resultIntMv
        )
    )
    val fflags = Mux1H(
        Seq(
            isIntCvtFP,
            isIntMv
        ),
        Seq(
            fflagsIntCvtFP,
            0.U(5.W)
        )
    )
    io.data := result
    io.fflags := fflags
}