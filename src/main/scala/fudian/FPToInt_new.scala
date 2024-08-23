package fudian

import chisel3._
import chisel3.util.Mux1H
import chisel3.util.Cat
import javax.sound.midi.Sequence
import fudian.FloatPoint.expBias
import fudian.utils.ShiftRightJam
import chisel3.util.Fill

class FPToInt_new extends Module{
    val io = IO(new Bundle(){
        val src1 = Input(UInt(64.W))
        val src2 = Input(UInt(64.W))
        val tagIn = Input(UInt(2.W))
        val fmt = Input(UInt(2.W))
        val op = Input(UInt(2.W))
        val wflag = Input(Bool())
        val fcvt = Input(Bool())
        val rm = Input(UInt(3.W))
        val data = Output(UInt(64.W))
        val fflags = Output(UInt(5.W))
    })

    val (src1, src2, tagIn, fmt, op, wflag, fcvt, rm) =
        (io.src1, io.src2, io.tagIn, io.fmt, io.op, io.wflag, io.fcvt, io.rm)
// (tagIn,op)
// input/output    wu          w           lu          l
// f16          (10,00)     (10,01)     (10,10)     (10,11)
// f32          (00,00)     (00,01)     (00,10)     (00,11)
// f64          (01,00)     (01,01)     (01,10)     (01,11)

    val isFPCvtInt = WireDefault(false.B)
    isFPCvtInt := wflag && fcvt

    val isFPClassify = WireDefault(false.B)
    isFPClassify := !wflag && !fcvt && rm === 1.U

    val isFPMv = WireDefault(false.B)
    isFPMv := !wflag && !fcvt && rm === 0.U

    val inputIsF16 = WireDefault(false.B)
    inputIsF16 := tagIn === 2.U
    val inputIsF32 = WireDefault(false.B)
    inputIsF32 := tagIn === 0.U
    val inputIsF64 = WireDefault(false.B)
    inputIsF64 := tagIn === 1.U

    val outputIsWU = WireDefault(false.B)
    outputIsWU := op === 0.U
    val outputIsW = WireDefault(false.B)
    outputIsW := op === 1.U
    val outputIsLU = WireDefault(false.B)
    outputIsLU := op === 2.U
    val outputIsL = WireDefault(false.B)
    outputIsL := op === 3.U

// ******************************************************************************
// fp format
// ******************************************************************************
    val fpSrc1 = Wire(UInt(64.W))
    fpSrc1 := Mux1H(
        Seq(
            tagIn === 2.U && !src1(63,16).andR,
            tagIn === 2.U && src1(63,16).andR,
            tagIn === 0.U && !src1(63,32).andR,
            tagIn === 0.U && src1(63,32).andR,
            tagIn === 1.U
        ),
        Seq(
            Cat(~0.U(48.W), 0.U(1.W), ~0.U(6.W), 0.U(9.W)),
            src1,
            Cat(~0.U(32.W), 0.U(1.W), ~0.U(9.W), 0.U(22.W)),
            src1,
            src1
        )
    )
    // assume NaN Boxing
    val rawFpSrc1 = Wire(UInt(65.W))
    rawFpSrc1 := Mux1H(
        Seq(
            // invaild
            tagIn === 2.U && !src1(63,16).andR,
            tagIn === 0.U && !src1(63,32).andR,
            // expIsNotZero
            tagIn === 2.U && src1(63,16).andR && fpSrc1(14,10).orR,
            // expIsZero
            tagIn === 2.U && src1(63,16).andR && !fpSrc1(14,10).orR,
            tagIn === 0.U && src1(63,32).andR && fpSrc1(30,23).orR,
            tagIn === 0.U && src1(63,32).andR && !fpSrc1(30,23).orR,
            tagIn === 1.U && src1(62,52).orR,
            tagIn === 1.U && !src1(62,52).orR
        ),
        Seq(
            Cat(~0.U(48.W), 0.U(1.W), ~0.U(7.W), 0.U(9.W)),
            Cat(~0.U(32.W), 0.U(1.W), ~0.U(10.W), 0.U(22.W)),
            Cat(src1(63,10), 1.U(1.W), src1(9,0)),
            Cat(src1(63,11), 1.U(1.W), 0.U(1.W), src1(9,0)),
            Cat(src1(63,23), 1.U(1.W), src1(22,0)),
            Cat(src1(63,24), 1.U(1.W), 0.U(1.W), src1(22,0)),
            Cat(src1(63,52), 1.U(1.W), src1(51,0)),
            Cat(src1(63,53), 1.U(1.W), 0.U(1.W), src1(51,0))
        )
    )

    val fpSrc2 = Wire(UInt(64.W))
    fpSrc2 := Mux1H(
        Seq(
            tagIn === 2.U && !src2(63,16).andR,
            tagIn === 2.U && src2(63,16).andR,
            tagIn === 0.U && !src2(63,32).andR,
            tagIn === 0.U && src2(63,32).andR,
            tagIn === 1.U
        ),
        Seq(
            Cat(~0.U(48.W), 0.U(1.W), ~0.U(6.W), 0.U(9.W)),
            src2,
            Cat(~0.U(32.W), 0.U(1.W), ~0.U(9.W), 0.U(22.W)),
            src2,
            src2
        )
    )
// ******************************************************************************
// fp arguments this step's combinatorial logic can be improve
// ******************************************************************************
    val fpSrc1Sign = Wire(Bool())
    fpSrc1Sign := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc1(15),
            fpSrc1(31),
            fpSrc1(63)
        )
    )
    val fpSrc2Sign = Wire(Bool())
    fpSrc2Sign := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc2(15),
            fpSrc2(31),
            fpSrc2(63)
        )
    )
    val fpSrc1Exp = Wire(UInt(11.W))
    fpSrc1Exp := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(0.U(6.W), fpSrc1(14,10)),
            Cat(0.U(3.W), fpSrc1(30,23)),
            fpSrc1(62,52)
        )
    )
    val rawFpSrc1Exp = Wire(UInt(11.W))
    rawFpSrc1Exp := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(0.U(6.W), rawFpSrc1(15,11)),
            Cat(0.U(3.W), rawFpSrc1(31,24)),
            rawFpSrc1(63,53)
        )
    )
    val rawFpSrc1Sig = Wire(UInt(53.W))
    rawFpSrc1Sig := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(0.U(42.W), rawFpSrc1(10,0)),
            Cat(0.U(29.W), rawFpSrc1(23,0)),
            rawFpSrc1(52,0)
        )
    )
    val fpSrc1Sig = Wire(UInt(52.W))
    fpSrc1Sig := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(0.U(42.W), fpSrc1(9,0)),
            Cat(0.U(29.W), fpSrc1(22,0)),
            fpSrc1(51,0)
        )
    )
    val fpSrc1SigPutHigh = Wire(UInt(52.W))
    fpSrc1SigPutHigh := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(fpSrc1(9,0), 0.U(42.W)),
            Cat(fpSrc1(22,0), 0.U(29.W)),
            fpSrc1(51,0)
        )
    )

    // exp limit 
    val fpSrc1ExpIsOnes = Wire(Bool())
    fpSrc1ExpIsOnes := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc1(14,10).andR,
            fpSrc1(30,23).andR,
            fpSrc1(62,52).andR
        )
    )
    val fpSrc1ExpIsZero = Wire(Bool())
    fpSrc1ExpIsZero := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            !fpSrc1(14,10).orR,
            !fpSrc1(30,23).orR,
            !fpSrc1(62,52).orR
        )
    )
    val fpSrc1ExpNotZero = Wire(Bool())
    fpSrc1ExpNotZero := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc1(14,10).orR,
            fpSrc1(30,23).orR,
            fpSrc1(62,52).orR
        )
    )
    // sigment limit
    val fpSrc1SigNotZero = Wire(Bool())
    fpSrc1SigNotZero := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc1(9,0).orR,
            fpSrc1(22,0).orR,
            fpSrc1(51,0).orR
        )
    )
    // NaN
    val fpSrc1IsNaN = Wire(Bool())
    fpSrc1IsNaN := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc1(14,10).andR && fpSrc1(9,0).orR,
            fpSrc1(30,23).andR && fpSrc1(22,0).orR,
            fpSrc1(62,52).andR && fpSrc1(51,0).orR
        )
    )
    val fpSrc2IsNaN = Wire(Bool())
    fpSrc2IsNaN := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc2(14,10).andR && fpSrc2(9,0).orR,
            fpSrc2(30,23).andR && fpSrc2(22,0).orR,
            fpSrc2(62,52).andR && fpSrc2(51,0).orR
        )
    )
    val fpSrc1IsSNaN = Wire(Bool())
    fpSrc1IsSNaN := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc1(14,10).andR && !fpSrc1(9) && fpSrc1(8,0).orR,
            fpSrc1(30,23).andR && !fpSrc1(22) && fpSrc1(21,0).orR,
            fpSrc1(62,52).andR && !fpSrc1(51) && fpSrc1(50,0).orR
        )
    )
    val fpSrc2IsSNaN = Wire(Bool())
    fpSrc2IsSNaN := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            fpSrc2(14,10).andR && !fpSrc2(9) && fpSrc2(8,0).orR,
            fpSrc2(30,23).andR && !fpSrc2(22) && fpSrc2(21,0).orR,
            fpSrc2(62,52).andR && !fpSrc2(51) && fpSrc2(50,0).orR
        )
    )
    // Zero
    val fpSrc1IsZero = Wire(Bool())
    fpSrc1IsZero := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            !fpSrc1(14,0).orR,
            !fpSrc1(30,0).orR,
            !fpSrc1(62,0).orR
        )
    )
    val fpSrc2IsZero = Wire(Bool())
    fpSrc2IsZero := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            !fpSrc2(14,0).orR,
            !fpSrc2(30,0).orR,
            !fpSrc2(62,0).orR
        )
    )
    // Subnormal
    val fpSrc1IsSubnormal = Wire(Bool())
    fpSrc1IsSubnormal := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            !fpSrc1(14,10).orR && fpSrc1(9,0).orR,
            !fpSrc1(30,23).orR && fpSrc1(22,0).orR,
            !fpSrc1(62,52).orR && fpSrc1(51,0).orR
        )
    )
// ******************************************************************************
// FPToInt state1 
// ******************************************************************************
    val isSignedInt = io.op(0)
    val isLongInt = io.op(1)
    val maxIntExp = WireDefault(0.U(11.W))
    maxIntExp := Mux1H(
        Seq(
            // F16 not overflow
            inputIsF16 && isLongInt,
            inputIsF16 && !isLongInt,
            inputIsF32 && isLongInt,
            inputIsF32 && !isLongInt,
            inputIsF64 && isLongInt,
            inputIsF64 && !isLongInt
        ),
        Seq(
            // expBias + Mux(isLongInt, 63, 31)
            Cat(0.U(4.W), 1.U(1.W), 0.U(2.W), ~0.U(3.W), 0.U(1.W)),
            Cat(0.U(5.W), 1.U(1.W), 0.U(1.W), ~0.U(3.W), 0.U(1.W)),
            Cat(0.U(3.W), 1.U(1.W), 0.U(1.W), ~0.U(5.W), 0.U(1.W)),
            Cat(0.U(3.W), 1.U(1.W), 0.U(2.W), ~0.U(4.W), 0.U(1.W)),
            Cat(1.U(1.W), 0.U(4.W), ~0.U(5.W), 0.U(1.W)),
            Cat(1.U(1.W), 0.U(5.W), ~0.U(4.W), 0.U(1.W))
        )
    )
    val minLiftShiftExp = WireDefault(0.U(11.W))
    minLiftShiftExp := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            // expBias + sigWidth
            Cat(0.U(6.W), ~0.U(2.W), 0.U(2.W), 1.U(1.W)),
            Cat(0.U(3.W), 1.U(1.W), 0.U(2.W), 1.U(1.W), 0.U(1.W), ~0.U(2.W), 0.U(1.W)),
            Cat(1.U(1.W), 0.U(4.W), ~0.U(2.W), 0.U(2.W), ~0.U(2.W))
        )
    )
    val expOf = WireDefault(false.B)
    expOf := rawFpSrc1Exp > maxIntExp

    // **********************************
    // lift shift path
    // **********************************
    val lpathShamt = rawFpSrc1Exp - minLiftShiftExp

    val lpathIv = !isSignedInt && fpSrc1Sign
    val lpathMayOf = isSignedInt && (rawFpSrc1Exp === maxIntExp)
    val lpathPosOf = lpathMayOf && !fpSrc1Sign
    val lpathNegOf = lpathMayOf && fpSrc1Sign && fpSrc1Sig.orR
    val lpathOf = lpathPosOf || lpathNegOf

    val ivSelMax = fpSrc1IsNaN || !fpSrc1Sign

    // **********************************
    // right shift path
    // **********************************
    val rpathShamt = WireDefault(0.U(11.W))
    rpathShamt := minLiftShiftExp - rawFpSrc1Exp

    // **********************************
    // select path
    // **********************************
    val selLpath = WireDefault(false.B)
    selLpath := rawFpSrc1Exp >= minLiftShiftExp

// ******************************************************************************
// FPToInt state2
// ******************************************************************************
    // **********************************
    // lift shift path
    // **********************************
    val lpathSigShifted = WireDefault(0.U(64.W))
    lpathSigShifted := (rawFpSrc1Sig << lpathShamt)(63, 0)


    // **********************************
    // right shift path
    // **********************************
    val (rpathSigShifted, rpathSitcky) = ShiftRightJam(
        Cat(rawFpSrc1Sig, 0.U(1.W)),
        rpathShamt
    )
    val rpathRounder = Module(new RoundingUnit(53))
    rpathRounder.io.in := rpathSigShifted.head(53)
    rpathRounder.io.roundIn := rpathSigShifted(0)
    rpathRounder.io.stickyIn := rpathSitcky
    rpathRounder.io.signIn := fpSrc1Sign
    rpathRounder.io.rm := rm
    val rpathSig = WireDefault(0.U(64.W))
    rpathSig := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(0.U(41.W), rpathRounder.io.cout, rpathRounder.io.out(10,0)),
            Cat(0.U(28.W), rpathRounder.io.cout, rpathRounder.io.out(23,0)),
            Cat(0.U(10.W), rpathRounder.io.cout, rpathRounder.io.out(52,0))
        )
    )
    val rpathIx = rpathRounder.io.inexact
    val rpathIv = !isSignedInt && fpSrc1Sign && rpathSig.orR
    val rpathExpInc = rpathRounder.io.r_up && rpathRounder.io.in(30, 0).andR
    val rpathExpEq31 = rawFpSrc1Exp === (1023 + 31).U
    val rpathExpEq30 = rawFpSrc1Exp === (1023 + 30).U
    val rpathPosOf = !fpSrc1Sign && Mux(
        isSignedInt,
        rpathExpEq31 || (rpathExpEq30 && rpathExpInc),
        rpathExpEq31 && rpathExpInc
    )
    val rpathNegOf = fpSrc1Sign && rpathExpEq31 && (rpathRounder.io.in(30, 0).orR || rpathRounder.io.r_up)
    val rpathOf = Mux1H(
        Seq(
            inputIsF64,
            !inputIsF64
        ),
        Seq(
            !isLongInt && (rpathPosOf || rpathNegOf),
            false.B
        )
    )

    // **********************************
    // select path
    // **********************************
    val of = expOf || (selLpath && lpathOf) || (!selLpath && rpathOf)
    val iv = fpSrc1ExpIsOnes || of || (selLpath && lpathIv) || (!selLpath && rpathIv)
    val ix = !iv && !selLpath && rpathIx

    val intAbs = Mux(selLpath, lpathSigShifted, rpathSig)
    val int = Mux(fpSrc1Sign && isSignedInt, -intAbs, intAbs) | Cat(Fill(32, outputIsWU && intAbs(31)), 0.U(32.W))

    val maxInt64 = Cat(!isSignedInt, ~0.U(63.W))
    val minInt64 = Cat(isSignedInt, 0.U(63.W))
    val maxInt32 = Cat(Fill(32, maxInt64.head(1)), maxInt64.head(32))
    val minInt32 = Cat(Fill(32, minInt64.head(1)), minInt64.head(32))

    val resultFPCvtInt = WireDefault(0.U(64.W))
    resultFPCvtInt := Mux(
        iv,
        Mux(
            ivSelMax,
            Mux(isLongInt, maxInt64, maxInt32),
            Mux(isLongInt, minInt64, minInt32)
        ),
        int
    )
    val fflagsFPCvtInt = WireDefault(0.U(5.W))
    fflagsFPCvtInt := Cat(iv, false.B, false.B, false.B, ix)
// ******************************************************************************
// FPClassify
// ******************************************************************************
    val resultFPClassify = WireDefault(0.U(64.W))
    resultFPClassify := Cat(
        // bit9 qNaN
        fpSrc1IsNaN && !fpSrc1IsSNaN,
        // bit8 sNaN
        fpSrc1IsSNaN,
        // bit7 positive INF
        !fpSrc1Sign && fpSrc1ExpIsOnes && !fpSrc1SigNotZero,
        // bit6 positive normal number
        !fpSrc1Sign && !fpSrc1ExpIsOnes && fpSrc1ExpNotZero,
        // bit5 positive subnormal number
        !fpSrc1Sign && fpSrc1ExpIsZero && fpSrc1SigNotZero,
        // bit4 +0
        !fpSrc1Sign && fpSrc1ExpIsZero && !fpSrc1SigNotZero,
        // bit3 -0
        fpSrc1Sign && fpSrc1ExpIsZero && !fpSrc1SigNotZero,
        // bit2 nagetive subnormal number
        fpSrc1Sign && fpSrc1ExpIsZero && fpSrc1SigNotZero,
        // bit1 nagetive normal number
        fpSrc1Sign && !fpSrc1ExpIsOnes && fpSrc1ExpNotZero,
        // bit0 nagetive INF
        fpSrc1Sign && fpSrc1ExpIsOnes && !fpSrc1SigNotZero
    )
// ******************************************************************************
// FPMv
// ******************************************************************************
    val resultFPMv = WireDefault(0.U(64.W))
    resultFPMv := Mux1H(
        Seq(
            fmt === 0.U,
            fmt === 1.U,
            fmt === 2.U
        ),
        Seq(
            Cat(Fill(32, src1(31)), src1(31,0)),
            src1,
            Cat(Fill(48, src1(15)), src1(15,0))
        )
    )
// ******************************************************************************
// Final result and fflags
// ******************************************************************************
    val result = Mux1H(
        Seq(
            isFPCvtInt,
            isFPClassify,
            isFPMv
        ),
        Seq(
            resultFPCvtInt,
            resultFPClassify,
            resultFPMv
        )
    )
    val fflags = Mux1H(
        Seq(
            isFPCvtInt,
            isFPClassify || isFPMv
        ),
        Seq(
            fflagsFPCvtInt,
            0.U(5.W)
        )
    )
    io.data := result
    io.fflags := fflags
}