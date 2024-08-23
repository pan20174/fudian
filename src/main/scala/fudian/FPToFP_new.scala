package fudian

import chisel3._
import chisel3.util.Mux1H
import fudian.utils.CLZ
import chisel3.util.Cat
import javax.sound.midi.Sequence
import chisel3.util.MuxLookup
import _root_.fudian.utils.ShiftRightJam

class FPToFP_new extends Module{
    val io = IO(new Bundle(){
        val src1 = Input(UInt(64.W))
        val src2 = Input(UInt(64.W))
        val tagIn = Input(UInt(2.W))
        val tagOut = Input(UInt(2.W))
        val wflag = Input(Bool())
        val fcvt = Input(Bool())
        val fpWen = Input(Bool())
        val rm = Input(UInt(3.W))
        val data = Output(UInt(64.W))
        val fflags = Output(UInt(5.W))
    })

// (tagIn,tagOut)
// input/output    f16         f32         f64 
// f16           (10,10)     (10,00)     (10,01)
// f32           (00,10)     (00,00)     (00,01)
// f64           (01,10)     (01,00)     (01,01)

    val (src1, src2, tagIn, tagOut, wflag, fcvt, fpWen, rm) =
        (io.src1, io.src2, io.tagIn, io.tagOut, io.wflag, io.fcvt, io.fpWen, io.rm)

// UpCvt f16->f32 f16->f64 f32->f64
    val isH2S = WireDefault(false.B)
    isH2S := fcvt === true.B && wflag === true.B && tagIn === 2.U && tagOut === 0.U
    val isH2D = WireDefault(false.B)
    isH2D := fcvt === true.B && wflag === true.B && tagIn === 2.U && tagOut === 1.U
    val isS2D = WireDefault(false.B)
    isS2D := fcvt === true.B && wflag === true.B && tagIn === 0.U && tagOut === 1.U
    val isUpCvt = WireDefault(false.B)
    isUpCvt := isH2S || isH2D || isS2D;

// DownCvt f32->f16 f64->f32
    val isD2S = WireDefault(false.B)
    isD2S := fcvt === true.B && tagIn === 1.U && tagOut === 0.U
    val isD2H = WireDefault(false.B)
    isD2H := fcvt === true.B && tagIn === 1.U && tagOut === 2.U
    val isS2H = WireDefault(false.B)
    isS2H := fcvt === true.B && tagIn === 0.U && tagOut === 2.U
    val isDownCvt = WireDefault(false.B)
    isDownCvt := isD2S || isD2H || isS2H;

// FpSgnj f16, f32, f64
    val isFpSgnj = WireDefault(false.B)
    isFpSgnj := fcvt === false.B && wflag === false.B && rm === 0.U
    val isFpSgnjn = WireDefault(false.B)
    isFpSgnjn := fcvt === false.B && wflag === false.B && rm === 1.U
    val isFpSgnjx = WireDefault(false.B)
    isFpSgnjx := fcvt === false.B && wflag === false.B && rm === 2.U
    val isSgnj = WireDefault(false.B)
    isSgnj := isFpSgnj || isFpSgnjn || isFpSgnjx;

// Fpmin/max f16, f32, f64
    val isFpMin = WireDefault(false.B)
    isFpMin := fcvt === false.B && wflag === true.B && rm === 0.U && fpWen === 1.U
    val isFpMax = WireDefault(false.B)
    isFpMax := fcvt === false.B && wflag === true.B && rm === 1.U && fpWen === 1.U
    val isFpMinMax = WireDefault(false.B)
    isFpMinMax := isFpMin || isFpMax;

// FpCmp f16, f32, f64
    val isFpEq = WireDefault(false.B)
    isFpEq := fcvt === false.B && wflag === true.B && rm === 2.U && fpWen === 0.U
    val isFpLt = WireDefault(false.B)
    isFpLt := fcvt === false.B && wflag === true.B && rm === 1.U && fpWen === 0.U
    val isFpLe = WireDefault(false.B)
    isFpLe := fcvt === false.B && wflag === true.B && rm === 0.U && fpWen === 0.U
    val isFpCmp = WireDefault(false.B)
    isFpCmp := isFpEq || isFpLt || isFpLe;

// ******************************************************************************
// fp format
// ******************************************************************************
    val inputIsF16 = WireDefault(false.B)
    inputIsF16 := tagIn === 2.U
    val inputIsF32 = WireDefault(false.B)
    inputIsF32 := tagIn === 0.U
    val inputIsF64 = WireDefault(false.B)
    inputIsF64 := tagIn === 1.U

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
    // exp_delta
    val expDeltaUpCvt = Wire(UInt(11.W))
    expDeltaUpCvt := Mux1H(
        Seq(
            isH2S,
            isS2D,
            isH2D
        ),
        Seq(
            Cat(0.U(4.W), ~0.U(3.W), 0.U(4.W)),
            Cat(0.U(1.W), ~0.U(3.W), 0.U(7.W)),
            Cat(0.U(1.W), ~0.U(6.W), 0.U(4.W))
        )
    )
    val expDeltaDownCvt = Wire(UInt(11.W))
    expDeltaDownCvt := Mux1H(
        Seq(
            isS2H,
            isD2S,
            isD2H
        ),
        Seq(
            Cat(0.U(4.W), ~0.U(3.W), 0.U(4.W)),
            Cat(0.U(1.W), ~0.U(3.W), 0.U(7.W)),
            Cat(0.U(1.W), ~0.U(6.W), 0.U(4.W))
        )
    )
// ******************************************************************************
// UpCvt f16->f32 f16->f64 f32->f64
// ******************************************************************************
    // **********************************
    // UpCvt Normal Path
    // **********************************
    val normalExp = WireDefault(0.U(11.W))
    normalExp := Mux1H(
        Seq(
            isH2S,
            isS2D,
            isH2D
        ),
        Seq(
            Mux1H(
                Seq(
                    fpSrc1(14),
                    !fpSrc1(14)
                ),
                Seq(
                    Cat(0.U(3.W), 1.U(1.W), 0.U(3.W), fpSrc1(13,10)),
                    Cat(0.U(3.W), 0.U(1.W), ~0.U(3.W), fpSrc1(13,10))
                )
            ),
            Mux1H(
                Seq(
                    fpSrc1(30),
                    !fpSrc1(30)
                ),
                Seq(
                    Cat(1.U(1.W), 0.U(3.W), fpSrc1(29,23)),
                    Cat(0.U(1.W), ~0.U(3.W), fpSrc1(29,23))
                )
            ),
            Mux1H(
                Seq(
                    fpSrc1(14),
                    !fpSrc1(14)
                ),
                Seq(
                    Cat(1.U(1.W), 0.U(6.W), fpSrc1(13,10)),
                    Cat(0.U(1.W), ~0.U(6.W), fpSrc1(13,10))
                )
            )
        )
    )
    // **********************************
    // UpCvt SubNormal Path
    // **********************************
    val subnormalShamt = Wire(UInt(5.W))
    subnormalShamt := CLZ(fpSrc1SigPutHigh)

    // **********************************
    // May can improve
    // **********************************
    val subnormalExp = WireDefault(0.U(11.W))
    subnormalExp := expDeltaUpCvt - subnormalShamt

    val subnormalSig = Wire(UInt(23.W))
    subnormalSig := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32
        ),
        Seq(
            Cat(0.U(13.W), (fpSrc1Sig << subnormalShamt)(8,0), 0.U(1.W)),
            Cat((fpSrc1Sig << subnormalShamt)(21,0), 0.U(1.W))
        )
    )
    // **********************************
    // UpCvt result and fflags
    // **********************************
    val resultUpCvtH2S = WireDefault(0.U(64.W))
    resultUpCvtH2S :=   Cat(
                            ~0.U(32.W),
                            !fpSrc1IsNaN && fpSrc1Sign,
                            Mux1H(
                                Seq(
                                    fpSrc1ExpIsOnes,
                                    fpSrc1IsZero,
                                    fpSrc1IsSubnormal,
                                    !fpSrc1ExpIsOnes && !fpSrc1ExpIsZero
                                ),
                                Seq(
                                    ~0.U(8.W),
                                    0.U(8.W),
                                    subnormalExp(7,0),
                                    normalExp(7,0)
                                )
                            ),
                            Mux1H(
                                Seq(
                                    fpSrc1ExpIsOnes,
                                    fpSrc1ExpIsZero,
                                    !fpSrc1ExpIsOnes && !fpSrc1ExpIsZero
                                ),
                                Seq(
                                    Cat(fpSrc1SigNotZero, 0.U(22.W)),
                                    Cat(subnormalSig(9,0), 0.U(13.W)),
                                    Cat(fpSrc1(9,0), 0.U(13.W))
                                )
                            )
                        )
    val resultUpCvtS2D = WireDefault(0.U(64.W))
    resultUpCvtS2D :=   Cat(
                            !fpSrc1IsNaN && fpSrc1Sign,
                            Mux1H(
                                Seq(
                                    fpSrc1ExpIsOnes,
                                    fpSrc1IsZero,
                                    fpSrc1IsSubnormal,
                                    !fpSrc1ExpIsOnes && !fpSrc1ExpIsZero
                                ),
                                Seq(
                                    ~0.U(11.W),
                                    0.U(11.W),
                                    subnormalExp,
                                    normalExp
                                )
                            ),
                            Mux1H(
                                Seq(
                                    fpSrc1ExpIsOnes,
                                    fpSrc1ExpIsZero,
                                    !fpSrc1ExpIsOnes && !fpSrc1ExpIsZero
                                ),
                                Seq(
                                    Cat(fpSrc1SigNotZero, 0.U(51.W)),
                                    Cat(subnormalSig, 0.U(29.W)),
                                    Cat(fpSrc1(22,0), 0.U(29.W))
                                )
                            )
                        )
    val resultUpCvtH2D = WireDefault(0.U(64.W))
    resultUpCvtH2D :=   Cat(
                            !fpSrc1IsNaN && fpSrc1Sign,
                            Mux1H(
                                Seq(
                                    fpSrc1ExpIsOnes,
                                    fpSrc1IsZero,
                                    fpSrc1IsSubnormal,
                                    !fpSrc1ExpIsOnes && !fpSrc1ExpIsZero
                                ),
                                Seq(
                                    ~0.U(11.W),
                                    0.U(11.W),
                                    subnormalExp,
                                    normalExp
                                )
                            ),
                            Mux1H(
                                Seq(
                                    fpSrc1ExpIsOnes,
                                    fpSrc1ExpIsZero,
                                    !fpSrc1ExpIsOnes && !fpSrc1ExpIsZero
                                ),
                                Seq(
                                    Cat(fpSrc1SigNotZero, 0.U(51.W)),
                                    Cat(subnormalSig(9,0), 0.U(42.W)),
                                    Cat(fpSrc1(9,0), 0.U(42.W))
                                )
                            )
                        )
    val resultUpCvt = WireDefault(0.U(64.W))
    resultUpCvt := Mux1H(
        Seq(
            isH2S,
            isS2D,
            isH2D
        ),
        Seq(
            resultUpCvtH2S,
            resultUpCvtS2D,
            resultUpCvtH2D
        )
    )
    val fflagsUpCvt = WireDefault(0.U(5.W))
    fflagsUpCvt := Cat(fpSrc1IsSNaN, 0.U(4.W))

// ******************************************************************************
// DownCvt f32->f16 f64->f32 f64->f16
// ******************************************************************************
    // **********************************
    // DownCvt Normal Path
    // **********************************
    val downExp = WireDefault(0.S(12.W))
    downExp := Cat(0.U(1.W), fpSrc1Exp).asSInt - Cat(0.U(1.W), expDeltaDownCvt).asSInt

    // **********************************
    // DownCvt f64->f32
    // **********************************
    val normalSigD2S = fpSrc1Sig.head(23)
    val normalRoundBitD2S = fpSrc1Sig.tail(23).head(1).asBool
    val normalStickyBitD2S = fpSrc1Sig.tail(24).orR

    val normalRounderD2S = Module(new RoundingUnit(23))
    normalRounderD2S.io.in := normalSigD2S
    normalRounderD2S.io.roundIn := normalRoundBitD2S
    normalRounderD2S.io.stickyIn := normalStickyBitD2S
    normalRounderD2S.io.signIn := fpSrc1Sign
    normalRounderD2S.io.rm := io.rm

    val normalSigRoundedD2S = normalRounderD2S.io.out
    val normalExpRoundedD2S = Mux(normalRounderD2S.io.cout, downExp + 1.S, downExp)
    val normalOfD2S = Mux(
        normalRounderD2S.io.cout,
        // downExp > maxExp
        downExp > Cat(0.U(4.W), ~0.U(6.W), 0.U(1.W), 1.U(1.W)).asSInt,
        downExp > Cat(0.U(4.W), ~0.U(7.W), 0.U(1.W)).asSInt
    )
    val expUfD2S = Mux(normalRounderD2S.io.cout, downExp < 0.S, downExp < 1.S)
    val normalIxD2S = normalRounderD2S.io.inexact || normalOfD2S
    val rmin = io.rm === ROD || io.rm === RTZ || (io.rm === RDN && !fpSrc1Sign) || (io.rm === RUP && fpSrc1Sign)
    // when overflow, 
    // if rm == rmin, f32_ouput is max normal format(0x7F7FFFFF),
    // else outOput is INF
    val normalOfExpD2S = Mux(
        rmin,
        Cat(~0.U(7.W), 0.U(1.W)),
        ~0.U(8.W)
    )
    val normalOfSigD2S = Mux(
        rmin,
        ~0.U(23.W),
        0.U(23.W)
    )
    // **********************************
    // DownCvt f64->f16
    // **********************************
    val normalSigD2H = fpSrc1Sig.head(10)
    val normalRoundBitD2H = fpSrc1Sig.tail(10).head(1).asBool
    val normalStickyBitD2H = fpSrc1Sig.tail(11).orR

    val normalRounderD2H = Module(new RoundingUnit(10))
    normalRounderD2H.io.in := normalSigD2H
    normalRounderD2H.io.roundIn := normalRoundBitD2H
    normalRounderD2H.io.stickyIn := normalStickyBitD2H
    normalRounderD2H.io.signIn := fpSrc1Sign
    normalRounderD2H.io.rm := io.rm

    val normalSigRoundedD2H = normalRounderD2H.io.out
    val normalExpRoundedD2H = Mux(normalRounderD2H.io.cout, downExp + 1.S, downExp)
    val normalOfD2H = Mux(
        normalRounderD2H.io.cout,
        // downExp > maxExp
        downExp > Cat(0.U(7.W), ~0.U(3.W), 0.U(1.W), 1.U(1.W)).asSInt,
        downExp > Cat(0.U(7.W), ~0.U(4.W), 0.U(1.W)).asSInt
    )
    val expUfD2H = Mux(normalRounderD2H.io.cout, downExp < 0.S, downExp < 1.S)
    val normalIxD2H = normalRounderD2H.io.inexact || normalOfD2H
    // when overflow, 
    // if rm == rmin, f16_ouput is max normal format(0x7BFF),
    // else outOput is INF
    val normalOfExpD2H = Mux(
        rmin,
        Cat(~0.U(4.W), 0.U(1.W)),
        ~0.U(5.W)
    )
    val normalOfSigD2H = Mux(
        rmin,
        ~0.U(10.W),
        0.U(10.W)
    )

    // **********************************
    // DownCvt f32->f16
    // **********************************
    val normalSigS2H = fpSrc1SigPutHigh.head(10)
    val normalRoundBitS2H = fpSrc1SigPutHigh.tail(10).head(1).asBool
    val normalStickyBitS2H = fpSrc1SigPutHigh.tail(11).orR

    val normalRounderS2H = Module(new RoundingUnit(10))
    normalRounderS2H.io.in := normalSigS2H
    normalRounderS2H.io.roundIn := normalRoundBitS2H
    normalRounderS2H.io.stickyIn := normalStickyBitS2H
    normalRounderS2H.io.signIn := fpSrc1Sign
    normalRounderS2H.io.rm := io.rm

    val normalSigRoundedS2H = normalRounderS2H.io.out
    val normalExpRoundedS2H = Mux(normalRounderS2H.io.cout, downExp + 1.S, downExp)
    val normalOfS2H = Mux(
        normalRounderS2H.io.cout,
        // downExp > maxExp
        downExp > Cat(0.U(7.W), ~0.U(3.W), 0.U(1.W), 1.U(1.W)).asSInt,
        downExp > Cat(0.U(7.W), ~0.U(4.W), 0.U(1.W)).asSInt
    )
    val expUfS2H = Mux(normalRounderS2H.io.cout, downExp < 0.S, downExp < 1.S)
    val normalIxS2H = normalRounderS2H.io.inexact || normalOfS2H
    // when overflow, 
    // if rm == rmin, f16_ouput is max normal format(0x7BFF),
    // else outOput is INF
    val normalOfExpS2H = Mux(
        rmin,
        Cat(~0.U(4.W), 0.U(1.W)),
        ~0.U(5.W)
    )
    val normalOfSigS2H = Mux(
        rmin,
        ~0.U(10.W),
        0.U(10.W)
    )

    // **********************************
    // DownCvt SubNormal Path
    // **********************************
    val mayBeSubnormal = downExp < 1.S

    /*
        if down_exp < 1, the input will be a subnormal in output's form,
        so we need to right shift the `sig`.
        shamt = 1 - down_exp
              = 1 - (fp_in.exp - exp_delta)
              = (1 + exp_delta) - fp_in.exp
    */
    // **********************************
    // DownCvt f64->f32
    // **********************************
    val shamt = Cat(expDeltaDownCvt(10,1), 1.U(1.W))- fpSrc1Exp
    val (subnormalSigD2S, shiftStickyD2S) = ShiftRightJam(
        Cat(fpSrc1ExpNotZero, fpSrc1Sig(51,28)),
        shamt
    )
    val subnormalRounderD2S = Module(new RoundingUnit(23))
    val subnormalSitckyBitD2S = shiftStickyD2S | normalStickyBitD2S
    subnormalRounderD2S.io.in := subnormalSigD2S(23,1)
    subnormalRounderD2S.io.roundIn := subnormalSigD2S(0)
    subnormalRounderD2S.io.stickyIn := subnormalSitckyBitD2S
    subnormalRounderD2S.io.signIn := fpSrc1Sign
    subnormalRounderD2S.io.rm := io.rm

    val subnormalSigRoundedD2S = subnormalRounderD2S.io.out
    val subnormalExpRoundedD2S = WireDefault(0.U(8.W))
    subnormalExpRoundedD2S := Mux(subnormalRounderD2S.io.cout, 1.U, 0.U)
    val subnormalIxD2S = subnormalRounderD2S.io.inexact

    // **********************************
    // DownCvt f64->f16
    // **********************************
    val (subnormalSigD2H, shiftStickyD2H) = ShiftRightJam(
        Cat(fpSrc1ExpNotZero, fpSrc1Sig(51,41)),
        shamt
    )
    val subnormalRounderD2H = Module(new RoundingUnit(10))
    val subnormalSitckyBitD2H = shiftStickyD2H | normalStickyBitD2H
    subnormalRounderD2H.io.in := subnormalSigD2H(10,1)
    subnormalRounderD2H.io.roundIn := subnormalSigD2H(0)
    subnormalRounderD2H.io.stickyIn := subnormalSitckyBitD2H
    subnormalRounderD2H.io.signIn := fpSrc1Sign
    subnormalRounderD2H.io.rm := io.rm

    val subnormalSigRoundedD2H = subnormalRounderD2H.io.out
    val subnormalExpRoundedD2H = WireDefault(0.U(8.W))
    subnormalExpRoundedD2H := Mux(subnormalRounderD2H.io.cout, 1.U, 0.U)
    val subnormalIxD2H = subnormalRounderD2H.io.inexact

    // **********************************
    // DownCvt f32->f16
    // **********************************
    val (subnormalSigS2H, shiftStickyS2H) = ShiftRightJam(
        Cat(fpSrc1ExpNotZero, fpSrc1Sig(22,12)),
        shamt
    )
    val subnormalRounderS2H = Module(new RoundingUnit(10))
    val subnormalSitckyBitS2H = shiftStickyS2H | normalStickyBitS2H
    subnormalRounderS2H.io.in := subnormalSigS2H(10,1)
    subnormalRounderS2H.io.roundIn := subnormalSigS2H(0)
    subnormalRounderS2H.io.stickyIn := subnormalSitckyBitS2H
    subnormalRounderS2H.io.signIn := fpSrc1Sign
    subnormalRounderS2H.io.rm := io.rm

    val subnormalSigRoundedS2H = subnormalRounderS2H.io.out
    val subnormalExpRoundedS2H = WireDefault(0.U(5.W))
    subnormalExpRoundedS2H := Mux(subnormalRounderS2H.io.cout, 1.U, 0.U)
    val subnormalIxS2H = subnormalRounderS2H.io.inexact
    
    val commonExp = WireDefault(0.U(8.W))
    commonExp := Mux1H(
        Seq(
            isD2S && !mayBeSubnormal && normalOfD2S,
            isD2S && !mayBeSubnormal && !normalOfD2S,
            isD2S && mayBeSubnormal,
            isS2H && !mayBeSubnormal && normalOfS2H,
            isS2H && !mayBeSubnormal && !normalOfS2H,
            isS2H && mayBeSubnormal,
            isD2H && !mayBeSubnormal && normalOfD2H,
            isD2H && !mayBeSubnormal && !normalOfD2H,
            isD2H && mayBeSubnormal
        ),
        Seq(
            normalOfExpD2S,
            normalExpRoundedD2S(7, 0),
            subnormalExpRoundedD2S,
            Cat(0.U(3.W), normalOfExpS2H),
            Cat(0.U(3.W), normalExpRoundedS2H(4, 0)),
            Cat(0.U(3.W), subnormalExpRoundedS2H),
            Cat(0.U(3.W), normalOfExpD2H),
            Cat(0.U(3.W), normalExpRoundedD2H(4, 0)),
            Cat(0.U(3.W), subnormalExpRoundedD2H)
        )
    )
    val commonSig = WireDefault(0.U(23.W))
    commonSig :=Mux1H(
        Seq(
            isD2S && !mayBeSubnormal && normalOfD2S,
            isD2S && !mayBeSubnormal && !normalOfD2S,
            isD2S && mayBeSubnormal,
            isS2H && !mayBeSubnormal && normalOfS2H,
            isS2H && !mayBeSubnormal && !normalOfS2H,
            isS2H && mayBeSubnormal,
            isD2H && !mayBeSubnormal && normalOfD2H,
            isD2H && !mayBeSubnormal && !normalOfD2H,
            isD2H && mayBeSubnormal
        ),
        Seq(
            normalOfSigD2S,
            normalSigRoundedD2S,
            subnormalSigRoundedD2S,
            Cat(0.U(13.W), normalOfSigS2H),
            Cat(0.U(13.W), normalSigRoundedS2H),
            Cat(0.U(13.W), subnormalSigRoundedS2H),
            Cat(0.U(13.W), normalOfSigD2H),
            Cat(0.U(13.W), normalSigRoundedD2H),
            Cat(0.U(13.W), subnormalSigRoundedD2H)
        )
    )
    // **********************************
    // UpDown result and fflags
    // **********************************
    val resultDownCvtDS2H = WireDefault(0.U(64.W))
    resultDownCvtDS2H := Cat(
                            ~0.U(48.W),
                            !fpSrc1IsNaN && fpSrc1Sign,
                            Mux1H(
                                Seq(fpSrc1ExpIsOnes, !fpSrc1ExpIsOnes),
                                Seq(~0.U(5.W), commonExp(4,0))
                            ),
                            Mux1H(
                                Seq(fpSrc1ExpIsOnes, !fpSrc1ExpIsOnes),
                                Seq(
                                    Cat(fpSrc1SigNotZero, 0.U(9.W)),
                                    commonSig(9,0)
                                )
                            )
                        )
    val resultDownCvtD2S = WireDefault(0.U(64.W))
    resultDownCvtD2S := Cat(
                            ~0.U(32.W),
                            !fpSrc1IsNaN && fpSrc1Sign,
                            Mux1H(
                                Seq(fpSrc1ExpIsOnes, !fpSrc1ExpIsOnes),
                                Seq(~0.U(8.W), commonExp)
                            ),
                            Mux1H(
                                Seq(fpSrc1ExpIsOnes, !fpSrc1ExpIsOnes),
                                Seq(
                                    Cat(fpSrc1SigNotZero, 0.U(22.W)),
                                    commonSig
                                )
                            )
                        )
    val resultDownCvt = WireDefault(0.U(64.W))
    resultDownCvt := Mux1H(
        Seq(
            tagOut === 2.U,
            tagOut === 0.U
        ),
        Seq(
            resultDownCvtDS2H,
            resultDownCvtD2S,
        )
    )

    val fflagsDownCvt = WireDefault(0.U(5.W))
    val iv = fpSrc1IsSNaN
    val dz = false.B
    val of = !fpSrc1ExpIsOnes && ((isD2S && normalOfD2S) || (isS2H && normalOfS2H) || (isD2H && normalOfD2H))
    val uf = (isD2S && expUfD2S && subnormalIxD2S) || (isS2H && expUfS2H && subnormalIxS2H) || (isD2H && expUfD2H && subnormalIxD2H)
    val ix = !fpSrc1ExpIsOnes && ((!mayBeSubnormal && ((isD2S && normalIxD2S) || (isS2H && normalIxS2H) || (isD2H && normalIxD2H))) ||
            (mayBeSubnormal && ((isD2S && subnormalIxD2S) || (isS2H && subnormalIxS2H) || (isD2H && subnormalIxD2H))))
            
    fflagsDownCvt := Cat(iv, dz, of, uf, ix)

// ******************************************************************************
// fp signj
// ******************************************************************************
    val resultFsgnj = WireDefault(0.U(64.W))
    resultFsgnj := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(fpSrc2.head(49), fpSrc1.tail(49)),
            Cat(fpSrc2.head(33), fpSrc1.tail(33)),
            Cat(fpSrc2.head(1), fpSrc1.tail(1))
        )
    )
    val resultFsgnjn = WireDefault(0.U(64.W))
    resultFsgnjn := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(fpSrc2.head(48), ~fpSrc2.tail(48).head(1), fpSrc1.tail(49)),
            Cat(fpSrc2.head(32), ~fpSrc2.tail(32).head(1), fpSrc1.tail(33)),
            Cat(~fpSrc2.head(1), fpSrc1.tail(1))
        )
    )
    val resultFsgnjx = WireDefault(0.U(64.W))
    resultFsgnjx := Mux1H(
        Seq(
            inputIsF16,
            inputIsF32,
            inputIsF64
        ),
        Seq(
            Cat(fpSrc2.head(48), fpSrc2.tail(48).head(1) ^ fpSrc1.tail(48).head(1), fpSrc1.tail(49)),
            Cat(fpSrc2.head(32), fpSrc2.tail(32).head(1) ^ fpSrc1.tail(32).head(1), fpSrc1.tail(33)),
            Cat(fpSrc2.head(1) ^ fpSrc1.head(1), fpSrc1.tail(1))
        )
    )
    val resultSgnj = WireDefault(0.U(64.W))
    resultSgnj := Mux1H(
        Seq(
            isFpSgnj,
            isFpSgnjn,
            isFpSgnjx
        ),
        Seq(
            resultFsgnj,
            resultFsgnjn,
            resultFsgnjx
        )
    )
    val fflagsSgnj = WireDefault(0.U(5.W))
// ******************************************************************************
// fp min/max
// ******************************************************************************
    val isFpSrc1EqFpSrc2 = WireDefault(false.B)
    isFpSrc1EqFpSrc2 := !fpSrc1IsNaN && !fpSrc2IsNaN && ((fpSrc1 === fpSrc2) || (fpSrc1IsZero && fpSrc2IsZero))
    val isFpSrc1LTFpSrc2 = WireDefault(false.B)
    isFpSrc1LTFpSrc2 := !fpSrc1IsNaN && !fpSrc2IsNaN && 
        //same sign 
        ((((fpSrc1Sign ^ (Cat(0.U(1.W), fpSrc1) - Cat(0.U(1.W), fpSrc2)).head(1).asBool) && !(fpSrc1 === fpSrc2)) && (fpSrc1Sign === fpSrc2Sign)) ||
        // different sign
        ((fpSrc1Sign ^ fpSrc2Sign) && (fpSrc1Sign === true.B) && !(fpSrc1IsZero && fpSrc2IsZero)))
    val resultFpMin = WireDefault(0.U(64.W))
    resultFpMin := Mux1H(
        Seq(
            isFpSrc1EqFpSrc2,
            isFpSrc1LTFpSrc2 || (!fpSrc1IsNaN && fpSrc2IsNaN),
            (!isFpSrc1EqFpSrc2 && !isFpSrc1LTFpSrc2 && !fpSrc1IsNaN && !fpSrc2IsNaN) || (fpSrc1IsNaN && !fpSrc2IsNaN),
            fpSrc1IsNaN && fpSrc2IsNaN
        ),
        Seq(
            fpSrc1 | fpSrc2,
            fpSrc1,
            fpSrc2,
            Mux1H(
                Seq(
                    inputIsF16,
                    inputIsF32,
                    inputIsF64
                ),
                Seq(
                    Cat(~0.U(48.W), 0.U(1.W), ~0.U(6.W), 0.U(9.W)),
                    Cat(~0.U(32.W), 0.U(1.W), ~0.U(9.W), 0.U(22.W)),
                    Cat(0.U(1.W), ~0.U(12.W), 0.U(51.W))
                )
            )
        )
    )
    val resultFpMax = WireDefault(0.U(64.W))
    resultFpMax := Mux1H(
        Seq(
            isFpSrc1EqFpSrc2,
            isFpSrc1LTFpSrc2 || (fpSrc1IsNaN && !fpSrc2IsNaN),
            (!isFpSrc1EqFpSrc2 && !isFpSrc1LTFpSrc2 && !fpSrc1IsNaN && !fpSrc2IsNaN) || (!fpSrc1IsNaN && fpSrc2IsNaN),
            fpSrc1IsNaN && fpSrc2IsNaN
        ),
        Seq(
            fpSrc1 & fpSrc2,
            fpSrc2,
            fpSrc1,
            Mux1H(
                Seq(
                    inputIsF16,
                    inputIsF32,
                    inputIsF64
                ),
                Seq(
                    Cat(~0.U(48.W), 0.U(1.W), ~0.U(6.W), 0.U(9.W)),
                    Cat(~0.U(32.W), 0.U(1.W), ~0.U(9.W), 0.U(22.W)),
                    Cat(0.U(1.W), ~0.U(12.W), 0.U(51.W))
                )
            )
        )
    )
    val resultFpMinMax = WireDefault(0.U(64.W))
    resultFpMinMax := Mux1H(
        Seq(
            isFpMin,
            isFpMax
        ),
        Seq(
            resultFpMin,
            resultFpMax
        )
    )
    val fflagsFpMinMax = WireDefault(0.U(5.W))
    fflagsFpMinMax := Cat(fpSrc1IsSNaN || fpSrc2IsSNaN, 0.U(4.W))
// ******************************************************************************
// fp min/max
// ******************************************************************************
    val resultFpCmp = WireDefault(0.U(64.W))
    resultFpCmp := Mux1H(
        Seq(
            isFpEq,
            isFpLt,
            isFpLe
        ),
        Seq(
            isFpSrc1EqFpSrc2,
            isFpSrc1LTFpSrc2,
            isFpSrc1EqFpSrc2 || isFpSrc1LTFpSrc2
        )
    )
    val isSCmpIv = isFpEq && (fpSrc1IsSNaN || fpSrc2IsSNaN)
    val isQCmpIv = (isFpLt || isFpLe ) && (fpSrc1IsNaN || fpSrc2IsNaN)
    val fflagsFpCmp = WireDefault(0.U(5.W))
    fflagsFpCmp := Cat(isQCmpIv || isSCmpIv, 0.U(4.W))
// ******************************************************************************
// Final result and fflags
// ******************************************************************************
    val result = Mux1H(
        Seq(
            isUpCvt,
            isDownCvt,
            isSgnj,
            isFpMinMax,
            isFpCmp
        ),
        Seq(
            resultUpCvt,
            resultDownCvt,
            resultSgnj,
            resultFpMinMax,
            resultFpCmp
        )
    )
    val fflags = Mux1H(
        Seq(
            isUpCvt,
            isDownCvt,
            isSgnj,
            isFpMinMax,
            isFpCmp
        ),
        Seq(
            fflagsUpCvt,
            fflagsDownCvt,
            fflagsSgnj,
            fflagsFpMinMax,
            fflagsFpCmp
        )
    )
    io.data := result
    io.fflags := fflags
}