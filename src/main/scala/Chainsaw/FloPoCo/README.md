### Generic options include:
- name=<string>:                override the the default entity name
- outputFile=<string>:          override the the default output file name (sticky option)
target=<string>:              target FPGA (default kintex7) (sticky option)
Supported targets: Kintex7, StratixV, Virtex6, Zynq7000, VirtexUltrascalePlus
frequency=<float>:            target frequency in MHz (default 400, 0 means: no pipeline) (sticky option)
plainVHDL=<0|1>:              use plain VHDL (default), or not (sticky option)
useHardMult=<0|1>:            use hardware multipliers (sticky option)
tableCompression=<0|1>:       use errorless table compression when possible (default false while experimental)(sticky option)
registerLargeTables=<0|1>:    force registering of large ROMs to force the use of blockRAMs (default false)(sticky option)
useTargetOptimizations=<0|1>: use target specific optimizations (e.g., using primitives) (sticky option)
ilpSolver=<string>:           override ILP solver for operators optimized by ILP, has to match a solver name known by the ScaLP library(sticky option)
ilpTimeout=<int>:             sets the timeout in seconds for the ILP solver for operators optimized by ILP (default=3600)(sticky option)
compression=<heuristicMaxEff,heuristicPA,heuristicFirstFit,optimal,optimalMinStages>:        compression method (default=heuristicMaxEff)(sticky option)
tiling=<heuristicBasicTiling,optimal,heuristicGreedyTiling,heuristicXGreedyTiling,heuristicBeamSearchTiling>:        tiling method (default=heuristicBeamSearchTiling)(sticky option)
hardMultThreshold=<float>: unused hard mult threshold (O..1, default 0.7) (sticky option)
verbose=<int>:        verbosity level (0-4, default=1)(sticky option)
generateFigures=<0|1>:generate graphics in SVG or LaTeX for some operators (default off) (sticky option)
dependencyGraph=<no|compact|full>: generate data dependence drawing of the Operator (default no)
Sticky options apply to the rest of the command line, unless changed again

List of operators with command-line interface (a few more are hidden inside FloPoCo)
========Shifters, Leading Zero Counters, etc========
Shifter: A flexible shifter.
wX (int): input size in bits  
maxShift (int): maximum shift distance in bits  
dir (bool): 0=left, 1=right  
wR (int): size of the shifted output , -1 means computed, will be equal to wX+maxShift    (optional, default value is -1)
computeSticky (bool): if true and wR<wX+maxShift, shifted-out bits are ORed into a sticky bit    (optional, default value is false)
inputPadBit (bool): if true, add an input bit used for left-padding, as in sign extension    (optional, default value is false)
LZOC: A leading zero or one counter. The output size is computed.
wIn (int): input size in bits  
countType (int): 0 means count zeroes, 1 means count ones, -1 means add an input that defines what to count    (optional, default value is -1)
LZOC3: A leading zero counter. The output size is computed.
wIn (int): input size in bits  
useLargeLut (bool): Use max unrouted lut size to build the encoding    (optional, default value is false)
Normalizer: A combined leading zero/one counter and left shifter, useful for floating-point normalization.
wX (int): input size in bits  
wR (int): output size in bits, with wR <= wX  
maxShift (int): how many bits to count, with maxShift<= wX   
computeSticky (bool): if true and wR<wX, a sticky bit is computed out of the discarded bits    (optional, default value is false)
countType (int): 0 to count zeroes, 1 to count ones, -1 to have a dynamic OZb input that tells what to count    (optional, default value is -1)
ShiftReg: A plain shift register implementation.
w (int): the size of the input  
n (int): the number of stages in the shift register, also the number of outputs  
reset (int): the reset type (0 for none, 1 for synchronous, 2 for asynchronous)    (optional, default value is 0)
GenericMux: A Multiplexer
wIn (int): input word size  
inputCount (int): the number of data inputs (NOT counting the select input!)  
GenericLut: A simple look up table.
wIn (int): input word size  
wOut (int): output word size  
entityName (string): unique name for the LUT  
inputValues (string): colon seperated list of (unsigned) ints specifying the inputs for the LUT  
outputValues (string): colon seperated list of (unsigned) ints specifying the corrisponding outputs  
========Basic integer operators (pipelined)========
Compressor: A basic compressor.
columnHeights (string): comma separated list of heights for the columns of the compressor, in decreasing order of the weight. For example, columnHeights="2,3" produces a (2,3:4) GPC  
compactView (bool): whether the VHDL code is printed in a more compact way, or not    (optional, default value is false)
IntAdder: Integer adder. In modern VHDL, integer addition is expressed by a + and one usually needn't define an entity for it. However, this operator will be pipelined if the addition is too large to be performed at the target frequency.
wIn (int): input size in bits  
arch (int): -1 for automatic, 0 for classical, 1 for alternative, 2 for short latency    (optional, default value is -1)
optObjective (int): 0 to optimize for logic, 1 to optimize for register, 2 to optimize for slice/ALM count    (optional, default value is 2)
SRL (bool): optimize for shift registers    (optional, default value is true)
IntDualAddSub: Pipelined dual adder/subtractor
wIn (int): input size in bits  
opType (int): 1=compute X-Y and X+Y, 2=compute X-Y and Y-X  
IntMultiAdder: A component adding n integers, bitheap based. If wIn=1 it is also a population count
signedIn (bool): 0=unsigned, 1=signed  
n (int): number of inputs to add  
wIn (int): input size in bits  
wOut (int): output size in bits -- if 0, wOut is computed to be large enough to represent the result    (optional, default value is 0)
DSPBlock: Implements a DSP block commonly found in FPGAs incl. pre-adders and post-adders computing R = (X1+X2) * Y + Z
wX (int): size of input X (or X1 and X2 if pre-adders are used)  
wY (int): size of input Y  
wZ (int): size of input Z (if post-adder is used)    (optional, default value is 0)
xIsSigned (bool): input X is signed    (optional, default value is 0)
yIsSigned (bool): input Y is signed    (optional, default value is 0)
isPipelined (bool): every stage is pipelined when set to 1    (optional, default value is 1)
usePostAdder (bool): use post-adders    (optional, default value is 0)
usePreAdder (bool): use pre-adders    (optional, default value is 0)
preAdderSubtracts (bool): if true, the pre-adder performs a pre-subtraction    (optional, default value is 0)
IntMultiplier: A pipelined integer multiplier.
wX (int): size of input X  
wY (int): size of input Y  
wOut (int): size of the output if you want a truncated multiplier. 0 for full multiplier    (optional, default value is 0)
signedIO (bool): inputs and outputs can be signed or unsigned    (optional, default value is false)
maxDSP (int): limits the number of DSP-Tiles used in Multiplier    (optional, default value is -1)
use2xk (bool): if true, attempts to use the 2xk-LUT-Multiplier with relatively high efficiency    (optional, default value is false)
useirregular (bool): if true, attempts to use the irregular-LUT-Multipliers with higher area/lut efficiency than the rectangular versions    (optional, default value is false)
useLUT (bool): if true, attempts to use the LUT-Multipliers for tiling    (optional, default value is true)
useDSP (bool): if true, attempts to use the DSP-Multipliers for tiling    (optional, default value is true)
useKaratsuba (bool): if true, attempts to use rectangular Karatsuba for tiling    (optional, default value is false)
superTile (bool): if true, attempts to use the DSP adders to chain sub-multipliers. This may entail lower logic consumption, but higher latency.    (optional, default value is false)
dspThreshold (real): threshold of relative occupation ratio of a DSP multiplier to be used or not    (optional, default value is 0.0)
beamRange (int): range for beam search    (optional, default value is 3)
IntMultiplierLUT: Implements a LUT multiplier by simply tabulating all results in the LUT, should only be used for very small word sizes
wX (int): size of input X  
wY (int): size of input Y  
IntKaratsubaRectangular: Implements a large unsigned Multiplier using rectangular shaped tiles as appears for Xilinx FPGAs. Currently limited to specific, hand-optimized sizes
wX (int): size of input X  
wY (int): size of input Y  
useKaratsuba (bool): Uses Karatsuba when set to 1, instead a standard tiling without sharing is used.    (optional, default value is 1)
useRectangularTiles (bool): Uses rectangular tiles when set to 1, otherwise quadratic tiles are used    (optional, default value is 1)
IntSquarer: An integer squarer.
wIn (int): size of input in bits  
wOut (int): size of the output if you want a truncated squarer. 0 for exact (full) squarer    (optional, default value is 0)
signedIn (bool): inputs can be signed or unsigned (output always unsigned)    (optional, default value is false)
BaseMultiplierDSPSuperTilesXilinx: Implements a DSP block commonly found in FPGAs incl. pre-adders and post-adders computing R = (X1+X2) * Y + Z
shape (int): Shape ID (1-12) of the DSP-Superblock  
isPipelined (bool): use pipelining    (optional, default value is 0)
xIsSigned (bool): input X is signed    (optional, default value is 0)
yIsSigned (bool): input Y is signed    (optional, default value is 0)
BaseMultiplierXilinx2xk: Implements a 2xY-LUT-Multiplier that can be realized efficiently on some Xilinx-FPGAs
wX (int): size of input X  
wY (int): size of input Y  
xIsSigned (bool): input X is signed    (optional, default value is 0)
yIsSigned (bool): input Y is signed    (optional, default value is 0)
BaseMultiplierIrregularLUTXilinx: Implements a non rectangular LUT multiplier from a set that yields a relatively high efficiency compared to recangular LUT multipliers
  _ _     _        _ _       _     _ _ _    _ _      _ _     _
|_|_|_  |_|_     |_|_|_    |_|_  |_|_|_|  |_|_|_   |_|_|   |_|_
|_|_|_| |_|_|_     |_|_|   |_|_|   |_|_|  |_|_|_|  |_|_|   |_|_|
|_|_|_|   |_|_|              |_|                     |_|   |_|_|
shape 1  shape 2 shape 3 shape 4 shape 5  shape 6 shape 7  shape 8

wS (int): shape ID  
BaseMultiplierDSPKaratsuba: Implements the Karatsuba pattern with DSPs where certain multipliers can be omitted to save DSPs
wX (int): size of input X of a single DSP-block  
wY (int): size of input Y of a single DSP-block  
n (int): size of pattern and number of DSP substitutions  
========Basic floating-point operators========
FPAdd: A correctly rounded floating-point adder.
wE (int): exponent size in bits  
wF (int): mantissa size in bits  
sub (bool): implement a floating-point subtractor instead of an adder    (optional, default value is false)
dualPath (bool): use a dual-path algorithm, more expensive but shorter latency    (optional, default value is false)
onlyPositiveIO (bool): optimize for only positive input and output numbers    (optional, default value is false)
FPDiv: A correctly rounded floating-point division.
wE (int): exponent size in bits  
wF (int): mantissa size in bits  
srt (int): Can be 42, 43 or 87 so far. Default 42 means radix 4 with digits between -2 and 2. Other choices may have a better area/speed trade-offs    (optional, default value is 42)
FPSqrt: A correctly rounded floating-point square root function.
wE (int): exponent size in bits  
wF (int): mantissa size in bits  
FPMult: A floating-point multiplier. The actual FloPoCo component supports different input and output sizes, but this is not available from the command line.
wE (int): exponent size in bits  
wF (int): input's mantissa size in bits  
wFout (int): output's mantissa size in bits (if 0 or ommitted, will be equal to wFIn)    (optional, default value is 0)
correctlyRounded (bool): Use correct rounding, if false use faithful rounding    (optional, default value is true)
dspThreshold (real): threshold of relative occupation ratio of a DSP multiplier to be used or not    (optional, default value is 0.0)
IEEEAdd: A floating-point adder with a new, more compact single-path architecture.
wE (int): exponent size in bits  
wF (int): mantissa size in bits  
IEEEFMA: A correctly rounded floating-point FMA.
wE (int): exponent size in bits  
wF (int): mantissa size in bits  
========Posit operators========
PositAdd: A correctly rounded posit adder.
width (int): posit size in bits  
wES (int): exponent size in bits  
PositExp: A faithful posit exponential function.
width (int): Posit size in bits  
wES (int): exponent size in bits  
d (int): degree of the polynomial. 0 choses a sensible default.    (optional, default value is 0)
k (int): input size to the range reduction table, should be between 5 and 15. 0 choses a sensible default.    (optional, default value is 0)
g (int): number of guard bits    (optional, default value is -1)
PositFunctionByTable: Evaluator of function f using a table.
f (string): function to be evaluated between double-quotes, for instance "exp(x*x)"  
width (int): size of the Posit  
wES (int): size of the Posit's exponent.  
========Multipliers and dividers by constants========
IntConstMult: Integer multiplier of an unsigned number by a constant using a shift-and-add tree.
wIn (int): input size in bits  
n (int): constant to multiply by  
FPConstMult: Floating-point constant multiplier using the shift-and-add approach.
wE_in (int): input exponent width  
wF_in (int): input significand part width  
wE_out (int): output exponent width  
wF_out (int): output significand width  
constant (string): constant in sollya formalism (e.g. "cos(3*pi/2)" or "13176795b-22")  
cst_width (int): constant precision. If set to zero, the actual width will be computed in order to get a faithful result.    (optional, default value is 0)
FPConstMultRational: Correctly rounded floating-point multiplier by a rational constant.
wE_in (int): input exponent width  
wF_in (int): input significand part width  
wE_out (int): output exponent width  
wF_out (int): output significand width  
a (int): numerator  
b (int): denominator  
IntConstDiv: Integer divider by a small constant.
wIn (int): input size in bits  
d (intlist): integer to divide by. Either a small integer, or a colon-separated list of small integers, in which case a composite divider by the product is built  
arch (int): architecture used -- 0 for linear-time, 1 for log-time, 2 for multiply-and-add by the reciprocal    (optional, default value is 0)
computeQuotient (bool): if true, the architecture outputs the quotient    (optional, default value is true)
computeRemainder (bool): if true, the architecture outputs the remainder    (optional, default value is true)
alpha (int): Algorithm uses radix 2^alpha. -1 choses a sensible default.    (optional, default value is -1)
FixRealKCM: Table based real multiplier. Output size is computed
signedIn (bool): 0=unsigned, 1=signed  
msbIn (int): weight associated to most significant bit (including sign bit)  
lsbIn (int): weight associated to least significant bit  
lsbOut (int): weight associated to output least significant bit  
constant (string): constant given in arbitrary-precision decimal, or as a Sollya expression, e.g "log(2)"  
targetUlpError (real): required precision on last bit. Should be strictly greater than 0.5 and lesser than 1    (optional, default value is 1.0)
FixRealShiftAdd: Table based real multiplier. Output size is computed
signedIn (bool): 0=unsigned, 1=signed  
msbIn (int): weight associated to most significant bit (including sign bit)  
lsbIn (int): weight associated to least significant bit  
lsbOut (int): weight associated to output least significant bit  
constant (string): constant given in arbitrary-precision decimal, or as a Sollya expression, e.g "log(2)"  
targetUlpError (real): required precision on last bit. Should be strictly greater than 0.5 and lesser than 1    (optional, default value is 1.0)
FixRealConstMult: Table based real multiplier. Output size is computed
signedIn (bool): 0=unsigned, 1=signed  
msbIn (int): weight associated to most significant bit (including sign bit)  
lsbIn (int): weight associated to least significant bit  
lsbOut (int): weight associated to output least significant bit  
constant (string): constant given in arbitrary-precision decimal, or as a Sollya expression, e.g "log(2)"  
targetUlpError (real): required precision on last bit. Should be strictly greater than 0.5 and lesser than 1    (optional, default value is 1.0)
method (string): desired method. Can be 'KCM', 'ShiftAdd' or 'auto' (let FloPoCo decide which operator performs best)    (optional, default value is auto)
FixFixConstMult: Table based real multiplier. Output size is computed
signedIn (bool): 0=unsigned, 1=signed  
msbIn (int): weight associated to most significant bit (including sign bit)  
lsbIn (int): weight associated to least significant bit  
lsbOut (int): weight associated to output least significant bit  
constant (string): constant given in arbitrary-precision decimal, or as a Sollya expression, e.g "log(2)"  
targetUlpError (real): required precision on last bit. Should be strictly greater than 0.5 and lesser than 1    (optional, default value is 1.0)
FixComplexKCM: Table-based complex multiplier. Inputs are two's complement. Output size is computed
msbIn (int): weight associated to most significant bit (including sign bit)  
lsbIn (int): weight associated to least significant bit  
lsbOut (int): weight associated to output least significant bit  
constantRe (string): real part of the constant, given as a Sollya expression, e.g "log(2)"  
constantIm (string): imaginary part of the constant, given as a Sollya expression, e.g "log(2)"  
extrabit (bool): do we need extra bit for addition    (optional, default value is true)
IntConstMultShiftAdd: A component for building constant multipliers based on pipelined adder graphs (PAGs).
wIn (int): Wordsize of pag inputs  
graph (string): Realization string of the pag  
epsilon (int): Allowable error for truncated constant multipliers    (optional, default value is 0)
pipeline (bool): Enable pipelining of the pag    (optional, default value is true)
sync_inout (bool): Enable pipeline registers for input and output stage    (optional, default value is true)
sync_muxes (bool): Enable counting mux-only stages as full stage    (optional, default value is true)
sync_every (int): Count of stages after which will be pipelined    (optional, default value is 1)
truncations (string): provides the truncations for subvalues    (optional, default value is "")
IntConstMultShiftAddOpt: Integer constant multiplication using shift and add in an optimal way (i.e., with minimum number of adders). Works for coefficients up to 524287 (19 bit)
wIn (int): Input word size  
constant (int): constant  
epsilon (int): Allowable error for truncated constant multipliers    (optional, default value is 0)
IntConstMultShiftAddRPAG: Integer constant multiplication using shift and add using the RPAG algorithm
wIn (int): Input word size  
constant (int): constant  
epsilon (int): Allowable error for truncated constant multipliers    (optional, default value is 0)
IntConstMultShiftAddOptTernary: Integer constant multiplication using shift and ternary additions in an optimal way (i.e., with minimum number of ternary adders). Works for coefficients up to 4194303 (22 bit)
wIn (int): Input word size  
constant (int): constant  
========Elementary functions in fixed- or floating-Point========
FPExp: A faithful floating-point exponential function.
wE (int): exponent size in bits  
wF (int): mantissa size in bits  
d (int): degree of the polynomial. 0 choses a sensible default.    (optional, default value is 0)
k (int): input size to the range reduction table, should be between 5 and 15. 0 choses a sensible default.    (optional, default value is 0)
g (int): number of guard bits    (optional, default value is -1)
FPLog: Floating-point logarithm
wE (int): exponent size in bits  
wF (int): mantissa size in bits  
method (int): 0 for iterative, 1 for polynomial    (optional, default value is 0)
inTableSize (int): The input size to the tables of the iterative method, in bits, between 6 and 16. 0 choses a a sensible value    (optional, default value is 0)
FixSinCos: Computes (1-2^(-w)) sin(pi*x) and (1-2^(-w)) cos(pi*x) for x in -[1,1[, using tables and multipliers.
lsb (int): weight of the LSB of the input and outputs  
method (int): 0 for table- and mult-based, 1 for traditional CORDIC, 2 for reduced-iteration CORDIC    (optional, default value is 0)
FixAtan2: Computes atan(X/Y) as A=(angle in radian)/pi,  so A in [-1,1).
lsb (int): weight of the LSB of both inputs and outputs  
method (int): parameter select between: InvMultAtan with approximations of the corresponding degree (0..7), plain CORDIC (8), CORDIC with scaling (9), a method using surface approximation (10), Taylor approximation of order 1 (11) and 2 (12)  
========Arbitrary function approximators========
FixFunctionByTable: Evaluator of function f on [0,1) or [-1,1), depending on signedIn, using a table.
f (string): function to be evaluated between double-quotes, for instance "exp(x*x)"  
signedIn (bool): if true the function input range is [-1,1), if false it is [0,1)  
lsbIn (int): weight of input LSB, for instance -8 for an 8-bit input  
lsbOut (int): weight of output LSB  
FixFunctionByMultipartiteTable: A function evaluator using the multipartite method.
f (string): function to be evaluated between double-quotes, for instance "exp(x*x)"  
signedIn (bool): if true the function input range is [-1,1), if false it is [0,1)  
lsbIn (int): weight of input LSB, for instance -8 for an 8-bit input  
lsbOut (int): weight of output LSB  
nbTO (int): number of Tables of Offsets, between 1 (bipartite) to 4 or 5 for large input sizes -- 0: let the tool choose     (optional, default value is 0)
compressTIV (bool): use Hsiao TIV compression, or not    (optional, default value is true)
FixFunctionBySimplePoly: Evaluator of function f on [0,1) or [-1,1), using a single polynomial with Horner scheme
f (string): function to be evaluated between double-quotes, for instance "exp(x*x)"  
signedIn (bool): if true the function input range is [-1,1), if false it is [0,1)    (optional, default value is true)
lsbIn (int): weight of input LSB, for instance -8 for an 8-bit input  
lsbOut (int): weight of output LSB  
FixFunctionByPiecewisePoly: Evaluator of function f on [0,1), using a piecewise polynomial of degree d with Horner scheme.
f (string): function to be evaluated between double-quotes, for instance "exp(x*x)"  
lsbIn (int): weight of input LSB, for instance -8 for an 8-bit input  
lsbOut (int): weight of output LSB  
d (int): degree of the polynomial  
approxErrorBudget (real): error budget in ulp for the approximation, between 0 and 0.5    (optional, default value is 0.25)
UniformPiecewisePolyApprox: Helper/Debug feature, does not generate VHDL. Uniformly segmented piecewise polynomial approximation of function f, accurate to targetAcc on [0,1)
f (string): function to be evaluated between double-quotes, for instance "exp(x*x)"  
targetAcc (real): the target approximation errror of the polynomial WRT the function  
d (int): the degree to use  
========Conversions between number systems========
Fix2FP: Conversion from FloPoCo floating-point to fixed-point.
signed (bool): can be false if all numbers will be positive    (optional, default value is true)
MSB (int): weight of the MSB of the output  
LSB (int): weight of LSB of the input  
wE (int): output exponent size in bits  
wF (int): output mantissa size in bits  
FP2Fix: Conversion from FloPoCo floating-point to fixed-point.
wE (int): input exponent size in bits  
wF (int): input mantissa size in bits  
signed (bool): can be false if all numbers will be positive    (optional, default value is true)
MSB (int): weight of the MSB of the output  
LSB (int): weight of LSB of the output  
trunc (bool): true means truncated (cheaper), false means rounded    (optional, default value is true)
OutputIEEE: Conversion from FloPoCo to IEEE-754-like floating-point formats.
wEIn (int): input exponent size in bits  
wFIn (int): input mantissa size in bits  
wEOut (int): output exponent size in bits  
wFOut (int): output mantissa size in bits  
onlyPositiveZeroes (bool): when true, normalize +0 and -0 to +0    (optional, default value is false)
Posit2FP: Convert Posit to floating point
width (int): total size of the encoding  
es (int): exponent field length  
PIF2Posit: Converts Posit Intermediate Format to Posits
width (int): The size of the posit  
wES (int): The exponent size (for the posit)  
Posit2PIF: Converts Posits to Posit Intermediate Format
width (int): The input size  
wES (int): The exponent size  
PIF2Fix: Converts Posit Intermediate Format to the FixPoint format used in the exponential
width (int): The size of the posit  
wES (int): The exponent size (for the posit)  
Posit2Posit: This should do nothing
width (int): The size of the posit  
wES (int): The width of the exponent  
========Filters and FFTs========
FixSOPC: A fix-point Sum of Product by Constants.
lsbIn (int): input's last significant bit  
lsbOut (int): output's last significant bit  
coeff (string): colon-separated list of real coefficients using Sollya syntax. Example: coeff="1.234567890123:sin(3*pi/8)"  
FixSOPCfull: A fix-point Sum of Product by Constants (detailed interface).
msbIn (string): colon-separated string of ints, input's last significant bit  
lsbIn (string): colon-separated string of ints, input's last significant bit  
msbOut (int): output's most significant bit  
lsbOut (int): output's last significant bit  
coeff (string): colon-separated list of real coefficients using Sollya syntax. Example: coeff="1.234567890123:sin(3*pi/8)"  
FixFIR: A fix-point Finite Impulse Filter generator.
lsbIn (int): integer size in bits  
lsbOut (int): integer size in bits  
symmetry (int): 0 for normal filter, 1 for symmetric, -1 for antisymmetric. If not 0, only the first half of the coeff list is used.    (optional, default value is 0)
rescale (bool): If true, divides all coefficients by 1/sum(|coeff|)    (optional, default value is false)
coeff (string): colon-separated list of real coefficients using Sollya syntax. Example: coeff="1.234567890123:sin(3*pi/8)"  
FixHalfSine: A generator of fixed-point Half-Sine filters, for inputs between -1 and 1
lsbIn (int): position of the LSB of the input, e.g. -15 for a 16-bit signed input  
lsbOut (int): position of the LSB of the output  
n (int): filter order (number of taps will be 2n)  
FixRootRaisedCosine: A generator of fixed-point Root-Raised Cosine filters, for inputs between -1 and 1
alpha (real): roll-off factor, between 0 and 1  
lsbIn (int): position of the LSB of the input, e.g. -15 for a 16-bit signed input  
lsbOut (int): position of the LSB of the output  
n (int): filter order (number of taps will be 2n+1)  
FixIIR: A fix-point Infinite Impulse Response filter generator.
lsbIn (int): input least significant bit  
lsbOut (int): output least significant bit  
H (real): worst-case peak gain. if 0, it will be computed by the WCPG library    (optional, default value is 0)
Heps (real): worst-case peak gain of the feedback loop. if 0, it will be computed by the WCPG library    (optional, default value is 0)
coeffa (string): colon-separated list of real coefficients using Sollya syntax. Example: coeffa="1.234567890123:sin(3*pi/8)"  
coeffb (string): colon-separated list of real coefficients using Sollya syntax. Example: coeffb="1.234567890123:sin(3*pi/8)"  
buildWorstCaseTestBench (bool): if true, the TestBench for this IIR will begin with a stimulation by the worst-case input signal    (optional, default value is false)
FixIIRShiftAdd: An Infinite Impulse Response filter generator using IntConstMultShiftAdd (optional).
msbIn (int): input most significant bit  
lsbIn (int): input least significant bit  
lsbOut (int): output least significant bit  
guardbits (int): number of guard bits for computation in recursive feedback path (-1: automatic)    (optional, default value is -1)
coeffa (string): colon-separated list of real coefficients using Sollya syntax. Example: coeffa="1.234567890123:sin(3*pi/8)"    (optional, default value is -1)
coeffb (string): colon-separated list of real coefficients using Sollya syntax. Example: coeffb="1.234567890123:sin(3*pi/8)"  
shifta (int): Num of rightshifts for coeffa (must be positive)    (optional, default value is -1)
shiftb (int): Num of rightshifts for coeffb (must be positive)  
method (string): plain or multiplierless    (optional, default value is plain)
grapha (string): graph in rpag format for coeffa    (optional, default value is emptya)
graphb (string): graph in rpag format for coeffb    (optional, default value is emptyb)
========Test Benches========
TestBench: Behavorial test bench for the preceding operator.
n (int): number of random tests. If n=-2, an exhaustive test is generated (use only for small operators)    (optional, default value is -2)
file (bool): Inputs and outputs are stored in file test.input (lower VHDL compilation time). If false, they are stored in the VHDL    (optional, default value is true)
Wrapper: Wraps the preceding operator between registers (for frequency testing).
========AutoTest========
AutoTest: A tester for operators.
Operator (string): name of the operator to test, All if we need to test all the operators  
Dependences (bool): test the operator's dependences    (optional, default value is false)
========Highly target optimized primitive operators========
XilinxAddSub: An adder/subtractor build of xilinx primitives.
wIn (int): The wordsize of the adder  
mode (int): Bitmask for input negation, removes configurability    (optional, default value is 0)
dss (bool): Creates configurable adder with possibility to substract both inputs at same time    (optional, default value is false)
XilinxComparator: A comparator build of xilinx primitives.
wIn  (int): Wordsize of comparator inputs  
type  (string): Type of comparator ( gt,ge,lt,le,eq,ne )  
XilinxTernaryAddSub: A ternary adder subtractor build of xilinx primitives.
wIn (int): The wordsize of the adder  
AddSubBitMask (int): First bitmask for input negation    (optional, default value is 0)
AddSubBitMask2 (int): Second bitmask for configurable input negation    (optional, default value is -1)
XilinxGPC: Implements Xilinx optimized GPCs                        Available GPC sizes are:                        (6,0,6;5), (6,0,7;5), (6,1,5;5), (6,2,3;5)                        (1,3,2,5;5), (1,4,1,5;5), (1,4,0,6;5), (1,4,0,7;5), (2,1,1,7;5)
columnHeights (string): comma separated list of heights for the columns of the compressor, in decreasing order of the weight. For example, columnHeights="6,0,6" produces a (6,0,6:5) GPC  
XilinxFourToTwoCompressor: An efficient 4:2 compressor build of xilinx primitives.
wOut (int): The wordsize of the 4:2 compressor  
useLastColumn (bool): if the 4:2 compressor should additonally compress two bits in the last column, this should be set to true    (optional, default value is 0)
========Miscellaneous========
TutorialOperator: An heavily commented example operator to start with FloPoCo.
param0 (int): A first parameter, here used as the input size    (optional, default value is 16)
param1 (int): A second parameter, here used as the output size  
ltr@311-FPGA:~/FloPoCo/flopoco/build$ 