function QAMSymbols = Bits2QAM(bits)
    save './data/bits' bits;
    convCodedBits = Convenc(bits); % 卷积编码
    save './data/convCodedBits' convCodedBits;
    interleavedBits = Interleaver(convCodedBits, 1); % 交织
    save './data/interleavedBits' interleavedBits;
    QAMSymbols = DynamicQammod(interleavedBits); % (动态)QAM映射
    save './data/QAMSymbols' QAMSymbols;
