function QAMSymbols = Bits2QAM(bits)
    convCodedBits = Convenc(bits); % 卷积编码
    save './data/convCodedBits' convCodedBits;
    interleavedBits = Interleave(convCodedBits); % 交织
    save './data/interleavedBits' interleavedBits;
    QAMSymbols = DynamicQammod(interleavedBits); % (动态)QAM映射
    save './data/QAMSymbols' QAMSymbols;
