function QAMSymbols = Bits2QAM(bits)
    convCodedBits = Convenc(bits); % 卷积编码
    interleavedBits = Interleave(convCodedBits); % 交织
    QAMSymbols = DynamicQammod(interleavedBits); % (动态)QAM映射
